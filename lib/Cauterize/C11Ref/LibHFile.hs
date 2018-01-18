{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibHFile
  ( hFileFromSpec
  ) where

import Cauterize.C11Ref.Util

import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)
import Numeric
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S
import qualified Data.Map as M

hFileFromSpec :: S.Specification -> String
hFileFromSpec = unindent . concat . fromSpec

fromSpec :: S.Specification -> [String]
fromSpec s = [chompNewline [i|
  #ifndef #{guardSym}
  #define #{guardSym}

  #include "cauterize.h"
|]
  , comment "library meta information"
  , chompNewline [i|
  #define NAME_#{ln} "#{ln}"
  #define VERSION_#{ln} "#{unpack $ S.specVersion s}"
  #define MIN_SIZE_#{ln} (#{C.sizeMin libsize})
  #define MAX_SIZE_#{ln} (#{C.sizeMax libsize})
  #define NUM_TYPES_#{ln} (#{length types})
|]
  , comment "schema hash"
  , [i|  extern hashtype_t const SCHEMA_HASH_#{ln};|]
  , blankLine

  , comment "type indicies"
  , chompNewline [i|
  enum type_index_#{ln} {
#{typeIndicies}
  };
|]
  , comment "type definitions"
  , unlines (mapMaybe (typeDefinition luDecl) types)

  , comment "function prototypes"
  , unlines (map typeFuncPrototypes types)

  , blankLine
  , chompNewline [i|
  #endif /* #{guardSym} */
|]
  ]
  where
    guardSym = [i|_CAUTERIZE_C11REF_#{ln}_|]
    blankLine = "\n"
    libsize = S.specSize s
    ln = unpack (S.specName s)
    types = S.specTypes s
    typeIndicies =
      let withIndex = zip [(0 :: Integer)..] types
      in intercalate "\n" $ map (\(ix,t) -> [i|    type_index_#{ln}_#{ident2str $ S.typeName t} = #{ix},|]) withIndex

    -- Names to how you delcare that name
    n2declMap = let s' = S.specTypes s
                    d = map t2decl s'
                    n = fmap S.typeName s'
                in primDeclMap `M.union` M.fromList (zip n d)
    luDecl n = fromMaybe (error $ "Invalid name: " ++ unpack (C.unIdentifier n) ++ ".")
                         (M.lookup n n2declMap)

typeFuncPrototypes :: S.Type -> String
typeFuncPrototypes t = chompNewline [i|
  enum caut_status encode_#{n}(struct caut_encode_iter * const _c_iter, #{d} const * const _c_obj);
  enum caut_status decode_#{n}(struct caut_decode_iter * const _c_iter, #{d} * const _c_obj);
  void init_#{n}(#{d} * _c_obj);
  enum caut_ord compare_#{n}(#{d} const * const _c_a, #{d} const * const _c_b);
|]
  where
    d = t2decl t
    n = ident2str $ S.typeName t

typeDefinition :: (C.Identifier -> String) -> S.Type -> Maybe String
typeDefinition refDecl (S.Type { S.typeName = n, S.typeDesc = d } ) =
  case d of
    S.Synonym { S.synonymRef = r } -> Just [i|typedef #{refDecl r} #{n'}; /* synonym */|]
    S.Range { S.rangePrim = p } -> Just [i|typedef #{prim2c p} #{n'}; /* range */|]
    S.Array {  S.arrayRef = r, S.arrayLength = l } -> Just $ defArray n' (refDecl r) (fromIntegral l)
    S.Vector { S.vectorRef = r, S.vectorLength = l, S.vectorTag = t } -> Just $ defVector n' (refDecl r) (fromIntegral l) t
    S.Enumeration { S.enumerationValues = vs } -> Just $ defEnum n' vs
    S.Record { S.recordFields = fs } -> Just $ defRecord n' refDecl fs
    S.Combination { S.combinationFields = fs, S.combinationTag = t } -> Just $ defCombination n' refDecl fs t
    S.Union { S.unionFields = fs } -> Just $ defUnion n' refDecl fs
  where
    n' = ident2str n

defArray :: String -> String -> Integer -> String
defArray n refDecl len =
  let lenSym = [i|ARRAY_LEN_#{n}|]
  in chompNewline [i|
  #define #{lenSym} (#{len})
  struct #{n} {
    #{refDecl} elems[#{lenSym}];
  };
|]

defVector :: String -> String -> Integer -> C.Tag -> String
defVector n refDecl len lenRep =
  let maxLenSym = [i|VECTOR_MAX_LEN_#{n}|]
      lenRepDecl = tag2c lenRep
  in chompNewline [i|
  #define #{maxLenSym} (#{len})
  struct #{n} {
    #{lenRepDecl} _length;
    #{refDecl} elems[#{maxLenSym}];
  };
|]

defEnum :: String -> [S.EnumVal] -> String
defEnum n [] = error $ "defEnum: enumeration '" ++ n ++ "' must have at least one value."
defEnum n vs =
  let minValSym = [i|ENUM_MIN_VAL_#{n}|]
      lminsym = [i|#{n}_#{ident2str $ S.enumValName (head vs)}|]
      maxValSym = [i|ENUM_MAX_VAL_#{n}|]
      lmaxsym = [i|#{n}_#{ident2str $ S.enumValName (last vs)}|]
  in chompNewline [i|
  #define #{minValSym} (#{lminsym})
  #define #{maxValSym} (#{lmaxsym})
  enum #{n} {
    #{vdefs}
  };
|]
  where
    vdefs = intercalate "\n    " $ map defVal vs
    defVal (S.EnumVal vn ix) = let vn' = unpack . C.unIdentifier $ vn
                              in [i|#{n}_#{vn'} = #{show ix},|]

defRecord :: String -> (C.Identifier -> String) -> [S.Field] -> String
defRecord n refDecl fields = chompNewline [i|
  struct #{n} {
    #{fdefs}
  };
|]
  where
    defField S.DataField { S.fieldName = fn, S.fieldRef = fr} = [i|#{refDecl fr} #{ident2str fn};|]
    defField f@S.EmptyField {} = emptyFieldComment f
    fdefs = intercalate "\n    " $ map defField fields

defCombination :: String -> (C.Identifier -> String) -> [S.Field] -> C.Tag -> String
defCombination n refDecl fields flagsRepr = chompNewline [i|
  #define COMBINATION_FLAGS_#{n} (0x#{flagsMask}ull)
  struct #{n} {
    #{tag2c flagsRepr} _flags;
    #{fdefs}
  };
|]
  where
    flagsMask = case length fields of
                  0 -> "0"
                  l -> map toUpper $ showHex (((2 :: Integer) ^ l) - 1) ""
    defField S.DataField { S.fieldName = fn, S.fieldRef = fr} = [i|#{refDecl fr} #{ident2str fn};|]
    defField f@S.EmptyField {} = emptyFieldComment f
    fdefs = intercalate "\n    " $ map defField fields

defUnion :: String -> (C.Identifier -> String) -> [S.Field] -> String
defUnion n refDecl fields = chompNewline [i|
  #define UNION_MIN_TAG_VALUE_#{n} (0x#{minTag}ull)
  #define UNION_MAX_TAG_VALUE_#{n} (0x#{maxTag}ull)
  struct #{n} {
    enum #{n}_tag {
      #{tagDefs}
    } _tag;

#{unionDecl}
  };
|]
  where
    defField S.DataField { S.fieldName = fn, S.fieldRef = fr} = [i|#{refDecl fr} #{ident2str fn};|]
    defField f@S.EmptyField {} = emptyFieldComment f
    defTag f = [i|#{n}_tag_#{ident2str $ S.fieldName f} = #{S.fieldIndex f},|]
    fdefs = intercalate "\n      " $ map defField fields
    tagDefs = intercalate "\n      " $ map defTag fields
    minTag = S.fieldIndex (head fields)
    maxTag = S.fieldIndex (last fields)

    isEmpty S.EmptyField {} = True
    isEmpty _ = False

    unionDecl =
      if length (filter (not . isEmpty) fields) <= 0
        then ""
        else [i|
    union {
      #{fdefs}
    };
|]

emptyFieldComment :: S.Field -> String
emptyFieldComment S.EmptyField { S.fieldName = fn, S.fieldIndex = ix } = [i|/* no data for field #{fn} with index #{ix} */|]
emptyFieldComment _ = error "emptyFieldComment: invalid for all but EmptyField"
