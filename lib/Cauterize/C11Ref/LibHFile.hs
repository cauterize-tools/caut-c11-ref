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
import Data.Text.Lazy (unpack)
import Numeric
import qualified Cauterize.Common.Types as S
import qualified Cauterize.Specification as S
import qualified Data.Map as M

hFileFromSpec :: S.Spec -> String
hFileFromSpec = unindent . concat . fromSpec

fromSpec :: S.Spec -> [String]
fromSpec s = [chompNewline [i|
  #ifndef #{guardSym}
  #define #{guardSym}

  #include "cauterize.h"
|]
  , comment "library meta information"
  , chompNewline [i|
  #define NAME_#{ln} "#{ln}"
  #define VERSION_#{ln} "#{unpack $ S.specVersion s}"
  #define MIN_SIZE_#{ln} (#{S.minSize libsize})
  #define MAX_SIZE_#{ln} (#{S.maxSize libsize})

  extern hashtype_t const SCHEMA_HASH_#{ln};
|]
  , comment "type size information"
  , unlines (map typeSizeInfo types)

  , comment "type hash externs"
  , unlines (map typeHash types)

  , comment "forward declarations"
  , unlines (mapMaybe typeForwardDecl types)

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
    blankLine = "  \n"
    libsize = S.specSize s
    ln = unpack $ S.specName s
    types = S.specTypes s

    -- Names to how you delcare that name
    n2declMap = let s' = S.specTypes s
                    d = map t2decl s'
                    n = fmap S.typeName s'
                in M.fromList $ zip n d
    luDecl n = fromMaybe (error $ "Invalid name: " ++ unpack n ++ ".")
                         (M.lookup n n2declMap)

typeSizeInfo :: S.SpType -> String
typeSizeInfo t = chompNewline [i|
  #define MIN_SIZE_#{tn} (#{minS})
  #define MAX_SIZE_#{tn} (#{maxS})|]
  where
    tn   = S.typeName t
    minS = S.minSize t
    maxS = S.maxSize t

typeHash :: S.SpType -> String
typeHash t = [i|  extern hashtype_t const TYPE_HASH_#{n};|] -- two spaces to line up with the 'unindent' call in the end
  where
    n = S.typeName t

typeForwardDecl :: S.SpType -> Maybe String
typeForwardDecl t = fmap ("  " ++) (go t)
  where
    n = S.typeName t
    structish flavor = Just [i|struct #{n}; /* #{flavor} */ |]
    go S.BuiltIn { S.unBuiltIn = S.TBuiltIn { S.unTBuiltIn = b } } =
      case b of
        S.BIbool -> Nothing -- We don't want to redefine 'bool'. stdbool.h defines this for us.
        b' -> Just [i|typedef #{bi2c b'} #{n}; /* builtin */|]
    go S.Synonym { S.unSynonym = S.TSynonym { S.synonymRepr = r } } =
      Just [i|typedef #{bi2c r} #{n}; /* synonym */|]
    go S.Array {} = structish "array"
    go S.Vector {} = structish "vector"
    go S.Record {} = structish "record"
    go S.Combination {} = structish "combination"
    go S.Union {} = structish "union"

typeFuncPrototypes :: S.SpType -> String
typeFuncPrototypes t = chompNewline [i|
  enum caut_status encode_#{n}(struct caut_encode_iter * const _c_iter, #{d} const * const _c_obj);
  enum caut_status decode_#{n}(struct caut_decode_iter * const _c_iter, #{d} * const _c_obj);
  size_t encoded_size_#{n}(#{d} const * const _c_obj);
  void init_#{n}(#{d} * _c_obj);
  enum caut_ord order_#{n}(#{d} const * const _c_a, #{d} const * const _c_b);
|]
  where
    d = t2decl t
    n = S.typeName t

typeDefinition :: (S.Name -> String) -> S.SpType -> Maybe String
typeDefinition refDecl t =
  case t of
    S.Array { S.unArray = S.TArray { S.arrayRef = r, S.arrayLen = l } } -> Just $ defArray n (refDecl r) l
    S.Vector { S.unVector = S.TVector { S.vectorRef = r, S.vectorMaxLen = l }
              , S.lenRepr = S.LengthRepr lr } -> Just $ defVector n (refDecl r) l lr
    S.Record { S.unRecord = S.TRecord { S.recordFields = S.Fields fs } } -> Just $ defRecord n refDecl fs
    S.Combination { S.unCombination = S.TCombination { S.combinationFields = S.Fields fs }
                  , S.flagsRepr = S.FlagsRepr fr } -> Just $ defCombination n refDecl fs fr
    S.Union { S.unUnion = S.TUnion { S.unionFields = S.Fields fs } } -> Just $ defUnion n refDecl fs
    _ -> Nothing
  where
    n = unpack $ S.typeName t

defArray :: String -> String -> Integer -> String
defArray n refDecl len =
  let lenSym = [i|ARRAY_LEN_#{n}|]
  in chompNewline [i|
  #define #{lenSym} (#{len})
  struct #{n} {
    #{refDecl} elems[#{lenSym}];
  };
|]

defVector :: String -> String -> Integer -> S.BuiltIn -> String
defVector n refDecl len lenRep =
  let maxLenSym = [i|VECTOR_MAX_LEN_#{n}|]
      lenRepDecl = bi2c lenRep
  in chompNewline [i|
  #define #{maxLenSym} (#{len})
  struct #{n} {
    #{lenRepDecl} _length;
    #{refDecl} elems[#{maxLenSym}];
  };
|]

defRecord :: String -> (S.Name -> String) -> [S.Field] -> String
defRecord n refDecl fields = chompNewline [i|
  struct #{n} {
    #{fdefs}
  };
|]
  where
    defField S.Field { S.fName = fn, S.fRef = fr} = [i|#{refDecl fr} #{fn};|]
    defField S.EmptyField { S.fName = fn } = [i|/* no data for field #{fn} */|]
    fdefs = intercalate "\n    " $ map defField fields

defCombination :: String -> (S.Name -> String) -> [S.Field] -> S.BuiltIn -> String
defCombination n refDecl fields flagsRepr = chompNewline [i|
  #define COMBINATION_FLAGS_#{n} (0x#{flagsMask}ull)
  struct #{n} {
    #{bi2c flagsRepr} _flags;
    #{fdefs}
  };
|]
  where
    flagsMask = case length fields of
                  0 -> "0"
                  l -> map toUpper $ showHex (((2 :: Integer) ^ (l - 1)) - 1) ""
    defField S.Field { S.fName = fn, S.fRef = fr} = [i|#{refDecl fr} #{fn};|]
    defField S.EmptyField { S.fName = fn } = [i|/* no data for field #{fn} */|]
    fdefs = intercalate "\n    " $ map defField fields

defUnion :: String -> (S.Name -> String) -> [S.Field] -> String
defUnion n refDecl fields = chompNewline [i|
  #define UNION_NUM_FIELDS_#{n} (0x#{numFields}ull)
  struct #{n} {
    enum #{n}_tag {
      #{tagDefs}
    } _tag;

    union {
      #{fdefs}
    };
  };
|]
  where
    defField S.Field { S.fName = fn, S.fRef = fr} = [i|#{refDecl fr} #{fn};|]
    defField S.EmptyField { S.fName = fn } = [i|/* no data for field #{fn} */|]
    defTag f = [i|#{n}_tag_#{S.fName f} = #{S.fIndex f},|]
    fdefs = intercalate "\n      " $ map defField fields
    tagDefs = intercalate "\n      " $ map defTag fields
    numFields = length fields
