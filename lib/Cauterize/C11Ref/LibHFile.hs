{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibHFile
  ( hFileFromSpec
  ) where

import Cauterize.C11Ref.Util

import Data.Maybe
import qualified Data.Map as M
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text.Lazy (unpack)
import qualified Cauterize.Common.Types as S
import qualified Cauterize.Specification as S

hFileFromSpec :: S.Spec -> String
hFileFromSpec = unindent . concat . fromSpec

fromSpec :: S.Spec -> [String]
fromSpec s = [chompNewline [i|
  #ifndef #{guardSym}
  #define #{guardSym}
|]
  , comment "library meta information"
  , chompNewline [i|
  #define NAME_#{ln} "#{ln}"
  #define VERSION_#{ln} "#{unpack $ S.specVersion s}"
  #define MIN_SIZE_#{ln} (#{S.minSize libsize})
  #define MAX_SIZE_#{ln} (#{S.maxSize libsize})
|]
  , comment "type size information"
  , unlines (map (typeSizeInfo ln) types)

  , comment "forward declarations"
  , unlines (mapMaybe typeForwardDecl types)

  , comment "function prototypes"
  , unlines (map typeFuncPrototypes types)

  , comment "type definitions"
  , unlines (mapMaybe (typeDefinition luDecl) types)

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

typeSizeInfo :: String -> S.SpType -> String
typeSizeInfo ln t = chompNewline [i|
  #define MIN_SIZE_#{ln}_#{tn} (#{minS})
  #define MAX_SIZE_#{ln}_#{tn} (#{maxS})|]
  where
    tn   = S.typeName t
    minS = S.minSize t
    maxS = S.maxSize t

typeForwardDecl :: S.SpType -> Maybe String
typeForwardDecl t = fmap ("  " ++) (go t)
  where
    n = S.typeName t
    structish flavor = Just [i|struct #{n}; /* #{flavor} */ |]
    go S.BuiltIn { S.unBuiltIn = S.TBuiltIn { S.unTBuiltIn = b } } =
      case b of
        S.BIbool -> Nothing
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
  enum caut_status pack_#{n}(struct caut_pack_iter * const _c_iter, #{d} const * const _c_obj);
  enum caut_status unpack_#{n}(struct caut_unpack_iter * const _c_iter, #{d} * const _c_obj);
  size_t packed_size_#{n}(#{d} const * const _c_obj);
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
  }
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
  }
|]

    {-
    S.Vector { Sp.unVector = Sp.TVector { Sp.vectorRef = r, Sp.vectorMaxLen = l } , Sp.lenRepr = Sp.LengthRepr lr } ->
      CVector { ctdDecl = d
              , ctdReprName = r
              , ctdReprDecl = nameToDecl r
              , ctdVectorMaxLen = l
              , ctdVectorMaxLenReprName = pack . show $ lr
              , ctdVectorMaxLenReprDecl = builtInToStdType lr
              }
    Sp.Record { Sp.unRecord = Sp.TRecord { Sp.recordFields = Sp.Fields fs } } ->
      let fs' = P.map mkNamedRef' fs
      in CRecord { ctdDecl = d, ctdFields = fs' }
    Sp.Combination { Sp.unCombination = Sp.TCombination { Sp.combinationFields = Sp.Fields fs } , Sp.flagsRepr = Sp.FlagsRepr r } ->
      let fs' = P.map mkNamedRef' fs
      in CCombination { ctdDecl = d
                      , ctdFields = fs'
                      , ctdCombinationFlagsReprName = pack . show $ r
                      , ctdCombinationFlagsReprDecl = builtInToStdType r
                      }
    Sp.Union { Sp.unUnion = Sp.TUnion { Sp.unionFields = Sp.Fields fs } , Sp.tagRepr = Sp.TagRepr r } ->
      let fs' = P.map mkNamedRef' fs
      in CUnion { ctdDecl = d
                , ctdFields = fs'
                , ctdUnionTagReprName = pack . show $ r
                , ctdUnionTagReprDecl = builtInToStdType r
                , ctdHasData = hasData fs'
                }
                -}
