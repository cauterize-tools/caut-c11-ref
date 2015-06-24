{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.Comparators
  ( typeCompare
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.Text.Lazy (unpack)
import Data.List (intercalate)
import qualified Cauterize.Common.Types as S
import qualified Cauterize.Specification as S

typeCompare :: S.SpType -> String
typeCompare t = chompNewline [i|
  enum caut_ord compare_#{name}(#{decl} const * const _c_a, #{decl} const * const _c_b) {
#{compareBody t}
  }
|]
  where
    name = S.typeName t
    decl = t2decl t

compareBody :: S.SpType -> String
compareBody t = b
  where
    n = unpack $ S.typeName t
    b =
      case t of
        S.BuiltIn {} -> compareBuiltin
        S.Synonym {} -> compareSynonym
        S.Array { S.unArray = S.TArray { S.arrayRef = r } } -> compareArray (unpack r)
        S.Vector { S.unVector = S.TVector { S.vectorRef = r } } -> compareVector (unpack r)
        S.Record { S.unRecord = S.TRecord { S.recordFields = S.Fields fs } } -> compareRecord fs
        S.Combination { S.unCombination = S.TCombination { S.combinationFields = S.Fields fs } } -> compareCombination fs
        S.Union { S.unUnion = S.TUnion { S.unionFields = S.Fields { S.unFields = fs } }
                , S.tagRepr = S.TagRepr tr } -> compareUnion n (show tr) fs

compareBuiltin :: String
compareBuiltin = [i|    return CAUT_ORDER(*_c_a, *_c_b);|]

compareSynonym :: String
compareSynonym = [i|    return CAUT_ORDER(*_c_a, *_c_b);|]

compareArray :: String -> String
compareArray r = chompNewline [i|
    for (size_t _c_i = 0; _c_i < ARR_LEN(_c_a->elems); _c_i++) {
      const enum caut_ord _c_o = compare_#{r}(&_c_a->elems[_c_i], &_c_b->elems[_c_i]);
      if (caut_ord_eq != _c_o) {
        return _c_o;
      }
    }

    return caut_ord_eq;|]

compareVector :: String -> String
compareVector r = chompNewline [i|
    for (size_t _c_i = 0; _c_i < _c_a->_length && _c_i < _c_b->_length ; _c_i++) {
      const enum caut_ord _c_o = compare_#{r}(&_c_a->elems[_c_i], &_c_b->elems[_c_i]);
      if (caut_ord_eq != _c_o) {
        return _c_o;
      }
    }

    return CAUT_ORDER(_c_a->_length, _c_b->_length);|]

compareRecord :: [S.Field] -> String
compareRecord fs = chompNewline [i|
    enum caut_ord _c_o;

#{fieldComps}

    return caut_ord_eq;
|]
  where
    fieldComps = intercalate "\n" $ map compareRecordField fs

compareCombination :: [S.Field] -> String
compareCombination fs = chompNewline [i|
    bool a, b;

#{fieldComps}

    return caut_ord_eq;
|]
  where
    fieldComps = intercalate "\n" $ map compareCombinationField fs

compareUnion :: String -> String -> [S.Field] -> String
compareUnion n tr fs = chompNewline [i|
|]
-- "(void)_c_a; (void)_c_b; return caut_ord_eq;"

compareRecordField :: S.Field -> String
compareRecordField S.EmptyField { S.fName = n }        = [i|    /* No comparison for empty field #{n} */|]
compareRecordField S.Field { S.fName = n ,S.fRef = r } = [i|    if (caut_ord_eq != (_c_o = compare_#{r}(&_c_a->#{n}, &_c_b->#{n}))) { return _c_o; }|]

compareCombinationField :: S.Field -> String
compareCombinationField f = chompNewline [i|
    a = CHECK_BIT(_c_a->_flags, #{ix}); b = CHECK_BIT(_c_b->_flags, #{ix});
    if (a && b) {
#{bothSetBody f}
    } else if (a && !b) {
      return caut_ord_gt;
    } else if (!a && b) {
      return caut_ord_lt;
    }|]
  where
    n = S.fName f
    ix = S.fIndex f
    bothSetBody S.EmptyField {} = chompNewline [i|      /* No comparison for empty field #{n}. */|]
    bothSetBody S.Field { S.fRef = r } = chompNewline [i|
      const enum caut_ord _c_o = compare_#{r}(&_c_a->#{n}, &_c_b->#{n});
      if (caut_ord_eq != _c_o) {
        return _c_o;
      }|]
