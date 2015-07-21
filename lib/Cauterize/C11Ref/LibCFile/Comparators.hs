{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.Comparators
  ( typeCompare
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.List (intercalate)
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S

typeCompare :: (C.Identifier -> String) -> S.Type -> String
typeCompare ident2decl t = chompNewline [i|
  enum caut_ord compare_#{name}(#{decl} const * const _c_a, #{decl} const * const _c_b) {
#{compareBody t}
  }
|]
  where
    name = ident2str $ S.typeName t
    decl = t2decl t

compareBody :: S.Type -> String
compareBody t = b
  where
    n = ident2str . S.typeName $ t
    b =
      case S.typeDesc t of
        S.Synonym { S.synonymRef = r } -> compareSynonym (ident2str r)
        S.Range { S.rangePrim = p } -> compareRange p
        S.Array {S.arrayRef = r } -> compareArray (ident2str r)
        S.Vector { S.vectorRef = r } -> compareVector (ident2str r)
        S.Enumeration {} -> compareEnumeration
        S.Record { S.recordFields = fs } -> compareRecord fs
        S.Combination { S.combinationFields = fs } -> compareCombination fs
        S.Union { S.unionFields = fs } -> compareUnion n fs

compareSynonym :: String -> String
compareSynonym rn = [i|    return compare_#{rn}(_c_a, _c_b);|]

compareRange :: C.Prim -> String
compareRange p = [i|    return compare_#{p'}(_c_a, _c_b);|]
  where
    p' = ident2str . C.primToText $ p

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

compareEnumeration :: String
compareEnumeration = [i|    return CAUT_ORDER(*_c_a, *_c_b);|]

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

compareUnion :: String -> [S.Field] -> String
compareUnion n fs = chompNewline [i|
    enum caut_ord _c_o;

    if (caut_ord_eq != (_c_o = CAUT_ORDER(_c_a->_tag, _c_b->_tag))) {
      return _c_o;
    }

    switch(_c_a->_tag) {
#{fieldComps}
    }

    return caut_ord_eq;|]
  where
    fieldComps = intercalate "\n" $ map (compareUnionField n) fs

compareRecordField :: S.Field -> String
compareRecordField S.EmptyField { S.fieldName = n }
  = [i|    /* No comparison for empty field #{ident2str n} */|]
compareRecordField S.DataField { S.fieldName = n ,S.fieldRef = r }
  = [i|    if (caut_ord_eq != (_c_o = compare_#{ident2str r}(&_c_a->#{ident2str n}, &_c_b->#{ident2str n}))) { return _c_o; }|]

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
    n = S.fieldName f
    ix = S.fieldIndex f
    bothSetBody S.EmptyField {} = chompNewline [i|      /* No comparison for empty field #{ident2str n}. */|]
    bothSetBody S.DataField { S.fieldRef = r } = chompNewline [i|
      const enum caut_ord _c_o = compare_#{ident2str r}(&_c_a->#{ident2str n}, &_c_b->#{ident2str n});
      if (caut_ord_eq != _c_o) {
        return _c_o;
      }|]

compareUnionField :: String -> S.Field -> String
compareUnionField tname f = chompNewline [i|
    case #{tname}_tag_#{ident2str n}:
#{compBody f}|]
  where
    n = S.fieldName f
    compBody S.EmptyField {} = chompNewline [i|
      _c_o = caut_ord_eq; /* No comparison for empty field #{ident2str n} */
      break;|]

    compBody S.DataField { S.fieldRef = r } = chompNewline [i|
      _c_o = compare_#{ident2str r}(&_c_a->#{ident2str n}, &_c_b->#{ident2str n});
      break;|]
