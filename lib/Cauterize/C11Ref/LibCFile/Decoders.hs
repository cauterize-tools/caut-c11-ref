{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.Decoders
  ( typeDecoder
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.List (intercalate)
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S

typeDecoder :: S.Type -> String
typeDecoder t = chompNewline [i|
  R decode_#{name}(DI * const _c_iter, #{decl} * const _c_obj) {
#{decoderBody t}
  }
|]
  where
    name = ident2str $ S.typeName t
    decl = t2decl t

decoderBody :: S.Type -> String
decoderBody t = b
  where
    n = ident2str $ S.typeName t
    b = case S.typeDesc t of
          S.Synonym { S.synonymRef = r } -> synonymDecoderBody (ident2str r)
          S.Range { S.rangeOffset = o, S.rangeLength = l, S.rangeTag = rt } -> rangeDecoderBody o l rt
          S.Array { S.arrayRef = r } -> arrayDecoderBody (ident2str r)
          S.Vector { S.vectorRef = r, S.vectorTag = lr } -> vectorDecoderBody n (ident2str r) lr
          S.Enumeration { S.enumerationValues = vs, S.enumerationTag = et } -> enumerationDecoderBody n vs et
          S.Record { S.recordFields = fs } -> recordDecoderBody fs
          S.Combination { S.combinationFields = fs, S.combinationTag = fr } -> combinationDecoderBody n fs fr
          S.Union { S.unionFields = fs , S.unionTag = tr } -> unionDecoderBody n fs tr

synonymDecoderBody :: String -> String
synonymDecoderBody r = [i|    return decode_#{r}(_c_iter, (#{r} *)_c_obj);|]

rangeDecoderBody :: C.Offset -> C.Length -> C.Tag -> String
rangeDecoderBody o l t = chompNewline [i|
    #{tag2c t} _c_tag;
    STATUS_CHECK(#{tag2decodefn t}(_c_iter, &_c_tag));

    if (_c_tag > #{show l}) {
      return caut_status_range_out_of_bounds;
    }

    *_c_obj = _c_tag + #{show o};

    return caut_status_ok;|]

arrayDecoderBody :: String -> String
arrayDecoderBody r = chompNewline [i|
    for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
      STATUS_CHECK(decode_#{r}(_c_iter, &_c_obj->elems[_c_i]));
    }

    return caut_status_ok;|]

vectorDecoderBody :: String -> String -> C.Tag -> String
vectorDecoderBody n r lr = chompNewline [i|
    STATUS_CHECK(#{tag2decodefn lr}(_c_iter, &_c_obj->_length));

    if (_c_obj->_length > VECTOR_MAX_LEN_#{n}) {
      return caut_status_invalid_length;
    }

    for (size_t _c_i = 0; _c_i < _c_obj->_length; _c_i++) {
      STATUS_CHECK(decode_#{r}(_c_iter, &_c_obj->elems[_c_i]));
    }

    return caut_status_ok;|]

enumerationDecoderBody :: String -> [S.EnumVal] -> C.Tag -> String
enumerationDecoderBody _ [] _ = error "enumerationDecoderBody: enumerations must have at lesat one value."
enumerationDecoderBody n vs t = chompNewline [i|
    #{tag2c t} _c_tag;
    STATUS_CHECK(#{tag2decodefn t}(_c_iter, &_c_tag));

    if (#{lsym} < _c_tag) {
      return caut_status_enumeration_out_of_range;
    }

    *_c_obj = (enum #{n})_c_tag;

    return caut_status_ok;|]
  where
    lsym = [i|#{n}_#{S.enumValName (last vs)}|]

recordDecoderBody :: [S.Field] -> String
recordDecoderBody fs =
  let fencs = map (("    " ++) . decodeField) fs
      withReturn = fencs ++ ["", "    return caut_status_ok;"]
  in intercalate "\n" withReturn

combinationDecoderBody :: String -> [S.Field] -> C.Tag -> String
combinationDecoderBody n fs fr =
  let decodeFlags = [i|    STATUS_CHECK(#{tag2decodefn fr}(_c_iter, &_c_obj->_flags));|] ++ "\n"
      checkFlags  = [i|    if (~COMBINATION_FLAGS_#{n} & _c_obj->_flags) { return caut_status_invalid_flags; }|]
      decodeFields = map decodeCombField fs
  in intercalate "\n" $ (decodeFlags : checkFlags : decodeFields) ++ ["", "    return caut_status_ok;"]

unionDecoderBody :: String -> [S.Field] -> C.Tag -> String
unionDecoderBody n fs tr = chompNewline [i|
    #{tr} _temp_tag;
    STATUS_CHECK(#{tag2decodefn tr}(_c_iter, &_temp_tag));

    if (_temp_tag >= UNION_NUM_FIELDS_#{n}) {
      return caut_status_invalid_tag;
    } else {
      _c_obj->_tag = (enum #{n}_tag)_temp_tag;
    }

    switch(_c_obj->_tag) {
#{fields}
    }

    return caut_status_ok;|]
  where
    fields = intercalate "\n" $ map (decodeUnionField n) fs


decodeField :: S.Field -> String
decodeField S.DataField { S.fieldName = n, S.fieldRef = r } =
  [i|STATUS_CHECK(decode_#{r}(_c_iter, &_c_obj->#{n}));|]
decodeField S.EmptyField { S.fieldName = n, S.fieldIndex = ix } =
  [i|/* No data for field #{n} with index #{ix}. */|]

decodeCombField :: S.Field -> String
decodeCombField f@S.EmptyField {} = "    " ++ decodeField f
decodeCombField f@S.DataField { S.fieldIndex = ix } = chompNewline [i|
    if (FSET(_c_obj->_flags, #{ix})) { #{decodeField f} }|]

decodeUnionField :: String -> S.Field -> String
decodeUnionField n f = [i|    case #{n}_tag_#{S.fieldName f}: #{decodeField f} break;|]
