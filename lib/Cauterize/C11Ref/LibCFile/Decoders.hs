{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.Decoders
  ( typeDecoder
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.Text.Lazy (unpack)
import Data.List (intercalate)
import qualified Cauterize.Common.Types as S
import qualified Cauterize.Specification as S

typeDecoder :: S.SpType -> String
typeDecoder t = chompNewline [i|
  R decode_#{name}(DI * const _c_iter, #{decl} * const _c_obj) {
#{decoderBody t}
  }
|]
  where
    name = S.typeName t
    decl = t2decl t

decoderBody :: S.SpType -> String
decoderBody t = b
  where
    n = unpack $ S.typeName t
    b = case t of
          S.BuiltIn {} -> builtinDecoderBody n
          S.Synonym { S.unSynonym = S.TSynonym { S.synonymRepr = r } } ->
            synonymDecoderBody r
          S.Array { S.unArray = S.TArray { S.arrayRef = r } } ->
            arrayDecoderBody (unpack r)
          S.Vector { S.unVector = S.TVector { S.vectorRef = r }
                    , S.lenRepr = S.LengthRepr lr } ->
            vectorDecoderBody n (unpack r) lr
          S.Record { S.unRecord = S.TRecord { S.recordFields = S.Fields fs } } ->
            recordDecoderBody fs
          S.Combination { S.unCombination = S.TCombination { S.combinationFields = S.Fields fs }
                        , S.flagsRepr = S.FlagsRepr fr } ->
            combinationDecoderBody n fs fr
          S.Union { S.unUnion = S.TUnion { S.unionFields = S.Fields fs }
                  , S.tagRepr = S.TagRepr tr } ->
            unionDecoderBody n fs tr

builtinDecoderBody :: String -> String
builtinDecoderBody n = [i|    return __caut_decode_#{n}(_c_iter, _c_obj);|]

synonymDecoderBody :: S.BuiltIn -> String
synonymDecoderBody r = [i|    return __caut_decode_#{r}(_c_iter, (#{r} *)_c_obj);|]

arrayDecoderBody :: String -> String
arrayDecoderBody r = chompNewline [i|
    for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
      STATUS_CHECK(decode_#{r}(_c_iter, &_c_obj->elems[_c_i]));
    }

    return caut_status_ok;|]

vectorDecoderBody :: String -> String -> S.BuiltIn -> String
vectorDecoderBody n r lr = chompNewline [i|
    STATUS_CHECK(decode_#{lr}(_c_iter, &_c_obj->_length));

    if (_c_obj->_length > VECTOR_MAX_LEN_#{n}) {
      return caut_status_invalid_length;
    }

    for (size_t _c_i = 0; _c_i < _c_obj->_length; _c_i++) {
      STATUS_CHECK(decode_#{r}(_c_iter, &_c_obj->elems[_c_i]));
    }

    return caut_status_ok;|]

recordDecoderBody :: [S.Field] -> String
recordDecoderBody fs =
  let fencs = map (("    " ++) . decodeField) fs
      withReturn = fencs ++ ["", "    return caut_status_ok;"]
  in intercalate "\n" withReturn

combinationDecoderBody :: String -> [S.Field] -> S.BuiltIn -> String
combinationDecoderBody n fs fr =
  let decodeFlags = [i|    STATUS_CHECK(decode_#{fr}(_c_iter, &_c_obj->_flags));|] ++ "\n"
      checkFlags  = [i|    if (~COMBINATION_FLAGS_#{n} & _c_obj->_flags) { return caut_status_invalid_flags; }|]
      decodeFields = map decodeCombField fs
  in intercalate "\n" $ (decodeFlags : checkFlags : decodeFields) ++ ["", "    return caut_status_ok;"]

unionDecoderBody :: String -> [S.Field] -> S.BuiltIn -> String
unionDecoderBody n fs tr = chompNewline [i|
    #{tr} _temp_tag;
    STATUS_CHECK(decode_#{tr}(_c_iter, &_temp_tag));

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
decodeField S.Field { S.fName = n, S.fRef = r } =
  [i|STATUS_CHECK(decode_#{r}(_c_iter, &_c_obj->#{n}));|]
decodeField S.EmptyField { S.fName = n, S.fIndex = ix } =
  [i|/* No data for field #{n} with index #{ix}. */|]

decodeCombField :: S.Field -> String
decodeCombField f@S.EmptyField {} = "    " ++ decodeField f
decodeCombField f@S.Field { S.fIndex = ix } = chompNewline [i|
    if (FSET(_c_obj->_flags, #{ix})) { #{decodeField f} }|]

decodeUnionField :: String -> S.Field -> String
decodeUnionField n f = [i|    case #{n}_tag_#{S.fName f}: #{decodeField f} break;|]
