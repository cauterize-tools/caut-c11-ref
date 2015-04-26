{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.Encoders
  ( typeEncoder
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.Text.Lazy (unpack)
import Data.List (intercalate)
import qualified Cauterize.Common.Types as S
import qualified Cauterize.Specification as S

typeEncoder :: S.SpType -> String
typeEncoder t = chompNewline [i|
  R encode_#{name}(EI * const _c_iter, #{decl} const * const _c_obj) {
#{encoderBody t}
  }
|]
  where
    name = S.typeName t
    decl = t2decl t

encoderBody :: S.SpType -> String
encoderBody t = b
  where
    n = unpack $ S.typeName t
    b = case t of
          S.BuiltIn {} -> builtinEncoderBody n
          S.Synonym { S.unSynonym = S.TSynonym { S.synonymRepr = r } } ->
            synonymEncoderBody r
          S.Array { S.unArray = S.TArray { S.arrayRef = r } } ->
            arrayEncoderBody (unpack r)
          S.Vector { S.unVector = S.TVector { S.vectorRef = r }
                    , S.lenRepr = S.LengthRepr lr } ->
            vectorEncoderBody n (unpack r) lr
          S.Record { S.unRecord = S.TRecord { S.recordFields = S.Fields fs } } ->
            recordEncoderBody fs
          S.Combination { S.unCombination = S.TCombination { S.combinationFields = S.Fields fs }
                        , S.flagsRepr = S.FlagsRepr fr } ->
            combinationEncoderBody n fs fr
          S.Union { S.unUnion = S.TUnion { S.unionFields = S.Fields fs }
                  , S.tagRepr = S.TagRepr tr } ->
            unionEncoderBody n fs tr

builtinEncoderBody :: String -> String
builtinEncoderBody n = [i|    return __caut_encode_#{n}(_c_iter, _c_obj);|]

synonymEncoderBody :: S.BuiltIn -> String
synonymEncoderBody r = [i|    return __caut_encode_#{r}(_c_iter, (#{r} *)_c_obj);|]

arrayEncoderBody :: String -> String
arrayEncoderBody r = chompNewline [i|
    for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
      STATUS_CHECK(encode_#{r}(_c_iter, &_c_obj->elems[_c_i]));
    }

    return caut_status_ok;|]

vectorEncoderBody :: String -> String -> S.BuiltIn -> String
vectorEncoderBody n r lr = chompNewline [i|
    if (_c_obj->_length > VECTOR_MAX_LEN_#{n}) {
      return caut_status_invalid_length;
    }

    STATUS_CHECK(encode_#{lr}(_c_iter, &_c_obj->_length));

    for (size_t _c_i = 0; _c_i < _c_obj->_length; _c_i++) {
      STATUS_CHECK(encode_#{r}(_c_iter, &_c_obj->elems[_c_i]));
    }

    return caut_status_ok;|]

recordEncoderBody :: [S.Field] -> String
recordEncoderBody fs =
  let fencs = map (("    " ++) . encodeField) fs
      withReturn = fencs ++ ["", "    return caut_status_ok;"]
  in intercalate "\n" withReturn

combinationEncoderBody :: String -> [S.Field] -> S.BuiltIn -> String
combinationEncoderBody n fs fr =
  let checkFlags  = [i|    if (~COMBINATION_FLAGS_#{n} & _c_obj->_flags) { return caut_status_invalid_flags; }|]
      encodeFlags = [i|    STATUS_CHECK(encode_#{fr}(_c_iter, &_c_obj->_flags));|] ++ "\n"
      encodeFields = map encodeCombField fs
  in intercalate "\n" $ (checkFlags : encodeFlags : encodeFields) ++ ["", "    return caut_status_ok;"]

unionEncoderBody :: String -> [S.Field] -> S.BuiltIn -> String
unionEncoderBody n fs tr = chompNewline [i|
    #{tr} _temp_tag = (#{tr})_c_obj->_tag;

    if (_temp_tag >= UNION_NUM_FIELDS_#{n}) {
      return caut_status_invalid_tag;
    }

    STATUS_CHECK(encode_#{tr}(_c_iter, &_temp_tag));

    switch(_c_obj->_tag) {
#{fields}
    }

    return caut_status_ok;|]
  where
    fields = intercalate "\n" $ map (encodeUnionField n) fs

encodeField :: S.Field -> String
encodeField S.Field { S.fName = n, S.fRef = r } =
  [i|STATUS_CHECK(encode_#{r}(_c_iter, &_c_obj->#{n}));|]
encodeField S.EmptyField { S.fName = n, S.fIndex = ix } =
  [i|/* No data for field #{n} with index #{ix}. */|]

encodeCombField :: S.Field -> String
encodeCombField f@S.EmptyField {} = "    " ++ encodeField f
encodeCombField f@S.Field { S.fIndex = ix } = chompNewline [i|
    if (FSET(_c_obj->_flags, #{ix})) { #{encodeField f} }|]

encodeUnionField :: String -> S.Field -> String
encodeUnionField n f = [i|    case #{n}_tag_#{S.fName f}: #{encodeField f} break;|]
