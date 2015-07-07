{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.Initializers
  ( typeInit
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.Text.Lazy (unpack)
import Data.List (intercalate)
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S

typeInit :: S.SpType -> String
typeInit t = chompNewline [i|
  void init_#{name}(#{decl} * _c_obj) {
#{initBody t}
  }
|]
  where
    name = S.typeName t
    decl = t2decl t

initBody :: S.SpType -> String
initBody t = b
  where
    n = unpack $ S.typeName t
    b =
      case t of
        S.BuiltIn {} -> initBuiltin
        S.Synonym {} -> initSynonym
        S.Array { S.unArray = S.TArray { S.arrayRef = r } } -> initArray (unpack r)
        S.Vector {} -> initVector
        S.Record { S.unRecord = S.TRecord { S.recordFields = S.Fields fs } } -> initRecord fs
        S.Combination {} -> initCombination
        S.Union { S.unUnion = S.TUnion { S.unionFields = S.Fields { S.unFields = fs } }
                , S.tagRepr = S.TagRepr tr } -> initUnion n (show tr) fs

initBuiltin :: String
initBuiltin = chompNewline [i|
    *_c_obj = 0;|]

initSynonym :: String
initSynonym = chompNewline [i|
    *_c_obj = 0;|]

initArray :: String -> String
initArray r = chompNewline [i|
    for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
      init_#{r}(&_c_obj->elems[_c_i]);
    }|]

initVector :: String
initVector = chompNewline [i|
    _c_obj->_length = 0;|]

initRecord :: [S.Field] -> String
initRecord fs = intercalate "\n" $ map initField fs

initCombination :: String
initCombination = chompNewline [i|
    _c_obj->_flags = 0;|]

initUnion :: String -> String -> [S.Field] -> String
initUnion _ _ [] = ""
initUnion n rep (f:_) = chompNewline [i|
    _c_obj->_tag = (#{rep}) #{n}_tag_#{fn};
#{initField f}|]
  where
    fn = S.fName f

initField :: S.Field -> String
initField S.EmptyField { S.fName = n }        = [i|    /* No initializer for empty field #{n} */|]
initField S.Field { S.fName = n ,S.fRef = r } = [i|    init_#{r}(&_c_obj->#{n});|]
