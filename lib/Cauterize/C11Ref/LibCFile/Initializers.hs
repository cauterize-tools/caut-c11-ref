{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.Initializers
  ( typeInit
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.List (intercalate)
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S

typeInit :: S.Type -> String
typeInit t = chompNewline [i|
  void init_#{name}(#{decl} * _c_obj) {
#{initBody t}
  }
|]
  where
    name = ident2str $ S.typeName t
    decl = t2decl t

initBody :: S.Type -> String
initBody t = b
  where
    n = ident2str . S.typeName $ t
    b =
      case S.typeDesc t of
        S.Synonym {} -> initSynonym
        S.Range { S.rangeOffset = o } -> initRange o
        S.Array { S.arrayRef = r } -> initArray (ident2str r)
        S.Vector {} -> initVector
        S.Enumeration { S.enumerationValues = vs } -> initEnumeration vs
        S.Record { S.recordFields = fs } -> initRecord fs
        S.Combination {} -> initCombination
        S.Union { S.unionFields = fs
                , S.unionTag = tr } -> initUnion n (show tr) fs

initRange :: C.Offset -> String
initRange o = [i|    *_c_obj = #{show o};|]

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

initEnumeration :: [S.EnumVal] -> String
initEnumeration [] = error "initEnumeration: enumerations must have at least one value."
initEnumeration (S.EnumVal v _:_) = [i|    *_c_obj = #{ident2str v};|]

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
    fn = S.fieldName f

initField :: S.Field -> String
initField S.EmptyField { S.fieldName = n }
  = [i|    /* No initializer for empty field #{n} */|]
initField S.DataField { S.fieldName = n ,S.fieldRef = r }
  = [i|    init_#{r}(&_c_obj->#{n});|]
