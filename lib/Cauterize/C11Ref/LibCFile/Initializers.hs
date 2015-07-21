{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.Initializers
  ( typeInit
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.List (intercalate)
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S

typeInit :: (C.Identifier -> String) -> S.Type -> String
typeInit ident2decl t = chompNewline [i|
  void init_#{name}(#{decl} * _c_obj) {
#{initBody ident2decl t}
  }
|]
  where
    name = ident2str $ S.typeName t
    decl = t2decl t

initBody :: (C.Identifier -> String) -> S.Type -> String
initBody ident2decl t = b
  where
    n = ident2str . S.typeName $ t
    b =
      case S.typeDesc t of
        S.Synonym { S.synonymRef = r} -> initSynonym r
        S.Range { S.rangeOffset = o } -> initRange o
        S.Array { S.arrayRef = r } -> initArray (ident2str r)
        S.Vector {} -> initVector
        S.Enumeration { S.enumerationValues = vs } -> initEnumeration n vs
        S.Record { S.recordFields = fs } -> initRecord fs
        S.Combination {} -> initCombination
        S.Union { S.unionFields = fs
                , S.unionTag = tr } -> initUnion n tr fs

initRange :: C.Offset -> String
initRange o = [i|    *_c_obj = #{show o};|]

initSynonym :: C.Identifier -> String
initSynonym r = chompNewline [i|
    init_#{ident2str r}(_c_obj);|]

initArray :: String -> String
initArray r = chompNewline [i|
    for (size_t _c_i = 0; _c_i < ARR_LEN(_c_obj->elems); _c_i++) {
      init_#{r}(&_c_obj->elems[_c_i]);
    }|]

initVector :: String
initVector = chompNewline [i|
    _c_obj->_length = 0;|]

initEnumeration :: String -> [S.EnumVal] -> String
initEnumeration _ [] = error "initEnumeration: enumerations must have at least one value."
initEnumeration n (S.EnumVal v _:_) = [i|    *_c_obj = #{n}_#{ident2str v};|]

initRecord :: [S.Field] -> String
initRecord fs = intercalate "\n" $ map initField fs

initCombination :: String
initCombination = chompNewline [i|
    _c_obj->_flags = 0;|]

initUnion :: String -> C.Tag -> [S.Field] -> String
initUnion _ _ [] = ""
initUnion n rep (f:_) = chompNewline [i|
    _c_obj->_tag = (#{tag2c rep}) #{n}_tag_#{ident2str fn};
#{initField f}|]
  where
    fn = S.fieldName f

initField :: S.Field -> String
initField S.EmptyField { S.fieldName = n }
  = [i|    /* No initializer for empty field #{ident2str n} */|]
initField S.DataField { S.fieldName = n ,S.fieldRef = r }
  = [i|    init_#{ident2str r}(&_c_obj->#{ident2str n});|]
