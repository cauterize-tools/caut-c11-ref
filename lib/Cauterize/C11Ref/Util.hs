{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.Util
  ( bi2c
  , t2decl
  , len2c
  , len2bi
  , chompNewline
  , comment
  ) where

import Data.Text.Lazy (unpack)
import Data.String.Interpolate
import qualified Cauterize.Common.Types as S
import qualified Cauterize.Specification as S

bi2c :: S.BuiltIn -> String
bi2c S.BIu8   = "uint8_t"
bi2c S.BIu16  = "uint16_t"
bi2c S.BIu32  = "uint32_t"
bi2c S.BIu64  = "uint64_t"
bi2c S.BIs8   = "int8_t"
bi2c S.BIs16  = "int16_t"
bi2c S.BIs32  = "int32_t"
bi2c S.BIs64  = "int64_t"
bi2c S.BIf32  = "float"
bi2c S.BIf64  = "double"
bi2c S.BIbool = "bool"

t2decl :: S.SpType -> String
t2decl t = case t of
    S.BuiltIn {} -> n
    S.Synonym {} -> n
    S.Array {} -> sn
    S.Vector {} -> sn
    S.Record {} -> sn
    S.Combination {} -> sn
    S.Union {} -> sn
  where
    n = unpack $ S.typeName t
    sn = [i|struct #{n}|]

len2c :: Integer -> String
len2c 1 = "uint8_t";
len2c 2 = "uint16_t";
len2c 4 = "uint32_t";
len2c 8 = "uint64_t";
len2c e = error "len2c: invalid length " ++ show e ++ "."

len2bi :: Integer -> String
len2bi 1 = "u8";
len2bi 2 = "u16";
len2bi 4 = "u32";
len2bi 8 = "u64";
len2bi e = error "len2bi: invalid length " ++ show e ++ "."

chompNewline :: String -> String
chompNewline ('\n':rest) = rest
chompNewline str = str

comment :: String -> String
comment s = [i|
  /* #{s} */
|]
