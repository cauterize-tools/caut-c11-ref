{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.Util
  ( prim2c
  , prim2suffix
  , t2decl
  , len2c
  , len2tag
  , tag2c
  , tag2decodefn
  , tag2encodefn
  , chompNewline
  , comment
  , ident2str
  , primDeclMap
  ) where

import Data.Text (unpack)
import Data.String.Interpolate
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S
import qualified Data.Map as M

prim2c :: C.Prim -> String
prim2c C.PU8   = "uint8_t"
prim2c C.PU16  = "uint16_t"
prim2c C.PU32  = "uint32_t"
prim2c C.PU64  = "uint64_t"
prim2c C.PS8   = "int8_t"
prim2c C.PS16  = "int16_t"
prim2c C.PS32  = "int32_t"
prim2c C.PS64  = "int64_t"
prim2c C.PF32  = "float"
prim2c C.PF64  = "double"
prim2c C.PBool = "bool"

prim2suffix :: C.Prim -> String
prim2suffix C.PU8   = ""
prim2suffix C.PU16  = ""
prim2suffix C.PU32  = "lu"
prim2suffix C.PU64  = "llu"
prim2suffix C.PS8   = ""
prim2suffix C.PS16  = ""
prim2suffix C.PS32  = "l"
prim2suffix C.PS64  = "ll"
prim2suffix C.PF32  = "f"
prim2suffix C.PF64  = ""
prim2suffix C.PBool = ""

t2decl :: S.Type -> String
t2decl (S.Type { S.typeName = tname, S.typeDesc = t}) =
  case t of
    S.Synonym {} -> n
    S.Range {} -> n
    S.Array {} -> sn
    S.Vector {} -> sn
    S.Enumeration {} -> en
    S.Record {} -> sn
    S.Combination {} -> sn
    S.Union {} -> sn
  where
    n = unpack (C.unIdentifier tname)
    sn = [i|struct #{n}|]
    en = [i|enum #{n}|]

len2c :: Integer -> String
len2c 1 = "uint8_t";
len2c 2 = "uint16_t";
len2c 4 = "uint32_t";
len2c 8 = "uint64_t";
len2c e = error "len2c: invalid length " ++ show e ++ "."

tag2c :: C.Tag -> String
tag2c C.T1 = "caut_tag8_t";
tag2c C.T2 = "caut_tag16_t";
tag2c C.T4 = "caut_tag32_t";
tag2c C.T8 = "caut_tag64_t";

tag2decodefn :: C.Tag -> String
tag2decodefn C.T1 = "decode_tag8";
tag2decodefn C.T2 = "decode_tag16";
tag2decodefn C.T4 = "decode_tag32";
tag2decodefn C.T8 = "decode_tag64";

tag2encodefn :: C.Tag -> String
tag2encodefn C.T1 = "encode_tag8";
tag2encodefn C.T2 = "encode_tag16";
tag2encodefn C.T4 = "encode_tag32";
tag2encodefn C.T8 = "encode_tag64";

len2tag :: Integer -> String
len2tag 1 = "u8";
len2tag 2 = "u16";
len2tag 4 = "u32";
len2tag 8 = "u64";
len2tag e = error "len2bi: invalid length " ++ show e ++ "."

chompNewline :: String -> String
chompNewline ('\n':rest) = rest
chompNewline str = str

comment :: String -> String
comment s = [i|
  /* #{s} */
|]

ident2str :: C.Identifier -> String
ident2str = unpack . C.unIdentifier

primDeclMap :: M.Map C.Identifier String
primDeclMap = fmap prim2c C.primMap
