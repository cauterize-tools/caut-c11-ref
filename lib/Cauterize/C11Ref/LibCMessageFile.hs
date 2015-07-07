{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCMessageFile
  ( cMessageFileFromSpec
  ) where

import Cauterize.C11Ref.LibCFile.MessageInterface
import Cauterize.C11Ref.Util
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)
import Data.Word
import Numeric
import qualified Cauterize.Hash as H
import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C

cMessageFileFromSpec :: S.Specification -> String
cMessageFileFromSpec = unindent . concat . fromSpec

fromSpec :: S.Specification -> [String]
fromSpec s = [chompNewline [i|
  #include "#{ln}_message.h"

  #define R enum caut_status
  #define EI struct caut_encode_iter
  #define DI struct caut_decode_iter
  #define FSET(FS,IX) ((FS) & (1ull << (IX)))
|]
  , comment "type descriptors"
  , chompNewline [i|
  const caut_type_descriptors_#{ln}_t type_descriptors = {
#{typeDescs}
  };
|]

  , comment "message interface"
  , messageInterfaceFromSpec s
  ]
  where
    types = S.specTypes s
    ln = unpack $ S.specName s
    typeDescs = intercalate ",\n" $ map typeDesc types

typeDesc :: S.Type -> String
typeDesc t = chompNewline [i|
    {
      .name = "#{n}",
      .hash = #{typeHashByteArray t},
      .encode = (gen_encode*)encode_#{n},
      .decode = (gen_decode*)decode_#{n},
      .min_size = #{C.sizeMin ts},
      .max_size = #{C.sizeMin ts},
    }|]
  where
    ts = S.typeSize t
    n = S.typeName t

typeHashByteArray :: S.Type -> String
typeHashByteArray t = [i|{ #{hashToBytes (S.typeFingerprint t)} }|]

-- Some utility functions specific to generating C files
hashToBytes :: H.Hash -> String
hashToBytes h = let bs = H.hashToBytes h
                in bytesToCSV bs

bytesToCSV :: [Word8] -> String
bytesToCSV bs = intercalate "," $ fmap showByte bs
  where
    showByte :: Word8 -> String
    showByte b = let s = showHex b ""
                 in case length s of
                      2 -> "0x" ++ map toUpper s
                      1 -> "0x0" ++ map toUpper s
                      _ -> error "This should be impossible."
