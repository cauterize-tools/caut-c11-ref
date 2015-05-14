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
import Data.Text.Lazy (unpack)
import Data.Word
import Numeric
import qualified Cauterize.FormHash as S
import qualified Cauterize.Specification as S

cMessageFileFromSpec :: S.Spec -> String
cMessageFileFromSpec = unindent . concat . fromSpec

fromSpec :: S.Spec -> [String]
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

typeDesc :: S.SpType -> String
typeDesc t = chompNewline [i|
    {
      .name = "#{n}",
      .hash = #{typeHashByteArray t},
      .encode = (gen_encode*)encode_#{n},
      .decode = (gen_decode*)decode_#{n},
      .min_size = #{S.minSize t},
      .max_size = #{S.maxSize t},
    }|]
  where
    n = S.typeName t

typeHashByteArray :: S.SpType -> String
typeHashByteArray t = [i|{ #{hashToBytes (S.spHash t)} }|]

-- Some utility functions specific to generating C files
hashToBytes :: S.FormHash -> String
hashToBytes h = let bs = S.hashToBytes h
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
