{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile
  ( cFileFromSpec
  ) where

import Cauterize.C11Ref.LibCFile.Encoders
import Cauterize.C11Ref.LibCFile.Decoders
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

cFileFromSpec :: S.Spec -> String
cFileFromSpec = unindent . concat . fromSpec

fromSpec :: S.Spec -> [String]
fromSpec s = [chompNewline [i|
  #include "#{ln}.h"

  #define R enum caut_status
  #define I struct caut_pack_iter
  #define FSET(FS,IX) ((FS) & (1ull << (IX)))
|]
  , comment "schema hash"
  , [i|  hashtype_t const SCHEMA_HASH_#{ln} = { #{hashToBytes (S.specHash s)} };|]
  , blankLine

  , comment "type hashes"
  , unlines (map typeHash types)
  , blankLine

  , comment "type encoders"
  , unlines (map typeEncoder types)
  , blankLine

  , comment "type decoders"
  , unlines (map typeDecoder types)
  , blankLine

  , chompNewline [i|
  #undef R
  #undef I
|]
  , blankLine
  ]
  where
    types = S.specTypes s
    ln = unpack $ S.specName s
    blankLine = "\n"
    {-
    n2declMap = let s' = S.specTypes s
                    d = map t2decl s'
                    n = fmap S.typeName s'
                in M.fromList $ zip n d
    luDecl n = fromMaybe (error $ "Invalid name: " ++ unpack n ++ ".")
                         (M.lookup n n2declMap)
    -}

typeHash :: S.SpType -> String
typeHash t =
  [i|  hashtype_t const TYPE_HASH_#{S.typeName t} = { #{hashToBytes (S.spHash t)} };|]


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
