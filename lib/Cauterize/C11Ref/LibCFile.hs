{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile
  ( cFileFromSpec
  ) where

import Cauterize.C11Ref.LibCFile.Encoders
import Cauterize.C11Ref.LibCFile.Decoders
import Cauterize.C11Ref.LibCFile.Initializers
import Cauterize.C11Ref.LibCFile.Comparators
import Cauterize.C11Ref.Util
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)
import Data.Word
import Data.Maybe
import Numeric
import qualified Data.Map as M
import qualified Cauterize.Hash as H
import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C

cFileFromSpec :: S.Specification -> String
cFileFromSpec = unindent . concat . fromSpec

fromSpec :: S.Specification -> [String]
fromSpec s = [chompNewline [i|
  #include "#{ln}.h"

  #define R enum caut_status
  #define EI struct caut_encode_iter
  #define DI struct caut_decode_iter
|]
  , comment "schema hash"
  , [i|  hashtype_t const SCHEMA_HASH_#{ln} = { #{hashToBytes (S.specFingerprint s)} };|]
  , blankLine

  , comment "type encoders"
  , unlines (map (typeEncoder ident2decl) types)
  , blankLine

  , comment "type decoders"
  , unlines (map (typeDecoder ident2decl) types)
  , blankLine

  , comment "type initializers"
  , unlines (map (typeInit ident2decl) types)
  , blankLine

  , comment "type comparators"
  , unlines (map (typeCompare ident2decl) types)
  , blankLine

  -- TODO: order_

  , chompNewline [i|
  #undef R
  #undef I
|]
  , blankLine
  ]
  where
    tDeclMap = fmap t2decl (S.specTypeMap s)

    primDec :: C.Identifier -> String
    primDec ident = fromMaybe (tyDec ident) (ident `M.lookup` primDeclMap)

    tyDec :: C.Identifier -> String
    tyDec ident = fromMaybe
                    (error $ "fromSpec: could not find type " ++ (unpack . C.unIdentifier) ident ++ " in specification or primitives")
                    (ident `M.lookup` tDeclMap)

    -- translate an identifier to its declaration
    ident2decl = primDec

    types = S.specTypes s
    ln = unpack $ S.specName s
    blankLine = "\n"

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
