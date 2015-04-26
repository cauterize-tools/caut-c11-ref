{-# LANGUAGE TemplateHaskell #-}
module Cauterize.C11Ref.Static
  ( allFiles
  ) where

import Data.FileEmbed
import qualified Data.ByteString as B

cauterizeDotH :: (FilePath, B.ByteString)
cauterizeDotH = ("cauterize.h", $(embedFile "static/lib/cauterize.h"))

cauterizeDotC :: (FilePath, B.ByteString)
cauterizeDotC = ("cauterize.c", $(embedFile "static/lib/cauterize.c"))

allFiles :: [(FilePath, B.ByteString)]
allFiles =
  [ cauterizeDotH
  , cauterizeDotC
  ]
