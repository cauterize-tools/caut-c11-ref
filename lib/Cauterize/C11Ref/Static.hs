{-# LANGUAGE TemplateHaskell #-}
module Cauterize.C11Ref.Static where

import Data.FileEmbed
import System.FilePath.Posix
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

copyStaticFilesTo :: FilePath -> IO ()
copyStaticFilesTo path = mapM_ go allFiles
  where
    go (p, d) = B.writeFile (path `combine` p) d
