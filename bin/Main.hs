module Main where

import qualified Cauterize.Specification as Sp

import Cauterize.C11Ref.FileSystem
import Cauterize.C11Ref.Generate
import Cauterize.C11Ref.Options
import Cauterize.C11Ref.Static

import Data.Text.Lazy (unpack)

main :: IO ()
main = runWithOptions caut2c11

caut2c11 :: Caut2C11Opts -> IO ()
caut2c11 (Caut2C11Opts { specFile = sf, outputDirectory = od }) = createGuard od $ do
  spec <- loadSpec
  let baseName = unpack $ Sp.specName spec

  copyStaticFilesTo od
  generateDynamicFiles od baseName spec

  where

    loadSpec :: IO Sp.Spec
    loadSpec = do
      s <- Sp.parseFile sf
      case s of
        Left e -> error $ show e
        Right s' -> return s'
