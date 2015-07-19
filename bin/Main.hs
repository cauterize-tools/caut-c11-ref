module Main where

import qualified Cauterize.Specification as Sp

import Cauterize.C11Ref.Options
import Cauterize.C11Ref.Static
import Cauterize.C11Ref.LibHFile
import Cauterize.C11Ref.LibHMessageFile
import Cauterize.C11Ref.LibCFile
import Cauterize.C11Ref.LibCMessageFile
import Cauterize.C11Ref.TestClient
import Cauterize.C11Ref.Makefile

import Data.Text (unpack)
import System.Directory
import System.FilePath.Posix
import qualified Data.ByteString as B

main :: IO ()
main = runWithOptions caut2c11

caut2c11 :: Caut2C11Opts -> IO ()
caut2c11 (Caut2C11Opts { specFile = sf, outputDirectory = od }) = createGuard od $ do
  spec <- loadSpec
  let baseName = unpack $ Sp.specName spec

  print spec

  copyStaticFilesTo od
  generateDynamicFiles od baseName spec
  where

    loadSpec :: IO Sp.Specification
    loadSpec = do
      s <- Sp.parseSpecificationFromFile sf
      case s of
        Left e -> error $ show e
        Right s' -> return s'

createGuard :: FilePath -> IO () -> IO ()
createGuard out go = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out

  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else if de
          then go
          else createDirectory out >> go

copyStaticFilesTo :: FilePath -> IO ()
copyStaticFilesTo path = mapM_ go allFiles
  where
    go (p, d) = B.writeFile (path `combine` p) d

generateDynamicFiles :: FilePath -> String -> Sp.Specification -> IO ()
generateDynamicFiles path baseName spec = do
  writeFile (path `combine` (baseName ++ ".h")) (hFileFromSpec spec)
  writeFile (path `combine` (baseName ++ ".c")) (cFileFromSpec spec)
  writeFile (path `combine` (baseName ++ "_message.h")) (hMessageFileFromSpec spec)
  writeFile (path `combine` (baseName ++ "_message.c")) (cMessageFileFromSpec spec)
  writeFile (path `combine` "test_client.h")  (testClientFromSpec spec)
  writeFile (path `combine` "Makefile")  (makefileFromSpec spec)
