module Cauterize.C11Ref.Generate where

import Cauterize.C11Ref.LibHFile
import Cauterize.C11Ref.LibCFile
import System.FilePath.Posix
import qualified Cauterize.Specification as Sp

generateDynamicFiles :: FilePath -> String -> Sp.Spec -> IO ()
generateDynamicFiles path baseName spec = do
  writeFile (path `combine` (baseName ++ ".h")) (hFileFromSpec spec)
  writeFile (path `combine` (baseName ++ ".c")) (cFileFromSpec spec)

