module Cauterize.C11Ref.Generate where

import Cauterize.C11Ref.Ctx
import Cauterize.C11Ref.LibHFile
import Cauterize.C11Ref.LibCFile
import System.FilePath.Posix
import qualified Cauterize.Specification as Sp

generateDynamicFiles :: FilePath -> Ctx -> Sp.Spec -> IO ()
generateDynamicFiles path ctx spec = do
  writeFile (path `combine` hFile ctx) (hFileFromSpec spec)
  writeFile (path `combine` cFile ctx) (cFileFromSpec spec)

