module Cauterize.C11Ref.Ctx where

import qualified Cauterize.Specification as Sp
import Data.Text.Lazy (unpack)

data Ctx = Ctx
  { libName :: String
  , hFile :: String
  , cFile :: String
  } deriving (Show)

ctxFromSpec :: Sp.Spec -> Ctx
ctxFromSpec s = Ctx
  { libName = n
  , hFile = n ++ ".h"
  , cFile = n ++ ".c"
  }
  where n = unpack $ Sp.specName s
