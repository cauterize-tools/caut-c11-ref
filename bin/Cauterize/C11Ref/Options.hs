module Cauterize.C11Ref.Options
  ( runWithOptions
  , Caut2C11Opts(..)
  ) where

import Data.Monoid         ((<>))
import Options.Applicative

runWithOptions :: (Caut2C11Opts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

data Caut2C11Opts = Caut2C11Opts
  { specFile        :: FilePath
  , outputDirectory :: FilePath
  } deriving (Show)

optParser :: Parser Caut2C11Opts
optParser = Caut2C11Opts
  <$> strOption
    ( long "spec"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize specification file."
    )
  <*> strOption
    ( long "output"
   <> metavar "DIRECTORY_PATH"
   <> help "Output Cauterize directory."
    )

options :: ParserInfo Caut2C11Opts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Process Cauterize schema files."
            )
