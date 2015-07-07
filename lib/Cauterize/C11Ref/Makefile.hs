{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.Makefile
  ( makefileFromSpec
  ) where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)
import qualified Cauterize.Specification as S

makefileFromSpec :: S.Specification -> String
makefileFromSpec s = unindent [i|
  CC=clang
  CFLAGS=-O3 -Werror -Wall -Wextra -std=c11 -pedantic
  CLIENT_PROG=test_client

  CFILES := cauterize.c #{ln}.c #{ln}_message.c
  HFILES := cauterize.h #{ln}.h #{ln}_message.h

  all: $(CLIENT_PROG)
  clean:
  	rm $(CLIENT_PROG)

  $(CLIENT_PROG): $(HFILES) $(CFILES)
  	$(CC) $(CFLAGS) $(CFILES) test_client.c -o $@
|]
  where
    ln = unpack $ S.specName s
