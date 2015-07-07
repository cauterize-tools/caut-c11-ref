{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.TestClient
  ( testClientFromSpec
  ) where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)
import qualified Cauterize.Specification as S

testClientFromSpec :: S.Specification -> String
testClientFromSpec s = unindent [i|
  #ifndef TEST_CLIENT_INTERFACE_#{ln}
  #define TEST_CLIENT_INTERFACE_#{ln}

  #include "#{ln}.h"
  #include "#{ln}_message.h"

  #define MESSAGE_HEADER_T struct message_header_#{ln}
  #define MESSAGE_T struct message_#{ln}
  #define MESSAGE_OVERHEAD MESSAGE_OVERHEAD_#{ln}
  #define MESSAGE_MAX_SIZE MESSAGE_MAX_SIZE_#{ln}

  #define DECODE_HEADER decode_message_header_#{ln}

  #define DECODE_MESSAGE decode_message_#{ln}
  #define ENCODE_MESSAGE encode_message_#{ln}

  #endif /* TEST_CLIENT_INTERFACE_#{ln} */
|]
  where
    ln = unpack $ S.specName s
