{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.Decoders
  ( typeDecoder
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.Text.Lazy (unpack)
import Data.List (intercalate)
import qualified Cauterize.Common.Types as S
import qualified Cauterize.Specification as S

typeDecoder :: S.SpType -> String
typeDecoder t = chompNewline [i|
  R decode_#{name}(I * const _c_iter, #{decl} * const _c_obj) {
#{decoderBody t}
  }
|]
  where
    name = S.typeName t
    decl = t2decl t

decoderBody :: S.SpType -> String
decoderBody t = "    // DECODE"
