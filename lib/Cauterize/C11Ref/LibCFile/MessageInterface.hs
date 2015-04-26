{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.MessageInterface
  ( messageInterfaceFromSpec
  ) where

import Cauterize.C11Ref.LibCFile.Encoders
import Cauterize.C11Ref.LibCFile.Decoders
import Cauterize.C11Ref.Util
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text.Lazy (unpack)
import Data.Word
import Numeric
import qualified Cauterize.FormHash as S
import qualified Cauterize.Specification as S

messageInterfaceFromSpec :: S.Spec -> String
messageInterfaceFromSpec s = chompNewline [i|
  R encode_message_#{ln}(EI * const _iter, struct message_#{ln} const * const _obj) {
    const struct caut_type_descriptor * const desc = &type_descriptors[_obj->_type];
    size_t _data_position = 0;
    #{lenDecl} _data_len = 0;
    void * len_ptr = 0;

    STATUS_CHECK(__caut_encode_reserve(_iter, LENGTH_WIDTH_#{ln}, &len_ptr));
    STATUS_CHECK(__caut_encode_raw_bytes(_iter, desc->hash, TYPE_TAG_WIDTH_#{ln}));
    _data_position = _iter->position;
    STATUS_CHECK(desc->encode(_iter, &_obj->_data));
    _data_len = (#{lenDecl})(_iter->position - _data_position);

    memmove(len_ptr, &_data_len, sizeof(_data_len));

    return caut_status_ok;
  }

  R decode_message_header_#{ln}(DI * const _iter, struct message_header_#{ln} * const _header) {
    return caut_status_would_underflow;
  }

  R decode_message_#{ln}(DI * const _iter, struct message_header_#{ln} const * const _header, struct message_#{ln} * const _obj) {
    return caut_status_would_underflow;
  }
|]
  where
    ln = unpack $ S.specName s
    lenDecl = len2c $ (S.unLengthTagWidth . S.specLengthTagWidth) s
