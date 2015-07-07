{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibCFile.MessageInterface
  ( messageInterfaceFromSpec
  ) where

import Cauterize.C11Ref.Util
import Data.String.Interpolate
import Data.Text (unpack)
import qualified Cauterize.CommonTypes as C
import qualified Cauterize.Specification as S

messageInterfaceFromSpec :: S.Specification -> String
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
    #{lenDecl} length = 0;
    STATUS_CHECK(__caut_decode_#{lenBi}(_iter, &length));
    _header->length = length;
    STATUS_CHECK(__caut_decode_raw_bytes(_iter, _header->tag, sizeof(_header->tag)));

    return caut_status_ok;
  }

  R decode_message_#{ln}(DI * const _iter, struct message_header_#{ln} const * const _header, struct message_#{ln} * const _obj) {
    const struct caut_type_descriptor * desc = 0;

    for (size_t i = 0; i < ARR_LEN(type_descriptors); i++) {
      if (0 == memcmp(_header->tag, type_descriptors[i].hash, TYPE_TAG_WIDTH_#{ln})) {
        desc = &type_descriptors[i];
        _obj->_type = (enum type_index_#{ln})i;
        break;
      }
    }

    if (0 == desc) {
      return caut_status_invalid_tag;
    } else {
      STATUS_CHECK(desc->decode(_iter, &_obj->_data));
    }

    return caut_status_ok;
  }
|]
  where
    ln = unpack $ S.specName s
    lenWidth = (C.sizeMax . C.tagToSize . S.specLengthTag) s
    lenDecl = len2c lenWidth
    lenBi = len2tag lenWidth
