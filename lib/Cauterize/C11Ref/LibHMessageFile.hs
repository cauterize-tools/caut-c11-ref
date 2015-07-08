{-# LANGUAGE QuasiQuotes #-}
module Cauterize.C11Ref.LibHMessageFile
  ( hMessageFileFromSpec
  ) where

import Cauterize.C11Ref.Util

import Data.List (intercalate)
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text (unpack)
import qualified Cauterize.Specification as S
import qualified Cauterize.CommonTypes as C

hMessageFileFromSpec :: S.Specification -> String
hMessageFileFromSpec = unindent . concat . fromSpec

fromSpec :: S.Specification -> [String]
fromSpec s = [chompNewline [i|
  #ifndef #{guardSym}
  #define #{guardSym}

  #include "#{ln}.h"

  /* message interface */
  #define TYPE_TAG_WIDTH_#{ln} (#{S.specTypeLength s})
  #define LENGTH_WIDTH_#{ln} (#{C.sizeMax . C.tagToSize $ S.specLengthTag s})

  #define MESSAGE_OVERHEAD_#{ln} (TYPE_TAG_WIDTH_#{ln} + LENGTH_WIDTH_#{ln})
  #define MESSAGE_MAX_SIZE_#{ln} (MESSAGE_OVERHEAD_#{ln} + MAX_SIZE_#{ln})
  #define MESSAGE_MIN_SIZE_#{ln} (MESSAGE_OVERHEAD_#{ln} + MIN_SIZE_#{ln})

  /* type descriptors extern */
  typedef struct caut_type_descriptor caut_type_descriptors_#{ln}_t[NUM_TYPES_#{ln}];
  extern const caut_type_descriptors_#{ln}_t type_descriptors;

  struct message_header_#{ln} {
    size_t length;
    uint8_t tag[TYPE_TAG_WIDTH_#{ln}];
  };

  struct message_#{ln} {
    enum type_index_#{ln} _type;
#{unionDecl}
  };

  enum caut_status encode_message_#{ln}(
    struct caut_encode_iter * const _iter,
    struct message_#{ln} const * const _obj);

  enum caut_status decode_message_header_#{ln}(
    struct caut_decode_iter * const _iter,
    struct message_header_#{ln} * const _header);

  enum caut_status decode_message_#{ln}(
    struct caut_decode_iter * const _iter,
    struct message_header_#{ln} const * const _header,
    struct message_#{ln} * const _obj);

  #endif /* #{guardSym} */
|]
  ]
  where
    guardSym = [i|_CAUTERIZE_C11REF_#{ln}_MESSAGE_|]
    ln = unpack $ S.specName s
    types = S.specTypes s
    typeUnionFields = intercalate "\n" $ map (\t -> [i|      #{t2decl t} msg_#{ident2str $ S.typeName t};|]) types
    unionDecl =
      if length types <= 0
        then ""
        else [i|
    union {
#{typeUnionFields}
    } _data;
|]
