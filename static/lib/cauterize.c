#include "cauterize.h"

#include <string.h>

#define P struct caut_encode_iter
#define U struct caut_decode_iter

static void caut_encode_iter_advance(P * iter, size_t adv);
static void caut_decode_iter_advance(U * iter, size_t adv);

void caut_encode_iter_init(P * iter, void * buffer, size_t length) {
  CAUT_ASSERT(iter);
  CAUT_ASSERT(buffer);

  iter->buffer = buffer;
  iter->length = length;
  iter->position = 0;
}

void * caut_encode_iter_buffer(P * iter) {
  CAUT_ASSERT(iter);

  return iter->buffer;
}

size_t caut_encode_iter_remaining(P * iter) {
  CAUT_ASSERT(iter);
  CAUT_ASSERT(iter->position <= iter->length);

  return iter->length - iter->position;
}

static void caut_encode_iter_advance(P * iter, size_t adv) {
  CAUT_ASSERT(iter);

  size_t rem = caut_encode_iter_remaining(iter);

  if (rem >= adv) {
    iter->position += adv;
  }
}

void caut_decode_iter_init(U * iter, void * buffer, size_t length) {
  CAUT_ASSERT(iter);
  CAUT_ASSERT(buffer);

  iter->buffer = buffer;
  iter->length = length;
  iter->position = 0;
}

void * caut_decode_iter_buffer(U * iter) {
  CAUT_ASSERT(iter);

  return iter->buffer;
}
size_t caut_decode_iter_remaining(U * iter) {
  CAUT_ASSERT(iter);
  CAUT_ASSERT(iter->position <= iter->length);

  return iter->length - iter->position;
}

static void caut_decode_iter_advance(U * iter, size_t adv) {
  CAUT_ASSERT(iter);

  size_t rem = caut_decode_iter_remaining(iter);

  if (rem >= adv) {
    iter->position += adv;
  }
}

#define ITER_FOCUS_PTR(ITER) (&((ITER)->buffer[(ITER)->position]))

#define MEMMOVE_PACK(ITER, OBJ_PTR) \
  do { \
    if (caut_encode_iter_remaining(ITER) >= sizeof(*OBJ_PTR)) { \
      memmove(ITER_FOCUS_PTR(ITER), OBJ_PTR, sizeof(*OBJ_PTR)); \
      caut_encode_iter_advance(ITER, sizeof(*OBJ_PTR)); \
    } else { \
      return caut_status_would_overflow; \
    } \
    \
    return caut_status_ok; \
  } while (0)

#define MEMMOVE_UNPACK(ITER, OBJ_PTR) \
  do { \
    if (caut_decode_iter_remaining(ITER) >= sizeof(*OBJ_PTR)) { \
      memmove(OBJ_PTR, ITER_FOCUS_PTR(ITER), sizeof(*OBJ_PTR)); \
      caut_decode_iter_advance(ITER, sizeof(*OBJ_PTR)); \
    } else { \
      return caut_status_would_underflow; \
    } \
    \
    return caut_status_ok; \
  } while (0)

#define GENERIC_PACK(CAUT_TYPE, C_TYPE) \
enum caut_status __caut_encode_##CAUT_TYPE(P * const iter, C_TYPE const * const obj) { \
  CAUT_ASSERT(iter); \
  CAUT_ASSERT(obj); \
  MEMMOVE_PACK(iter, obj); \
}

#define GENERIC_UNPACK(CAUT_TYPE, C_TYPE) \
enum caut_status __caut_decode_##CAUT_TYPE(U * const iter, C_TYPE * const obj) { \
  CAUT_ASSERT(iter); \
  CAUT_ASSERT(obj); \
  MEMMOVE_UNPACK(iter, obj); \
}

GENERIC_PACK(s8,  int8_t)
GENERIC_PACK(s16, int16_t)
GENERIC_PACK(s32, int32_t)
GENERIC_PACK(s64, int64_t)

GENERIC_PACK(u8,  uint8_t)
GENERIC_PACK(u16, uint16_t)
GENERIC_PACK(u32, uint32_t)
GENERIC_PACK(u64, uint64_t)

GENERIC_PACK(f32, float)
GENERIC_PACK(f64, double)

enum caut_status __caut_encode_bool(P * const iter, bool const * const obj) {
  CAUT_ASSERT(iter);
  CAUT_ASSERT(obj);
  MEMMOVE_PACK(iter, (uint8_t*)obj);
}


GENERIC_UNPACK(s8,  int8_t)
GENERIC_UNPACK(s16, int16_t)
GENERIC_UNPACK(s32, int32_t)
GENERIC_UNPACK(s64, int64_t)

GENERIC_UNPACK(u8,  uint8_t)
GENERIC_UNPACK(u16, uint16_t)
GENERIC_UNPACK(u32, uint32_t)
GENERIC_UNPACK(u64, uint64_t)

GENERIC_UNPACK(f32, float)
GENERIC_UNPACK(f64, double)

enum caut_status __caut_decode_bool(U * const iter, bool * const obj) {
  CAUT_ASSERT(iter);
  CAUT_ASSERT(obj);
  MEMMOVE_UNPACK(iter, (uint8_t*)obj);
}


enum caut_status __caut_encode_null_bytes(struct caut_encode_iter * const iter, size_t count) {
  if (caut_encode_iter_remaining(iter) >= count) {
    memset(ITER_FOCUS_PTR(iter), 0, count);
    caut_encode_iter_advance(iter, count);
  } else {
    return caut_status_would_overflow;
  }

  return caut_status_ok;
}

enum caut_status __caut_encode_reserve(P * const iter, size_t reserve_bytes, void ** ptr) {
  if (reserve_bytes < caut_encode_iter_remaining(iter)) {
    caut_encode_iter_advance(iter, reserve_bytes);
    *ptr = ITER_FOCUS_PTR(iter);
    return caut_status_ok;
  } else {
    return caut_status_would_overflow;
  }
}

enum caut_status __caut_encode_raw_bytes(struct caut_encode_iter * const iter, uint8_t const * const bytes, size_t len) {
  if (caut_encode_iter_remaining(iter) >= len) {
    memmove(ITER_FOCUS_PTR(iter), bytes, len);
    caut_encode_iter_advance(iter, len);
  } else {
    return caut_status_would_overflow;
  }

  return caut_status_ok;
}

enum caut_status __caut_decode_and_ignore_bytes(struct caut_decode_iter * const iter, size_t count) {
  if (caut_decode_iter_remaining(iter) >= count) {
    caut_decode_iter_advance(iter, count);
  } else {
    return caut_status_would_underflow;
  }

  return caut_status_ok;
}

enum caut_status __caut_decode_raw_bytes(struct caut_decode_iter * const iter, uint8_t * const bytes, size_t len) {
  if (caut_decode_iter_remaining(iter) >= len) {
    memmove(bytes, ITER_FOCUS_PTR(iter), len);
    caut_decode_iter_advance(iter, len);
  } else {
    return caut_status_would_underflow;
  }

  return caut_status_ok;
}
