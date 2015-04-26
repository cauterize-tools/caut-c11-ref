#include "greatest.h"
#include "lib/cauterize.h"

TEST identity(void) {
  uint8_t buffer[64];
  struct caut_encode_iter encode;
  struct caut_decode_iter decode;

#define ID_TYPE(TYPE, REPR, INIT) \
  { \
    caut_encode_iter_init(&encode, buffer, sizeof(buffer)); \
    caut_decode_iter_init(&decode, buffer, sizeof(buffer)); \
    REPR _x = (INIT); \
    ASSERT_EQ(caut_status_ok, __caut_encode_##TYPE(&encode, &_x)); \
    memset(&_x, 0, sizeof(_x)); \
    ASSERT_EQ(caut_status_ok, __caut_decode_##TYPE(&decode, &_x)); \
    ASSERT_EQm(#TYPE " isn't ID.", (INIT), _x); \
  }

  ID_TYPE(s8,  int8_t,  10);
  ID_TYPE(s16, int16_t, 11);
  ID_TYPE(s32, int32_t, 12);
  ID_TYPE(s64, int64_t, 13);

  ID_TYPE(u8,  uint8_t,  10);
  ID_TYPE(u16, uint16_t, 11);
  ID_TYPE(u32, uint32_t, 12);
  ID_TYPE(u64, uint64_t, 13);

  ID_TYPE(f32, float, 599.0);
  ID_TYPE(f64, double, 601.2);

  ID_TYPE(bool, bool, true);

#undef ID_TYPE

  PASS();
}

TEST iteration(void) {
  uint8_t buffer[64];
  struct caut_encode_iter encode;
  struct caut_decode_iter decode;

  /* Check that encodeing advances the iterator. */
#define ENCODE_ITER_TYPE(TYPE, REPR) \
  { \
    caut_encode_iter_init(&encode, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(sizeof(buffer), caut_encode_iter_remaining(&encode)); \
    ASSERT_EQ(caut_status_ok, __caut_encode_##TYPE(&encode, &_x)); \
    ASSERT_EQ(sizeof(buffer) - sizeof(_x), caut_encode_iter_remaining(&encode)); \
  }

  ENCODE_ITER_TYPE(s8,  int8_t);
  ENCODE_ITER_TYPE(s16, int16_t);
  ENCODE_ITER_TYPE(s32, int32_t);
  ENCODE_ITER_TYPE(s64, int64_t);

  ENCODE_ITER_TYPE(u8,  uint8_t);
  ENCODE_ITER_TYPE(u16, uint16_t);
  ENCODE_ITER_TYPE(u32, uint32_t);
  ENCODE_ITER_TYPE(u64, uint64_t);

  ENCODE_ITER_TYPE(f32, float);
  ENCODE_ITER_TYPE(f64, double);

  ENCODE_ITER_TYPE(bool, bool);

#undef ENCODE_ITER_TYPE

  /* Check that decodeing advances the iterator. */
#define DECODE_ITER_TYPE(TYPE, REPR) \
  { \
    caut_decode_iter_init(&decode, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(sizeof(buffer), caut_decode_iter_remaining(&decode)); \
    ASSERT_EQ(caut_status_ok, __caut_decode_##TYPE(&decode, &_x)); \
    ASSERT_EQ(sizeof(buffer) - sizeof(_x), caut_decode_iter_remaining(&decode)); \
  }

  DECODE_ITER_TYPE(s8,  int8_t);
  DECODE_ITER_TYPE(s16, int16_t);
  DECODE_ITER_TYPE(s32, int32_t);
  DECODE_ITER_TYPE(s64, int64_t);

  DECODE_ITER_TYPE(u8,  uint8_t);
  DECODE_ITER_TYPE(u16, uint16_t);
  DECODE_ITER_TYPE(u32, uint32_t);
  DECODE_ITER_TYPE(u64, uint64_t);

  DECODE_ITER_TYPE(f32, float);
  DECODE_ITER_TYPE(f64, double);

  DECODE_ITER_TYPE(bool, bool);

#undef DECODE_ITER_TYPE


  PASS();
}

TEST overflow(void) {
  struct caut_encode_iter encode;

#define ENCODE_OVERFLOW(TYPE, REPR) \
  { \
    uint8_t buffer[sizeof(REPR) - 1]; \
    caut_encode_iter_init(&encode, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(caut_status_would_overflow, __caut_encode_##TYPE(&encode, &_x)); \
    ASSERT_EQ(sizeof(buffer), caut_encode_iter_remaining(&encode)); \
  }

  ENCODE_OVERFLOW(s8,  int8_t);
  ENCODE_OVERFLOW(s16, int16_t);
  ENCODE_OVERFLOW(s32, int32_t);
  ENCODE_OVERFLOW(s64, int64_t);

  ENCODE_OVERFLOW(u8,  uint8_t);
  ENCODE_OVERFLOW(u16, uint16_t);
  ENCODE_OVERFLOW(u32, uint32_t);
  ENCODE_OVERFLOW(u64, uint64_t);

  ENCODE_OVERFLOW(f32, float);
  ENCODE_OVERFLOW(f64, double);

  ENCODE_OVERFLOW(bool, bool);

#undef ENCODE_OVERFLOW

  /* Ensure exact sizing still works. */
#define ENCODE_EXACT(TYPE, REPR) \
  { \
    uint8_t buffer[sizeof(REPR)]; \
    caut_encode_iter_init(&encode, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(caut_status_ok, __caut_encode_##TYPE(&encode, &_x)); \
    ASSERT_EQ(0, caut_encode_iter_remaining(&encode)); \
  }

  ENCODE_EXACT(s8,  int8_t);
  ENCODE_EXACT(s16, int16_t);
  ENCODE_EXACT(s32, int32_t);
  ENCODE_EXACT(s64, int64_t);

  ENCODE_EXACT(u8,  uint8_t);
  ENCODE_EXACT(u16, uint16_t);
  ENCODE_EXACT(u32, uint32_t);
  ENCODE_EXACT(u64, uint64_t);

  ENCODE_EXACT(f32, float);
  ENCODE_EXACT(f64, double);

  ENCODE_EXACT(bool, bool);

#undef ENCODE_EXACT

  PASS();
}

TEST underflow(void) {
  struct caut_decode_iter decode;

#define DECODE_UNDERFLOW(TYPE, REPR) \
  { \
    uint8_t buffer[sizeof(REPR) - 1]; \
    caut_decode_iter_init(&decode, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(caut_status_would_underflow, __caut_decode_##TYPE(&decode, &_x)); \
    ASSERT_EQ(sizeof(buffer), caut_decode_iter_remaining(&decode)); \
  }

  DECODE_UNDERFLOW(s8,  int8_t);
  DECODE_UNDERFLOW(s16, int16_t);
  DECODE_UNDERFLOW(s32, int32_t);
  DECODE_UNDERFLOW(s64, int64_t);

  DECODE_UNDERFLOW(u8,  uint8_t);
  DECODE_UNDERFLOW(u16, uint16_t);
  DECODE_UNDERFLOW(u32, uint32_t);
  DECODE_UNDERFLOW(u64, uint64_t);

  DECODE_UNDERFLOW(f32, float);
  DECODE_UNDERFLOW(f64, double);

  DECODE_UNDERFLOW(bool, bool);

#undef DECODE_UNDERFLOW

  /* Ensure exact sizing still works. */
#define DECODE_EXACT(TYPE, REPR) \
  { \
    uint8_t buffer[sizeof(REPR)]; \
    caut_decode_iter_init(&decode, buffer, sizeof(buffer)); \
    REPR _x; \
    ASSERT_EQ(caut_status_ok, __caut_decode_##TYPE(&decode, &_x)); \
    ASSERT_EQ(0, caut_decode_iter_remaining(&decode)); \
  }

  DECODE_EXACT(s8,  int8_t);
  DECODE_EXACT(s16, int16_t);
  DECODE_EXACT(s32, int32_t);
  DECODE_EXACT(s64, int64_t);

  DECODE_EXACT(u8,  uint8_t);
  DECODE_EXACT(u16, uint16_t);
  DECODE_EXACT(u32, uint32_t);
  DECODE_EXACT(u64, uint64_t);

  DECODE_EXACT(f32, float);
  DECODE_EXACT(f64, double);

  DECODE_EXACT(bool, bool);

#undef DECODE_EXACT

  PASS();
}

TEST null_bytes(void) {
  struct caut_encode_iter encode;
  struct caut_decode_iter decode;
  uint8_t buffer[81];

  caut_encode_iter_init(&encode, buffer, sizeof(buffer));
  caut_decode_iter_init(&decode, buffer, sizeof(buffer));

  /* Pack */
  ASSERT_EQ(caut_status_ok, __caut_encode_null_bytes(&encode, 40));
  ASSERT_EQ(sizeof(buffer) - 40, caut_encode_iter_remaining(&encode));
  ASSERT_EQ(caut_status_ok, __caut_encode_null_bytes(&encode, 40));
  ASSERT_EQ(sizeof(buffer) - 40 - 40, caut_encode_iter_remaining(&encode));

  ASSERT_EQ(caut_status_would_overflow, __caut_encode_null_bytes(&encode, 2));
  ASSERT_EQ(sizeof(buffer) - 40 - 40, caut_encode_iter_remaining(&encode));

  /* Unencode */
  ASSERT_EQ(caut_status_ok, __caut_decode_and_ignore_bytes(&decode, 40));
  ASSERT_EQ(sizeof(buffer) - 40, caut_decode_iter_remaining(&decode));
  ASSERT_EQ(caut_status_ok, __caut_decode_and_ignore_bytes(&decode, 40));
  ASSERT_EQ(sizeof(buffer) - 40 - 40, caut_decode_iter_remaining(&decode));

  ASSERT_EQ(caut_status_would_underflow, __caut_decode_and_ignore_bytes(&decode, 2));
  ASSERT_EQ(sizeof(buffer) - 40 - 40, caut_decode_iter_remaining(&decode));

  PASS();
}


SUITE(serializer_suite);

SUITE(serializer_suite) {
  RUN_TEST(identity);
  RUN_TEST(iteration);
  RUN_TEST(overflow);
  RUN_TEST(underflow);
  RUN_TEST(null_bytes);
}
