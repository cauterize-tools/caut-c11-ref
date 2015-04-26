#include "greatest.h"
#include "lib/cauterize.h"

TEST encode_iter_init__can_be_initialized(void) {
  struct caut_encode_iter iter;
  uint8_t buffer[128];

  caut_encode_iter_init(&iter, buffer, ARR_LEN(buffer));

  ASSERT_EQ(buffer, iter.buffer);
  ASSERT_EQ(ARR_LEN(buffer), iter.length);
  ASSERT_EQ(0, iter.position);

  PASS();
}

TEST encode_iter_remaining__is_correct(void) {
  struct caut_encode_iter iter;
  uint8_t buffer[128];
  uint8_t x = 5;

  caut_encode_iter_init(&iter, buffer, ARR_LEN(buffer));
  ASSERT_EQ(sizeof(buffer), caut_encode_iter_remaining(&iter));

  ASSERT_EQ(caut_status_ok, __caut_encode_u8(&iter, &x));
  ASSERT_EQ(sizeof(buffer) - 1, caut_encode_iter_remaining(&iter));

  PASS();
}

TEST encode_iter_buffer__is_the_buffer(void) {
  struct caut_encode_iter iter;
  uint8_t buffer[128];

  caut_encode_iter_init(&iter, buffer, ARR_LEN(buffer));

  ASSERT_EQ(buffer, caut_encode_iter_buffer(&iter));

  PASS();
}

TEST decode_iter_init__can_be_initialized(void) {
  struct caut_decode_iter iter;
  uint8_t buffer[128];

  caut_decode_iter_init(&iter, buffer, ARR_LEN(buffer));

  ASSERT_EQ(buffer, iter.buffer);
  ASSERT_EQ(ARR_LEN(buffer), iter.length);
  ASSERT_EQ(0, iter.position);

  PASS();
}

TEST decode_iter_remaining__is_correct(void) {
  struct caut_decode_iter iter;
  uint8_t buffer[128];
  uint8_t x;

  caut_decode_iter_init(&iter, buffer, ARR_LEN(buffer));
  ASSERT_EQ(sizeof(buffer), caut_decode_iter_remaining(&iter));

  ASSERT_EQ(caut_status_ok, __caut_decode_u8(&iter, &x));
  ASSERT_EQ(sizeof(buffer) - 1, caut_decode_iter_remaining(&iter));

  PASS();
}

TEST decode_iter_buffer__is_the_buffer(void) {
  struct caut_decode_iter iter;
  uint8_t buffer[128];

  caut_decode_iter_init(&iter, buffer, ARR_LEN(buffer));
  ASSERT_EQ(buffer, caut_decode_iter_buffer(&iter));

  PASS();
}


SUITE(iterator_suite);

SUITE(iterator_suite) {
  RUN_TEST(encode_iter_init__can_be_initialized);
  RUN_TEST(encode_iter_remaining__is_correct);
  RUN_TEST(encode_iter_buffer__is_the_buffer);

  RUN_TEST(decode_iter_init__can_be_initialized);
  RUN_TEST(decode_iter_remaining__is_correct);
  RUN_TEST(decode_iter_buffer__is_the_buffer);
}
