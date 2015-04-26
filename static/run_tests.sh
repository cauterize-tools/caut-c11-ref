#!/bin/sh
clang -Wall -Wextra -Weverything \
      -Wno-padded \
      -Wno-float-equal \
      -Wno-zero-length-array \
      *.c lib/cauterize.c -o test && ./test
