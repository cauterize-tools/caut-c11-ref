#!/bin/sh

stack exec crucible -- tester \
  --build-cmd="stack exec caut-c11-ref -- --spec=%s --output=c11" \
  --build-cmd="cd c11 && make" \
  --run-cmd="./c11/test_client" \
  --schema-count=1 \
  --instance-count=1000 \
  --type-count=10 \
  --enc-size=1024
