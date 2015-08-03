#!/bin/sh

./.cabal-sandbox/bin/crucible tester \
  --build-cmd="../../dist/build/caut-c11-ref/caut-c11-ref  --spec=%s --output=c11" \
  --build-cmd="cd c11 && make" \
  --run-cmd="./c11/test_client" \
  --schema-count=1 \
  --instance-count=1000 \
  --type-count=10 \
  --enc-size=1024
