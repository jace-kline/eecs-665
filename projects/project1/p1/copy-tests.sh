#!/bin/bash
for i in $(seq 1 14); do cp tests/test$i/test.holyc test$i.holyc ; cp tests/test$i/test.out.expected test$i.out.expected ; cp tests/test$i/test.err.expected test$i.err.expected; done
