#!/bin/bash -eux

TAG=$(basename $(cd $(dirname $0) && pwd))
DIR=../../
docker build $DIR \
       -t $TAG \
       -f ./Dockerfile && \
docker run --rm -t $TAG
