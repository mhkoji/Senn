#!/bin/bash

DIR=../../
docker build $DIR -t kkc-builder \
       -f ../kkc-engine/hachee-lm.dockerfile && \
docker build $DIR -t deb-builder \
       -f ./Dockerfile && \
docker run --rm -v $PWD:/host --rm -t deb-builder
