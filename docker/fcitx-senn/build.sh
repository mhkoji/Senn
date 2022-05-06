#!/bin/bash

DIR=../../
# docker build $DIR -t menu-builder \
#        -f ../common/menu-builder.dockerfile && \
docker build $DIR -t kkc-builder \
       -f ../kkc-engine/hachee-lm.dockerfile &&\
docker build $DIR -t ecl-builder \
       -f ./ecl.dockerfile && \
docker build $DIR -t deb-builder \
       -f ./deb.dockerfile  && \
docker run --rm -v $PWD:/host --rm -t deb-builder
