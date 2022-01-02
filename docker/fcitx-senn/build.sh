#!/bin/bash

DIR=../../
docker build $DIR -f ../common/menu-builder.dockerfile -t menu-builder && \
docker build $DIR -f ../common/kkc-builder.dockerfile  -t kkc-builder  && \
docker build $DIR -f ./ecl-builder.dockerfile -t ecl-builder  && \
docker build $DIR -f ./deb-builder.dockerfile -t deb-builder  && \
docker run  -v $PWD:/host --rm -t deb-builder
