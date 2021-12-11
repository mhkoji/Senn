#!/bin/bash

DIR=../../
docker build $DIR -f ./senn-ibus-builder.dockerfile -t senn-ibus-builder && \
docker build $DIR -f ./kkc-builder.dockerfile -t kkc-builder && \
docker build $DIR -f ./deb-builder.dockerfile -t deb-builder && \
docker run -v $PWD:/host --rm -t deb-builder
