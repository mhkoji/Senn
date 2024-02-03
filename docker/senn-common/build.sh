#!/bin/bash

DIR=../../
docker build $DIR -t senn-common -f Dockerfile && \
docker run --rm -v $PWD:/host --rm -t senn-common
