#!/bin/bash

DIR=../../
docker build $DIR -t senn-common -f Dockerfile && \
docker run --rm -v $PWD:/host -t senn-common
