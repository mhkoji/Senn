#!/bin/bash

DIR=../../
docker build $DIR -t ibus-senn -f ./Dockerfile && \
docker run --rm -v $PWD:/host --rm -t ibus-senn
