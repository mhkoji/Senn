#!/bin/bash

DIR=../../
docker build $DIR -t ibus-senn -f ./Dockerfile && \
docker run --rm -v $PWD:/host -t ibus-senn
