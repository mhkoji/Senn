#!/bin/bash

DIR=../../
docker build $DIR -t senn -f ./Dockerfile && \
docker run --rm -v $PWD:/host -t senn
