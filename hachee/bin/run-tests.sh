#!/bin/bash

rm -rf ./coverage && \
ros run \
    -e "(require :sb-cover)" \
    -e "(declaim (optimize sb-cover:store-coverage-data))" \
    -e "(asdf:oos 'asdf:load-op :hachee-test :force t)" \
    -e "(hachee.t.main:run)" \
    -e "(sb-cover:report \"coverage/\")" \
    -q
