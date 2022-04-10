#!/bin/bash

rm -rf ./coverage-ibus && \
ros run \
    -e "(require :sb-cover)" \
    -e "(declaim (optimize sb-cover:store-coverage-data))" \
    -e "(asdf:oos 'asdf:load-op :senn-ibus :force t)" \
    -e "(asdf:oos 'asdf:load-op :senn-ibus-test :force t)" \
    -e "(senn.t.ibus:run)" \
    -e "(sb-cover:report \"coverage-ibus/\")" \
    -q
