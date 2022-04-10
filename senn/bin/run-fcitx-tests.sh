#!/bin/bash

rm -rf ./coverage && \
ros run \
    -e "(require :sb-cover)" \
    -e "(declaim (optimize sb-cover:store-coverage-data))" \
    -e "(asdf:oos 'asdf:load-op :senn :force t)" \
    -e "(asdf:oos 'asdf:load-op :senn-fcitx :force t)" \
    -e "(asdf:oos 'asdf:load-op :senn-fcitx-test :force t)" \
    -e "(senn.t.fcitx:run)" \
    -e "(sb-cover:report \"coverage/\")" \
    -q
