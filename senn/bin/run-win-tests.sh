#!/bin/bash

ros run \
    -e "(asdf:oos 'asdf:load-op :senn :force t)" \
    -e "(asdf:oos 'asdf:load-op :senn-win :force t)" \
    -e "(asdf:oos 'asdf:load-op :senn-win-test :force t)" \
    -e "(senn.t.win:run)" \
    -q
