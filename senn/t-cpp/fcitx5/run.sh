#!/bin/bash -eux

LIB=senn-lib-fcitx-kkc-proxy--all-systems.a

rm -f $LIB

ros use ecl

ros \
    -s senn-lib-fcitx-kkc-proxy \
    -e "(asdf:make-build :senn-lib-fcitx-kkc-proxy :type :static-library :monolithic t :init-name \"init_senn\" :move-here \"./\" :prologue-code '(progn (require :sockets) (require :asdf)))" \
    -q

g++ \
    `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --cflags` \
    -I../../../third-party \
    -I../../src-cpp \
    main.cpp \
    ../../src-cpp/fcitx/im/stateful_ime_ecl.cpp \
    ../../src-cpp/fcitx/im/stateful_ime_proxy.cpp \
    $LIB \
    `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --ldflags` \
    -lecl \
    -o main

DIFF=`./main | grep Sym: | diff - expected.txt`
if [ $? -ne 0 ]; then
    echo $DIFF
fi
