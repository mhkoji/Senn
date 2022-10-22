#!/bin/bash -eux

rm -f senn-lib-fcitx--all-systems.a

ros \
    -s senn-lib-fcitx \
    -e '(asdf:make-build :senn-lib-fcitx :type :static-library :monolithic t :init-name "init_senn" :move-here "./")' \
    -q

g++ \
    `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --cflags` \
    -I../../../third-party \
    -I../../src-cpp \
    main.cpp \
    ../../src-cpp/fcitx/im/stateful_ime_ecl.cpp \
    ../../src-cpp/fcitx/im/stateful_ime_proxy.cpp \
    senn-lib-fcitx--all-systems.a \
    `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --ldflags` \
    -lecl \
    -o main

DIFF=`./main | grep Sym: | diff - expected.txt`
if [ $? -ne 0 ]; then
    echo $DIFF
fi
