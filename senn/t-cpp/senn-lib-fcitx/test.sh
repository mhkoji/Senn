#!/bin/bash -eux

LIB=senn-lib-fcitx--all-systems.a
ENGINE=$(cd $(dirname $0)/../ && pwd)/kkc-engine-goto-tokyo.py

DIFF=`./main $ENGINE | grep "Sym:" | diff - expected.txt`
if [ $? -ne 0 ]; then
    echo $DIFF
fi
