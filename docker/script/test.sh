#!/bin/bash -eux

BIN_DIR=$1
TESTCASES_DIR=$2

for f in `ls "$TESTCASES_DIR"`; do
    echo "Testing ... $f"
    cat "$TESTCASES_DIR/$f" | "$BIN_DIR/main" "$BIN_DIR/kkc-engine-goto-tokyo.py"
done
