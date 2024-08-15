#!/bin/bash -eux

CURR_DIR=$(cd $(dirname $0) && pwd)
for f in `ls "$CURR_DIR/testcases"`; do
    echo "Testing ... $f"
    cat "$CURR_DIR/testcases/$f" | "$CURR_DIR/main" "$CURR_DIR/kkc-engine-goto-tokyo.py"
done
