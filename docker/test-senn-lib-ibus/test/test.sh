#!/bin/bash -eux

CURR_DIR=$(cd $(dirname $0) && pwd)

DIFF=$(
    "$CURR_DIR/main" "$CURR_DIR/kkc-engine-goto-tokyo.py" | \
        grep "Sym:" | \
        diff - "$CURR_DIR/expected.txt"
    )

if [ $? -ne 0 ]; then
    echo $DIFF
else
    echo "ok"
fi
