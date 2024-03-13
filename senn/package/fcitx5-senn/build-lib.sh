#!/bin/bash -eux

CURR_DIR="$(cd $(dirname $0) && pwd)"

## Avoiding writing the same cache file at the same time by building ibus-senn parallelly.
ecl \
    -eval '(require :asdf)' \
    -eval '(setq asdf:*user-cache* (merge-pathnames "fcitx5-senn/" asdf:*user-cache*))' \
    -eval "(push #p\"${CURR_DIR}/../../\" asdf:*central-registry*)" \
    -eval "(asdf:make-build :senn-lib-fcitx :type :static-library :move-here #P\"${CURR_DIR}\" :monolithic t :init-name \"init_senn\")" \
    -eval '(quit)'
