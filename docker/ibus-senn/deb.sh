#!/bin/bash

set -eu

sbcl --noinform \
     --no-userinit \
     --no-sysinit \
     --non-interactive \
     --load "/root/quicklisp/setup.lisp" \
     --load "/app/senn/dists/fcitx-senn/backend/bin/server.lisp" \
     --eval "(sb-ext:save-lisp-and-die
              \"/app/senn/dists/fcitx-senn/backend/bin/server\"
              :executable t
              :toplevel #'senn.fcitx-senn.backend.bin:main)"

./autogen.sh

## Add nostrip because dh_strip removes the sbcl core of the server
DEB_BUILD_OPTIONS=nostrip debuild -us -uc -b

cp ../*.deb /output/
