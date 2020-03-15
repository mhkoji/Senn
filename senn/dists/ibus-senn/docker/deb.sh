#!/bin/bash

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

./autogen.sh && make

debuild -us -uc -b && cp ../*.deb /output/

