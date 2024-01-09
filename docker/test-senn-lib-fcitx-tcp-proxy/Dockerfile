FROM ubuntu:22.04

RUN apt update && apt install -y \
    # g++
    build-essential \
    # asdf:make-build
    cl-alexandria \
    # asdf:make-build
    cl-babel \
    # sb-ext:save-lisp-and-die
    cl-bordeaux-threads \
    # asdf:make-build
    cl-yason \
    # asdf:make-build, g++, test.sh
    ecl \
    # test.sh
    python3 \
    # sb-ext:save-lisp-and-die
    sbcl && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p \
    /app \
    /build \
    /usr/lib/senn/fcitx/

COPY senn/src   /build/senn/src
COPY senn/*.asd /build/senn/
COPY senn-ipc   /build/senn-ipc

RUN sbcl \
        --eval '(require :asdf)' \
        --eval '(push #p"/build/senn/" asdf:*central-registry*)' \
        --eval '(push #p"/build/senn-ipc/" asdf:*central-registry*)' \
        --eval '(asdf:load-system :senn-bin-kkc-proxy)' \
        --eval "(sb-ext:save-lisp-and-die \"/usr/lib/senn/fcitx/kkc-proxy\" :executable t :toplevel #'senn.bin.kkc-proxy-unix:main)" && \
    ecl \
        -eval '(require :asdf)' \
        -eval '(push #p"/build/senn/" asdf:*central-registry*)' \
        -eval '(push #p"/build/senn-ipc/" asdf:*central-registry*)' \
        -eval "(asdf:make-build :senn-lib-fcitx-kkc-proxy :type :static-library :monolithic t :init-name \"init_senn\" :move-here #p\"/build/\" :prologue-code '(progn (require :sockets) (require :asdf)))"

COPY senn/src-cpp /build/senn/src-cpp
COPY third-party/picojson /build/third-party/picojson
COPY docker/test-senn-lib-fcitx/main.cpp /build/

RUN g++ \
        /build/main.cpp \
        /build/senn/src-cpp/fcitx/im/stateful_ime_ecl.cpp \
        /build/senn/src-cpp/fcitx/im/stateful_ime_proxy.cpp \
        /build/senn-lib-fcitx-kkc-proxy--all-systems.a \
        -I./build/senn/src-cpp \
        -I./build/third-party/picojson \
        -lecl \
        -o /app/main

COPY docker/script/kkc-engine-goto-tokyo.py docker/test-senn-lib-fcitx/test /app/

CMD ["/app/test.sh"]
