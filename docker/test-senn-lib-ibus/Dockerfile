FROM ubuntu:22.04

RUN apt update && apt install -y \
    # g++
    build-essential \
    # asdf:make-build
    cl-alexandria \
    # asdf:make-build
    cl-babel \
    # asdf:make-build
    cl-yason \
    # asdf:make-build, g++, test.sh
    ecl \
    # g++
    picojson-dev \
    # test.sh
    python3 && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p \
    /app \
    /build

COPY senn/src   /build/senn/src
COPY senn/*.asd /build/senn/

RUN ecl \
        -eval '(require :asdf)' \
        -eval '(push #p"/build/senn/" asdf:*central-registry*)' \
        -eval '(asdf:make-build :senn-lib-ibus :type :static-library :move-here #P"/build/" :monolithic t :init-name "init_senn")'

COPY senn/src-cpp /build/senn/src-cpp/
COPY docker/test-senn-lib-ibus/main.cpp /build/

RUN g++ \
        /build/main.cpp \
        /build/senn/src-cpp/ibus/im/stateful_ime_ecl.cpp \
        /build/senn/src-cpp/ibus/im/stateful_ime_proxy.cpp \
        /build/senn-lib-ibus--all-systems.a \
        -I./build/senn/src-cpp \
        -lecl \
        -o /app/main

COPY docker/script/kkc-engine-goto-tokyo.py docker/test-senn-lib-ibus/test /app/

CMD ["/app/test.sh"]
