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
## If the folloing error occurs, adding `:prologue-code '(require :asdf)` to asdf:make-build can resolve this.
## Condition of type: SIMPLE-ERROR
## The packages
##   ((UIOP/OS . #<UIOP/OS package>) (UIOP/PATHNAME . #<UIOP/PATHNAME package>))
## were referenced in compiled file
##   NIL
## but have not been created
        -eval '(asdf:make-build :senn-lib-fcitx :type :static-library :move-here #P"/build/" :monolithic t :init-name "init_senn")'

COPY senn/src-cpp /build/senn/src-cpp

RUN g++ \
        /build/senn/src-cpp/fcitx/im/stateful_ime_ecl.cpp \
        /build/senn/src-cpp/fcitx/im/stateful_ime_proxy.cpp \
        /build/senn/src-cpp/fcitx/im/test.cpp \
        /build/senn/src-cpp/fcitx/im/test_ime_ecl_main.cpp \
        /build/senn-lib-fcitx--all-systems.a \
        -I./build/senn/src-cpp \
        -lecl \
        -o /app/main

COPY docker/script/kkc-engine-goto-tokyo.py docker/test-senn-lib-fcitx/test /app/

CMD ["/app/test.sh"]
