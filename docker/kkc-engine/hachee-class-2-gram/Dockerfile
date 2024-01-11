FROM ubuntu:18.04

RUN apt update && apt install -y \
    cl-alexandria \
    cl-bordeaux-threads \
    cl-cffi \
    cl-ppcre \
    cl-trivial-gray-streams \
    cl-yason \
    sbcl && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir \
    /build \
    /output

COPY hachee /build/hachee
COPY senn-ipc /build/senn-ipc
COPY senn-kkc-engine/hachee /build/engine
COPY docker/kkc-engine/hachee-class-2-gram/data /build/data

RUN sbcl \
      --dynamic-space-size 2048 \
      --noinform \
      --no-userinit \
      --no-sysinit \
      --non-interactive \
      --eval '(require :asdf)' \
      --eval '(push #p"/build/engine/" asdf:*central-registry*)' \
      --eval '(push #p"/build/hachee/" asdf:*central-registry*)' \
      --eval '(push #p"/build/senn-ipc/" asdf:*central-registry*)' \
      --eval '(asdf:load-system :senn-kkc-engine-hachee-class-2-gram)' \
      --eval '(senn-kkc-engine.hachee.engine.class-2-gram:set-kkc "/build/data/")' \
      --eval "(sb-ext:save-lisp-and-die \"/output/kkc-engine\" :toplevel #'senn-kkc-engine.hachee.engine.class-2-gram:main :executable t)"

COPY docker/script/copy-output.sh /build

CMD ["/build/copy-output.sh"]
