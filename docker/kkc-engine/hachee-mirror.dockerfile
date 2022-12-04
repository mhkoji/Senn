FROM ubuntu:18.04

RUN apt update && apt install -y \
    wget \
    sbcl \
 && rm -rf /var/lib/apt/lists/*

RUN mkdir \
    /app \
    /app-build \
    /output

RUN wget \
      https://beta.quicklisp.org/quicklisp.lisp \
      --directory-prefix /app-build && \
    sbcl \
      --noinform \
      --no-userinit \
      --no-sysinit \
      --non-interactive \
      --load /app-build/quicklisp.lisp \
      --eval "(quicklisp-quickstart:install)"

COPY hachee                 /app/hachee
COPY senn-ipc               /app/senn-ipc
COPY senn-kkc-engine/hachee /app/engine
COPY data/mirror            /app/data

RUN sbcl \
      --dynamic-space-size 2048 \
      --noinform \
      --no-userinit \
       --no-sysinit \
      --non-interactive \
      --load "/root/quicklisp/setup.lisp" \
      --eval '(push "/app/engine/" ql:*local-project-directories*)' \
      --eval '(push "/app/hachee/" ql:*local-project-directories*)' \
      --eval '(push "/app/senn-ipc/" ql:*local-project-directories*)' \
      --eval '(ql:quickload :senn-kkc-engine-hachee-mirror)' \
      --eval '(senn-kkc-engine.hachee.engine.mirror:set-kkc "/app/data/")' \
      --eval "(sb-ext:save-lisp-and-die \"/output/kkc-engine\" :toplevel #'senn-kkc-engine.hachee.engine.mirror:main :executable t)"

COPY docker/script/copy-output.sh /app
CMD ["/app/copy-output.sh"]
