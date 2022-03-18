FROM ubuntu:18.04

RUN apt update && apt install -y \
    wget \
    sbcl

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
COPY senn-kkc-engine/hachee /app/engine
COPY data/markov            /app/data

RUN sbcl \
      --dynamic-space-size 2048 \
      --noinform \
      --no-userinit \
       --no-sysinit \
      --non-interactive \
      --load "/root/quicklisp/setup.lisp" \
      --eval '(push "/app/engine/" ql:*local-project-directories*)' \
      --eval '(push "/app/hachee/" ql:*local-project-directories*)' \
      --eval '(ql:quickload :senn-kkc-engine-hachee-markov)' \
      --eval '(senn-kkc-engine.hachee.markov:set-kkc "/app/data/")' \
      --eval "(sb-ext:save-lisp-and-die \"/output/kkc-engine\" :toplevel #'senn-kkc-engine.hachee.markov:main :executable t)" && \
    echo "#!/bin/bash"         > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
