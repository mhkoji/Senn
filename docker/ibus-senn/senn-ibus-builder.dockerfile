FROM ubuntu:18.04

RUN apt update && apt install -y \
    wget \
    sbcl

RUN mkdir \
    /app \
    /app-build \
    /output \
    /output-build

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

COPY . /app

RUN sbcl \
      --noinform \
      --no-userinit \
      --no-sysinit \
      --non-interactive \
      --load "/root/quicklisp/setup.lisp" \
      --eval '(push "/app/senn/" ql:*local-project-directories*)' \
      --eval '(push "/app/hachee/" ql:*local-project-directories*)' \
      --eval '(ql:quickload :senn-bin-senn-ibus)' \
      --eval "(sb-ext:save-lisp-and-die \"/output/senn-ibus\" :toplevel #'senn.bin.senn-ibus:main :executable t)" && \
    echo "#!/bin/bash"         > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
