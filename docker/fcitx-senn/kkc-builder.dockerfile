FROM ubuntu:18.04 AS kkc-builder

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

COPY . /app

RUN sbcl \
      --noinform \
      --no-userinit \
      --no-sysinit \
      --non-interactive \
      --load "/root/quicklisp/setup.lisp" \
      --eval '(push "/app/senn/" ql:*local-project-directories*)' \
      --eval '(push "/app/hachee/" ql:*local-project-directories*)' \
      --eval '(ql:quickload :senn-bin-kkc-engine)' \
      --eval "(sb-ext:save-lisp-and-die \"/output/kkc-engine\" :toplevel #'senn.bin.kkc-engine:main :executable t)" && \
    echo "#!/bin/bash"         > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
