FROM ubuntu:22.04

RUN apt update && apt install -y \
    ecl \
    # https://github.com/daewok/lisp-devel-docker/issues/4
    # Without netbase, the following error occurs during (quicklisp-quickstart:install).
    # > An error occurred during initialization:
    # > Protocol not found: "tcp".
    netbase \
    wget \
 && rm -rf /var/lib/apt/lists/*

RUN mkdir -p\
    /app \
    /app-build \
    /output

RUN wget \
      https://beta.quicklisp.org/quicklisp.lisp \
      --directory-prefix /app-build && \
    ecl \
      -norc \
      -load /app-build/quicklisp.lisp \
      -eval "(quicklisp-quickstart:install)"

COPY senn /app/senn

RUN ecl \
      -load "/root/quicklisp/setup.lisp" \
      -eval '(push "/app" ql:*local-project-directories*)' \
      -eval '(ql:quickload :senn-lib-ibus)' \
      -eval '(asdf:make-build :senn-lib-ibus :type :static-library :move-here #P"/output" :monolithic t :init-name "init_senn")'

COPY docker/script/copy-output.sh /app
CMD ["/app/copy-output.sh"]
