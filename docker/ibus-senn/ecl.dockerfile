FROM ubuntu:18.04

RUN apt update && apt install -y \
    build-essential \
    wget \
    m4 \
 && rm -rf /var/lib/apt/lists/*

RUN mkdir -p\
    /app \
    /app-build \
    /output

RUN wget \
      https://common-lisp.net/project/ecl/static/files/release/ecl-21.2.1.tgz \
      --directory-prefix /app-build && \
    cd /app-build && \
    tar zxvf ecl-21.2.1.tgz && \
    cd ecl-21.2.1 && \
    ./configure --prefix=/usr/lib/senn/ibus/ecl/ && \
    make && \
    make install && \
    wget \
      https://beta.quicklisp.org/quicklisp.lisp \
      --directory-prefix /app-build && \
    /usr/lib/senn/ibus/ecl/bin/ecl \
      -norc \
      -load /app-build/quicklisp.lisp \
      -eval "(quicklisp-quickstart:install)"

COPY senn /app/senn

RUN /usr/lib/senn/ibus/ecl/bin/ecl \
      -load "/root/quicklisp/setup.lisp" \
      -eval '(push "/app" ql:*local-project-directories*)' \
      -eval '(ql:quickload :senn-lib-ibus)' \
      -eval '(asdf:make-build :senn-lib-ibus :type :static-library :move-here #P"/output" :monolithic t :init-name "init_senn")'

COPY docker/script/copy-output.sh /app
CMD ["/app/copy-output.sh"]
