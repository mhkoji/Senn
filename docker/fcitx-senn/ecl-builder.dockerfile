FROM ubuntu:18.04

RUN apt update && apt install -y \
    build-essential \
    wget \
    m4

RUN mkdir \
    /app \
    /app-build \
    /output \
    /output-build

RUN wget \
      https://common-lisp.net/project/ecl/static/files/release/ecl-21.2.1.tgz \
      --directory-prefix /app-build && \
    cd /app-build && \
    tar zxvf ecl-21.2.1.tgz && \
    cd ecl-21.2.1 && \
    ./configure && \
    make && \
    make install && \
    wget \
      https://beta.quicklisp.org/quicklisp.lisp \
      --directory-prefix /app-build && \
    ecl \
      -norc \
      -load /app-build/quicklisp.lisp \
      -eval "(quicklisp-quickstart:install)"

COPY . /app

RUN ecl \
      -load "/root/quicklisp/setup.lisp" \
      -eval '(push "/app/senn/" ql:*local-project-directories*)' \
      -eval '(push "/app/hachee/" ql:*local-project-directories*)' \
      -eval '(ql:quickload :senn-lib-fcitx)' \
      -eval '(asdf:make-build :senn-lib-fcitx :type :static-library :move-here #P"/output" :monolithic t :init-name "init_senn")' && \
    cp /usr/local/lib/libecl* /output && \
    echo "#!/bin/bash"         > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
