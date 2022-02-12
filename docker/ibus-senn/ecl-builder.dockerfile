FROM ubuntu:18.04

RUN apt update && apt install -y \
    build-essential \
    wget \
    m4

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

COPY . /app

RUN /usr/lib/senn/ibus/ecl/bin/ecl \
      -load "/root/quicklisp/setup.lisp" \
      -eval '(push "/app/senn/" ql:*local-project-directories*)' \
      -eval '(push "/app/hachee/" ql:*local-project-directories*)' \
      -eval '(ql:quickload :senn-lib-ibus)' \
      -eval '(asdf:make-build :senn-lib-ibus :type :static-library :move-here #P"/output" :monolithic t :init-name "init_senn")' && \
    cp    /usr/lib/senn/ibus/ecl/lib/libecl*    /output && \
    cp -r /usr/lib/senn/ibus/ecl/lib/ecl-21.2.1 /output && \
    echo "#!/bin/bash"         > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
