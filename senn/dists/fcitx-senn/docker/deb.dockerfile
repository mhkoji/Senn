FROM ubuntu:18.04

RUN apt update && apt install -y \
    ## for backend
    sbcl \
    wget \
    ## for frontend
    build-essential \
    cmake \
    fcitx-libs-dev

RUN mkdir \
    /app \
    /build \
    /output \
    /output-build

RUN cd /build && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load /build/quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)"

COPY . /app
RUN cd /root/quicklisp/local-projects && \
    ln -s /app/hachee/ && \
    ln -s /app/senn/ && \
    ## Download dependencies by ql:quickload
    sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load "/root/quicklisp/setup.lisp" \
         --eval "(ql:quickload :senn-fcitx)"

WORKDIR /output-build
ENTRYPOINT ["/app/senn/dists/fcitx-senn/docker/deb.sh"]
