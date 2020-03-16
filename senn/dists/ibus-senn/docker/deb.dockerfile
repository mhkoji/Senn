FROM ubuntu:18.04

RUN apt update && apt install -y \
    wget \
    build-essential \
    sbcl \
    gnome-common \
    libibus-1.0-dev \
    cdbs \
    devscripts

RUN mkdir \
    /app \
    /build \
    /output

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

WORKDIR /app/senn/dists/ibus-senn/frontend/
CMD ["/app/senn/dists/ibus-senn/docker/deb.sh"]
