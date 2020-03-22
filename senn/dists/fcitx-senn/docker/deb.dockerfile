## Build the executable file for menu.
FROM electronuserland/builder:12 as build-menu

RUN mkdir /app \
          /output

COPY ./senn/menu/about /app

RUN cd /app && \
    yarn install && \
    npm run build && \
    cp ./dist/*.AppImage /output/menu


## Build the environment for building a deb package.
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

## Deb builder copys the executable menu file here.
COPY --from=build-menu /output/menu /app/senn/bin/

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
