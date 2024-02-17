FROM ubuntu:22.04

RUN apt update && DEBIAN_FRONTEND=noninteractive apt install -y \
    build-essential \
    cdbs \
    cl-alexandria \
    cl-bordeaux-threads \
    cl-cffi \
    cl-ppcre \
    cl-trivial-gray-streams \
    cl-yason \
    devscripts \
    sbcl && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p \
    /app \
    /build \
    /output

COPY hachee /build/hachee
COPY senn-ipc /build/senn-ipc
COPY senn-common /build/senn-common
COPY package/senn-common /build/package/senn-common

RUN cd /build/package/senn-common && \
    touch NEWS README AUTHORS ChangeLog && \
    mkdir m4 && \
    debuild -us -uc -b && \
    mv /build/package/*.deb /output/

COPY docker/script/copy-output.sh /app

CMD ["/app/copy-output.sh"]
