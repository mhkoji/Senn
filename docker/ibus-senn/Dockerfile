FROM ubuntu:22.04

RUN apt update && apt install -y \
    # debuild
    build-essential \
    # debuild
    cdbs \
    # asdf:make-build
    cl-alexandria \
    # asdf:make-build
    cl-babel \
    # asdf:make-build
    cl-yason \
    # debuild
    devscripts \
    # asdf:make-build, debuild
    ecl \
    # debuild
    gnome-common \
    # debuild
    libibus-1.0-dev \
    # debuild
    picojson-dev && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p \
    /app \
    /build \
    /output

COPY senn /build/senn

RUN cd /build/senn/package/ibus-senn && \
    touch NEWS README AUTHORS ChangeLog && \
    mkdir m4 && \
    debuild -us -uc -b && \
    mv /build/senn/package/*.deb /output/

COPY docker/script/copy-output.sh /app

CMD ["/app/copy-output.sh"]
