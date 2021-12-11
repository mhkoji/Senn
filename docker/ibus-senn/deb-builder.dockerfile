FROM ubuntu:18.04

RUN apt update && apt install -y \
    build-essential \
    gnome-common \
    libibus-1.0-dev \
    cdbs \
    devscripts

RUN mkdir -p \
    /app \
    /output \
    /usr/lib/senn/

COPY . /app
COPY --from=kkc-builder /output /app/senn/package/ibus-senn/dep-kkc
COPY --from=ecl-builder /output /app/senn/package/ibus-senn/dep-ecl
COPY --from=ecl-builder /usr/local/include/ecl /usr/local/include/ecl

## Add nostrip because dh_strip removes the sbcl core of the server
RUN cp /app/senn/package/ibus-senn/dep-ecl/libecl* /usr/lib/senn && \
    cd /app/senn/package/ibus-senn && \
    ./autogen.sh && \
    DEB_BUILD_OPTIONS=nostrip debuild -us -uc -b && \
    cp /app/senn/package/*.deb /output/ && \
    echo "#!/bin/bash"         > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
