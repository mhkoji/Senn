FROM ubuntu:18.04

RUN apt update && apt install -y \
    build-essential \
    cdbs \
    devscripts \
    gnome-common \
    libanthy-dev \
    libibus-1.0-dev

RUN mkdir -p \
    /app \
    /output \
    /usr/lib/senn/ibus/ecl/lib/

COPY . /app
COPY --from=kkc-builder /output /app/senn/package/ibus-senn/dep-kkc
COPY --from=ecl-builder /output /app/senn/package/ibus-senn/dep-ecl
COPY --from=ecl-builder /usr/lib/senn/ibus/ecl/include/ecl /usr/local/include/ecl

## Add nostrip because dh_strip removes the sbcl core of the server
RUN cp /app/senn/package/ibus-senn/dep-ecl/libecl* /usr/lib/senn && \
    cd /app/senn/package/ibus-senn && \
    touch NEWS README AUTHORS ChangeLog && \
    autoreconf -i && \
    debuild -us -uc -b && \
    cp /app/senn/package/*.deb /output/ && \
    echo "#!/bin/bash"         > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
