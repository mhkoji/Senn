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
    /output

COPY senn /app/senn
COPY --from=kkc-builder /output /app/senn/package/ibus-senn/dep-kkc
COPY --from=ecl-builder /output /app/senn/package/ibus-senn/dep-ecl
COPY --from=ecl-builder /usr/lib/senn/ibus/ /usr/lib/senn/ibus/

RUN ln -s /usr/lib/senn/ibus/ecl/include/ecl /usr/local/include/ && \
    cd /app/senn/package/ibus-senn && \
    touch NEWS README AUTHORS ChangeLog && \
    autoreconf -i && \
    debuild -us -uc -b && \
    mv /app/senn/package/*.deb /output/

COPY docker/script/copy-output.sh /app
CMD ["/app/copy-output.sh"]