FROM ubuntu:18.04

RUN apt update && apt install -y \
    build-essential \
    cdbs \
    cmake \
    devscripts \
    fcitx-libs-dev \
    libanthy-dev

RUN mkdir \
    /app \
    /output

COPY senn /app/senn
# COPY --from=menu-builder /output /app/senn/package/fcitx-senn/dep-menu
COPY --from=kkc-builder /output /app/senn/package/fcitx-senn/dep-kkc
COPY --from=ecl-builder /output /app/senn/package/fcitx-senn/dep-ecl
COPY --from=ecl-builder /usr/lib/senn/fcitx/ /usr/lib/senn/fcitx/

RUN ln -s /usr/lib/senn/fcitx/ecl/include/ecl /usr/local/include/ && \
    cd /app/senn/package/fcitx-senn && \
    debuild -b -us -uc && \
    mv ../*.deb /output

COPY docker/script/copy-output.sh /app
CMD ["/app/copy-output.sh"]
