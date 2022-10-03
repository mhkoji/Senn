FROM ubuntu:22.04

RUN apt update && apt install -y \
    build-essential \
    cdbs \
    cmake \
    devscripts \
    ecl \
    libfcitx5core-dev \
 && rm -rf /var/lib/apt/lists/*

RUN mkdir \
    /app \
    /output

COPY senn /app/senn
COPY third-party /app/third-party
COPY senn-kkc-engine/hachee/src-cpp /app/senn-kkc-engine/hachee/src-cpp
COPY --from=kkc-builder /output /app/senn/package/fcitx5-senn/dep-kkc
COPY --from=ecl-builder /output /app/senn/package/fcitx5-senn/dep-ecl

RUN cd /app/senn/package/fcitx5-senn && \
    debuild -b -us -uc && \
    mv ../*.deb /output

COPY docker/script/copy-output.sh /app
CMD ["/app/copy-output.sh"]
