FROM ubuntu:18.04

RUN apt update && apt install -y \
    build-essential \
    fcitx-libs-dev \
    libanthy-dev

RUN mkdir \
    /app \
    /output

COPY senn-kkc-engine/anthy     /app/senn-kkc-engine/anthy
COPY senn/third-party/picojson /app//senn/third-party/picojson

RUN cd /app/senn-kkc-engine/anthy && \
    make fcitx-engine && \
    mv ./fcitx-engine /output/kkc-engine

COPY docker/script/copy-output.sh /app
CMD ["/app/copy-output.sh"]
