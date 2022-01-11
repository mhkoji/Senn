FROM ubuntu:18.04 AS kkc-builder

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
    make && \
    cp ./engine /output/kkc-engine && \
    echo "#!/bin/bash"         > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
