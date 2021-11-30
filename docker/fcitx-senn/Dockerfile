FROM ubuntu:18.04 AS kkc-builder

RUN apt update && apt install -y \
    build-essential \
    cmake \
    fcitx-libs-dev

RUN mkdir \
    /app \
    /output \
    /output-build

COPY . /app
COPY --from=menu-builder /output /app/senn/package/fcitx-senn/dep-menu
COPY --from=kkc-builder /output /app/senn/package/fcitx-senn/dep-kkc
COPY --from=ecl-builder /output /app/senn/package/fcitx-senn/dep-ecl
COPY --from=ecl-builder /usr/local/include/ecl /usr/local/include/ecl

RUN cd /app && \
    patch -p1 < /app/senn/patch/ecl.patch && \
    cd /output-build && \
    cmake -DCMAKE_BUILD_TYPE=Release /app/senn/package/fcitx-senn/ && \
    make && \
    make package && \
    cp *.deb /output/ && \
    echo "#!/bin/bash"        > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh \ && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
