FROM ubuntu:18.04 AS menu-builder

RUN apt update && apt install -y \
    build-essential \
    qt5-default

COPY senn /app

RUN mkdir /output && \
    cd /app/src-cpp/gui/menu/about && \
    qmake && \
    make && \
    mv about /output/menu-about && \
    echo "#!/bin/bash"        > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh \ && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
