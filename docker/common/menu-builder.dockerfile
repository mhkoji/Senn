FROM ubuntu:18.04 AS menu-builder

RUN apt update && apt install -y \
    build-essential \
    libgtk-3-dev

COPY senn/src-cpp/gtk/menu /app

RUN mkdir /output && \
    cd /app && \
    g++ about.cpp -o about `pkg-config --cflags --libs gtk+-3.0` && \
    mv about /output/menu-about && \
    echo "#!/bin/bash"         > /app/cmd.sh && \
    echo "cp /output/* /host" >> /app/cmd.sh && \
    chmod +x /app/cmd.sh

CMD ["/app/cmd.sh"]
