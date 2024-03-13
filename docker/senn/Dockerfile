FROM ubuntu:22.04

RUN apt update && apt install -y \
      build-essential \
      cmake \
      debhelper \
      devscripts \
      # senn-common, ibus-senn
      cl-alexandria \
      cl-yason \
      # fcitx5-senn
      libfcitx5core-dev \
      # ibus-senn
      cl-babel \
      ecl \
      libibus-1.0-dev \
      picojson-dev \
      # senn-common
      cl-bordeaux-threads \
      cl-cffi \
      cl-ppcre \
      cl-trivial-gray-streams \
      sbcl && \
  rm -rf /var/lib/apt/lists/*

COPY . /Senn/

RUN cd /Senn && \
    debuild -us -uc -b && \
    mkdir -p /output && \
    mv /*.deb /output/

CMD ["/Senn/docker/script/copy-output.sh"]
