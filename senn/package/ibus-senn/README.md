# ibus-senn

## Install via dpkg

### Build the Deb file

#### Docker

```
% cd <path/to/Hachee>
% docker build . -t ibus-deb -f senn/dists/ibus-senn/docker/deb.dockerfile
% docker run -v $PWD/output:/output -t ibus-deb
% ls output
ibus-senn_0.0.1_all.deb
```

## Install from Command Line

```
% sudo apt install -y gnome-common libibus-1.0-dev

% cd <path/to/senn>/senn/package/ibus-senn/
% mkdir dep-ecl dep-kkc
% ros use sbcl
% ros dump executable ../../src/bin/kkc-engine.ros
% mv ../../src/bin/kkc-engine ./dep-kkc

% ros run -e '(asdf:make-build :senn-lib-ibus :type :static-library :move-here #P"./dep-ecl" :monolithic t :init-name "init_senn")' -q
% cp -r ~/.roswell/impls/x86-64/linux/ecl/21.2.1/lib/ecl-21.2.1 ./dep-ecl
% cp ~/.roswell/impls/x86-64/linux/ecl/21.2.1/lib/libecl.so* ./dep-ecl

% ./autogen.sh
% make && sudo make install
% ibus-daemon -drx
```
