# fcitx-senn

Roswell needs to be installed first.

## Setup

```
% sudo apt install -y fcitx-libs-dev cmake

% sudo mkdir -p /usr/lib/senn/fcitx/ecl
% wget https://common-lisp.net/project/ecl/static/files/release/ecl-21.2.1.tgz
% tar xvf ecl-21.2.1.tgz
% cd ecl-21.2.1/
% ./configure --prefix=/usr/lib/senn/fcitx/ecl && make && sudo make install
% sudo ln -s /usr/lib/senn/fcitx/ecl/include/ecl/ /usr/local/include/
```

## Install by CMake

```
% cd <path/to/senn>/senn/package/fcitx-senn/
% mkdir build
% cd build
% cmake -DCMAKE_BUILD_TYPE=Release .. && make
% sudo make install
```

### Uninstall

Manually delete the objects installed by `sudo make install` as well as `/usr/lib/senn`, which you created explicitly.
The paths of the installed objects are found in the log messages.

```
...
Install the project...
-- Install configuration: ""
-- Up-to-date: /usr/share/fcitx/addon/fcitx-senn.conf
-- Up-to-date: /usr/share/fcitx/inputmethod/senn.conf
-- Installing: /usr/lib/x86_64-linux-gnu/fcitx/fcitx-senn.so
```

Thus, you can uninstall fcitx-senn by the following commands:

```
% sudo rm -rf /usr/lib/senn
% sudo rm -rf /usr/share/fcitx/addon/fcitx-senn.conf
% sudo rm -rf /usr/share/fcitx/inputmethod/senn.conf
% sudo rm -rf /usr/lib/x86_64-linux-gnu/fcitx/fcitx-senn.so
```

## Install by Building Deb Package

The debian directory is created by `dh_make --createorig -p fcitx-senn_0.0.1`.

Add `DEB_BUILD_OPTIONS=nostrip` so that the sbcl core of kkc-engine survives strip by dh_strip.
https://bugs.launchpad.net/sbcl/+bug/310108

```
% cd <path/to/senn>/senn/package/fcitx-senn/
% DEB_BUILD_OPTIONS=nostrip debuild -b -us -uc
% sudo dpkg -i ../fcitx-senn_0.0.1-1_amd64.deb
```
