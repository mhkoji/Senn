# ibus-senn

## Install from Command Line

```
% sudo apt install -y gnome-common libibus-1.0-dev picojson-dev

% cd <path/to/senn>/senn/package/ibus-senn/
% touch NEWS README AUTHORS ChangeLog && mkdir m4
% autoreconf -i
% ./configure && make && sudo make install
% ibus-daemon -drx
```
