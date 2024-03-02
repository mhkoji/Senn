# ibus-senn

## Install from Command Line

```
% sudo apt install -y libibus-1.0-dev picojson-dev

% cd <path/to/senn>/senn/package/ibus-senn/
% autoreconf -i
% ./configure && make && sudo make install
% ibus-daemon -drx
```
