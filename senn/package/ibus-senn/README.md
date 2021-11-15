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
% /usr/lib/senn/server

% sudo apt install -y gnome-common libibus-1.0-dev
% cd <path/to/Hachee>/dists/ibus-senn/frontend
% ./autogen.sh
% make && sudo make install
% ibus-daemon -drx
```

## Debug info

Connect to a server listening on port 5678:

```
% /usr/lib/ibus/ibus-engine-senn --backend-commm connect
```
