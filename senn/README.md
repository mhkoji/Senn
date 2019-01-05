# Senn Input Method

Input method based on Hachee for the Japanese language

## Emacs

Add the code below to your .emacs file.

```lisp
(defvar senn-elisp-dir
  "<path/to/Hachee>/senn/senn-emacs/elisp/")
(push senn-elisp-dir load-path)
(load (expand-file-name "./leim-list.el" senn-elisp-dir))
```

### Dump the Server (optional)

```
% <path/to/Hachee>/senn/senn-emacs/bin/dump
```

## Fcitx (WIP)

```
% cd <path/to/Hachee>/senn-fcitx
% ros dump executable bin/server.ros -o bin/server
% sudo mkdir /usr/lib/senn
% sudo mv bin/server /usr/lib/senn
% /usr/lib/senn/server

% sudo apt install fcitx-libs-dev
% mkdir <path/to/Hachee>/senn/senn-fcitx/build
% cd <path/to/Hachee>/senn/senn-fcitx/build
% cmake ..
% make && sudo make install
```
