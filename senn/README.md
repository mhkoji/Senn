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
% sudo apt install fcitx-libs-dev
% mkdir <path/to/Hachee>/senn/senn-fcitx/build
% cd build
% cmake ..
% make && sudo make install
```
