# Hachee Input Method

## Emacs

Add the code below to your .emacs file.

```lisp
(defvar hachee-elisp-dir
  "<path/to/Hachee>/src/input-method/emacs/elisp/")
(push hachee-elisp-dir load-path)
(load (expand-file-name "./leim-list.el" hachee-elisp-dir))
```

### Dump the Server (optional)

```
% <path/to/Hachee>/src/input-method/emacs/bin/dump
```

## Fcitx (WIP)

```
% mkdir <path/to/Hachee>/src/input-method/fcitx/build
% cd build
% cmake ..
% make && sudo make install
```
