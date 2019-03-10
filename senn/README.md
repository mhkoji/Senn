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
% cd <path/to/Hachee>/senn/senn-emacs/
% ros dump executable bin/server.ros -o bin/server
```

## Fcitx

### Install
```
% cd <path/to/Hachee>/senn-fcitx
% ros dump executable bin/server.ros -o bin/server
% sudo mkdir /usr/lib/senn
% sudo mv bin/server /usr/lib/senn

% sudo apt install fcitx-libs-dev
% mkdir <path/to/Hachee>/senn/senn-fcitx/build
% cd <path/to/Hachee>/senn/senn-fcitx/build
% cmake ..
% make && sudo make install
```

### Uninstall

To uninstall all the objects, you manually delete them.
Delete `/usr/lib/senn`, which you created explicitly.
The paths of the objects installed by `sudo make install` are found in the log messages.
```
...
Install the project...
-- Install configuration: ""
-- Up-to-date: /usr/share/fcitx/addon/fcitx-senn.conf
-- Up-to-date: /usr/share/fcitx/inputmethod/senn.conf
-- Installing: /usr/lib/x86_64-linux-gnu/fcitx/fcitx-senn.so
```

Thus, you can uninstall senn-fcitx by the following commands:

```
% sudo rm -rf /usr/lib/senn
% sudo rm -rf /usr/share/fcitx/addon/fcitx-senn.conf
% sudo rm -rf /usr/share/fcitx/inputmethod/senn.conf
% sudo rm -rf /usr/lib/x86_64-linux-gnu/fcitx/fcitx-senn.so
```

## Windows (WIP)

- Build tip
- Run `cmd.exe` as administrator and execute `C:\WINDOWS\system32>regsvr32.exe <path\to\Release\dir>\tip.dll`
