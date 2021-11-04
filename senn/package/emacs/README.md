# Senn for Emacs

Add the code below to your .emacs file.

```lisp
(defvar senn-elisp-dir
  "<path/to/Hachee>/senn/dists/emacs/elisp/")
(push senn-elisp-dir load-path)
(load (expand-file-name "./leim-list.el" senn-elisp-dir))
```

### Dump the Server (optional)

```
% cd <path/to/Hachee>/senn/dists/emacs/
% ros dump executable bin/server.ros -o bin/server
```

