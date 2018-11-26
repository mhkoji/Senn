# Hachee KKC

## Emacs

Add the code below to your .emacs file.

```lisp
(defvar hachee-elisp-dir
  "<path/to/Hachee>/src/kkc/frontend/emacs/elisp/")
(push hachee-elisp-dir load-path)
(load (expand-file-name "./leim-list.el" hachee-elisp-dir))
```
