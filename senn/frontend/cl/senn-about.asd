(asdf:defsystem :senn-about
  :serial t
  :pathname "src"
  :components
  ((:file "menu/about"))
  :depends-on (:cl-cffi-gtk))
