(asdf:defsystem :senn-gui
  :serial t
  :pathname "src/cl/gui"
  :components
  ((:file "menu/about"))
  :depends-on (:cl-cffi-gtk))
