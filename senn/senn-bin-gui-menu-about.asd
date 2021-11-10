(asdf:defsystem :senn-bin-gui-menu-about
  :serial t
  :components
  ((:file "src/gui/menu/about")
   (:file "src/bin/gui-menu-about"))
  :depends-on (:cl-cffi-gtk))
