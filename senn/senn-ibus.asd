(asdf:defsystem :senn-ibus
  :serial t
  :pathname "src"
  :components
  ((:file "ibus/stateful-ime")
   (:file "ibus/server")
   (:file "ibus/im"))
  :depends-on (:senn-fcitx))
