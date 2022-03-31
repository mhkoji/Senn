(asdf:defsystem :senn-ibus
  :serial t
  :pathname "src"
  :components
  ((:file "ibus/stateful-ime")
   (:file "ibus/server"))
  :depends-on (:senn-fcitx))
