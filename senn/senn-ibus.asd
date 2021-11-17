(asdf:defsystem :senn-ibus
  :serial t
  :pathname "src/ibus"
  :components
  ((:file "stateful-ime")
   (:file "server"))
  :depends-on (:senn-fcitx))
