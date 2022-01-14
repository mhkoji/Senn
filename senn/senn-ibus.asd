(asdf:defsystem :senn-ibus
  :serial t
  :pathname "src"
  :components
  ((:file "im/kkc/hachee")
   (:file "ibus/stateful-ime")
   (:file "ibus/server"))
  :depends-on (:hachee-kkc
               :senn-fcitx))
