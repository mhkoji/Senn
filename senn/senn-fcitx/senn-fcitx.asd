(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/cl"
  :components
  ((:file "states/states")
   (:file "states/view")
   (:file "keys")
   (:file "im")
   (:file "stateful-im/stateful-im")
   (:file "stateful-im/ipc"))
  :depends-on (:senn))

