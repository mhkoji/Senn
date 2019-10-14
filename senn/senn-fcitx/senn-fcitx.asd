(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/cl"
  :components
  ((:file "transit/states")
   (:file "transit/keys")
   (:file "transit/transit")
   (:file "stateful-im/stateful-im")
   (:file "stateful-im/ipc"))
  :depends-on (:senn))

