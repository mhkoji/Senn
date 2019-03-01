(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/cl"
  :components
  ((:file "states/states")
   (:file "states/view")
   (:file "keys")
   (:file "im")
   (:file "server/server")
   (:file "server/ipc"))
  :depends-on (:senn))

