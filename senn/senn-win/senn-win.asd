(asdf:defsystem :senn-win
  :serial t
  :pathname "cl"
  :components
  ((:file "server/server")
   (:file "server/ipc"))
  :depends-on (:hachee))
