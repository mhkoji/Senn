(asdf:defsystem :senn-win
  :serial t
  :pathname "src/cl/win"
  :components
  ((:file "input-processor/keys")
   (:file "input-processor/states")
   (:file "input-processor/transit")
   (:file "stateful-im")
   (:file "ipc"))
  :depends-on (:senn
               :jsown))
