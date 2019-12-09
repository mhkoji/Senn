(asdf:defsystem :senn-win
  :serial t
  :pathname "cl"
  :components
  ((:file "transit/keys")
   (:file "transit/states")
   (:file "transit/transit")
   (:file "stateful-im/stateful-im")
   (:file "stateful-im/ipc"))
  :depends-on (:senn
               :jsown))
