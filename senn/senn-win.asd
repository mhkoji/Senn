(asdf:defsystem :senn-win
  :serial t
  :pathname "src/"
  :components
  ((:file "im/kkc/hachee")
   (:module :win
    :pathname "win"
    :components
    ((:file "keys")
     (:file "im/ime")
     (:file "im/process-input")
     (:file "im/can-process")
     (:file "stateful-ime")
     (:file "server"))))
  :depends-on (:jsown
               :hachee-kkc-impl-lm
               :hachee-data
               :senn))

