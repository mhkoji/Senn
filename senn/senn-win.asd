(asdf:defsystem :senn-win
  :serial t
  :pathname "src/win/"
  :components
  ((:file "keys")
   (:file "im/ime")
   (:file "im/input-mode")
   (:file "im/process-input")
   (:file "im/can-process")
   (:file "im/toggle-input-mode")
   (:file "history")
   (:file "stateful-ime")
   (:file "server"))
  :depends-on (:jsown
               :senn))
