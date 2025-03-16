(asdf:defsystem :senn-win
  :serial t
  :pathname "src/win/"
  :components
  ((:file "keys")
   (:file "ime")
   (:file "history")
   (:file "stateful-ime")
   (:file "server")
   (:file "im/im")
   (:file "im/input-mode")
   (:file "im/process-input")
   (:file "im/can-process")
   (:file "im/toggle-input-mode"))
  :depends-on (:jsown
               :senn))
