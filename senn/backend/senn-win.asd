(asdf:defsystem :senn-win
  :serial t
  :pathname "src/win"
  :components
  ((:file "keys")

   (:file "im/ime")
   (:file "im/process-input")
   (:file "im/can-process")
   (:file "stateful-ime")

   (:file "server/server")
   #+windows
   (:file "server/named-pipe")
   #+linux
   (:file "server/tcp"))
  :depends-on (:senn
               :jsown
               #+linux
               :usocket))
