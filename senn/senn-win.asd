(asdf:defsystem :senn-win
  :serial t
  :pathname "src/cl/win"
  :components
  ((:file "keys")

   (:file "ime/ime")
   (:file "ime/process-input")
   (:file "ime/can-process")
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
