(asdf:defsystem :senn-win
  :serial t
  :pathname "src/cl/win"
  :components
  ((:file "keys")

   (:file "stateful-im/stateful-im")
   (:file "ipc/ipc")

   (:file "stateful-im/states")
   (:file "stateful-im/view")
   (:file "stateful-im/im")

   #+windows
   (:file "ipc/named-pipe")
   #+linux
   (:file "ipc/tcp"))
  :depends-on (:senn
               :jsown
               #+linux
               :usocket))
