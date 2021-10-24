(asdf:defsystem :senn-win
  :serial t
  :pathname "src/cl/win"
  :components
  ((:file "keys")

   (:file "im/im")
   (:file "stateful-im/stateful-im")

   (:file "im/states")
   (:file "im/view")
   (:file "im/ime")

   #+windows
   (:file "stateful-im/named-pipe")
   #+linux
   (:file "stateful-im/tcp"))
  :depends-on (:senn
               :jsown
               #+linux
               :usocket))
