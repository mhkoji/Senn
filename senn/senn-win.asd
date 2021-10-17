(asdf:defsystem :senn-win
  :serial t
  :pathname "src/cl/win"
  :components
  ((:file "input-processor/keys")
   (:file "input-processor/states")
   (:file "input-processor/view")
   (:file "input-processor/input-processor")
   (:file "stateful-im")
   #+windows
   (:file "ipc/named-pipe")
   #+linux
   (:file "ipc/tcp"))
  :depends-on (:senn
               :jsown
               #+linux
               :usocket))
