(asdf:defsystem :senn-kkc-named-pipe
  :serial t
  :pathname "src/"
  :components
  ((:file "request")
   (:file "named-pipe"))
  :depends-on (:senn-kkc
               :senn-ipc-named-pipe
               :babel
               :log4cl))
