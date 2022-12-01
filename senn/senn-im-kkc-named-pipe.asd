(asdf:defsystem :senn-im-kkc-named-pipe
  :serial t
  :pathname "src/im/kkc/"
  :components
  ((:file "request")
   (:file "named-pipe"))
  :depends-on (:senn-im-kkc
               :senn-ipc-named-pipe
               :babel
               :log4cl))
