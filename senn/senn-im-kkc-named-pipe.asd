(asdf:defsystem :senn-im-kkc-named-pipe
  :serial t
  :components
  ((:file "src/im/kkc/request")
   (:file "src/im/kkc/named-pipe"))
  :depends-on (:senn-im-kkc
               :senn-ipc-named-pipe
               :babel
               :log4cl))
