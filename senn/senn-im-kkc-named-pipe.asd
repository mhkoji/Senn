(asdf:defsystem :senn-im-kkc-named-pipe
  :serial t
  :pathname "src/im/"
  :components
  ((:file "kkc/request")
   (:file "kkc/named-pipe"))
  :depends-on (:babel
               :log4cl
	       :senn
	       :senn-ipc-named-pipe))
