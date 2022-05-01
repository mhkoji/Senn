(asdf:defsystem :senn-lib-win
  :serial t
  :components
  ((:file "src/lib/win"))
  :depends-on (:log4cl
               :senn-win
	       :senn-im-kkc-named-pipe))
