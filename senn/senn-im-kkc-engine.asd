(asdf:defsystem :senn-im-kkc-engine
  :serial t
  :pathname "src/im/"
  :components
  ((:file "kkc/request")
   (:file "kkc/engine")
   (:file "kkc-store/engine"))
  :depends-on (:jsown
	       :senn))
