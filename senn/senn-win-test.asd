(asdf:defsystem :senn-win-test
  :serial t
  :components
  ((:file "src/im/kkc/hachee")
   (:file "t/base")
   (:file "t/win") )
  :depends-on (:hachee-kkc-impl-lm
	       :hachee-data
	       :senn-win
               :fiveam))
