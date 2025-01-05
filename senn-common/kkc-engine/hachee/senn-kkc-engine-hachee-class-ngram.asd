(asdf:defsystem :senn-kkc-engine-hachee-class-ngram
  :serial t
  :pathname "src/engine/"
  :components
  ((:file "engine")
   (:file "class-ngram"))
  :depends-on (:alexandria
               :cl-ppcre
               :yason
               :hachee-kkc-impl-class-ngram
	       :senn-ipc-server-stdio))
