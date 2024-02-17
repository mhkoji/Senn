(asdf:defsystem :senn-kkc-engine-hachee-class-2-gram
  :serial t
  :pathname "src/engine/"
  :components
  ((:file "engine")
   (:file "class-2-gram"))
  :depends-on (:alexandria
               :cl-ppcre
               :yason
               :hachee-kkc-impl-class-2-gram
	       :senn-ipc-server-stdio))
