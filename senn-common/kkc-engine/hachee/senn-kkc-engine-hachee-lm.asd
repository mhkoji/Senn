(asdf:defsystem :senn-kkc-engine-hachee-lm
  :serial t
  :pathname "src/engine/"
  :components
  ((:file "engine")
   (:file "lm"))
  :depends-on (:alexandria
               :cl-fad
               :yason
               :hachee-kkc-impl-lm
               :hachee-corpus-data
               :senn-ipc-server-stdio))
