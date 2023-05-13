(asdf:defsystem :senn-im-kkc-engine-test
  :serial t
  :pathname "t/"
  :components
  ((:file "base")
   (:file "im/kkc/engine"))
  :depends-on (:senn-im-kkc-engine
               :fiveam))
