(asdf:defsystem :senn-im-kkc-engine
  :serial t
  :components
  ((:file "src/im/kkc/request")
   (:file "src/im/kkc/engine"))
  :depends-on (:yason
               :senn-im-kkc))
