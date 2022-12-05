(asdf:defsystem :senn-im-kkc-hachee
  :serial t
  :components
  ((:file "src/im/kkc/hachee"))
  :depends-on (:senn-im-kkc
               :hachee-data
               :hachee-kkc-impl-lm))
