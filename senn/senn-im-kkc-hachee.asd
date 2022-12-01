(asdf:defsystem :senn-im-kkc-hachee
  :serial t
  :components
  ((:file "src/im/kkc/hachee"))
  :depends-on (:senn-im-kkc
               :log4cl))

