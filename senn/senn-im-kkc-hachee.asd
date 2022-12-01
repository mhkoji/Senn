(asdf:defsystem :senn-im-kkc-hachee
  :serial t
  :pathname "src/im/kkc/"
  :components
  ((:file "hachee")
   (:file "store/hachee/user-dict")
   (:file "store/hachee/hachee"))
  :depends-on (:senn-im-kkc
               :log4cl))

