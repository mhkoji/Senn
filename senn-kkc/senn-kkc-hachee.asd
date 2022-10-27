(asdf:defsystem :senn-kkc-hachee
  :serial t
  :pathname "src/"
  :components
  ((:file "hachee")
   (:file "store/hachee/user-dict")
   (:file "store/hachee/hachee"))
  :depends-on (:senn-kkc
               :log4cl))

