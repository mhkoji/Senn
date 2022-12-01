(asdf:defsystem :senn-win-test
  :serial t
  :components
  ((:file "t/base")
   (:file "t/im-util")
   (:file "t/win/win")
   (:file "t/win/lib"))
  :depends-on (:senn-lib-win
               :senn-im-kkc-hachee
               :hachee-kkc-impl-lm
               :hachee-data
               :fiveam))
