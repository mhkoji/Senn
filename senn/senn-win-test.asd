(asdf:defsystem :senn-win-test
  :serial t
  :components
  ((:file "src/im/kkc/hachee")
   (:file "t/base")
   (:file "t/im-util")
   (:file "t/win/win")
   (:file "t/win/lib"))
  :depends-on (:senn-lib-win
               :hachee-kkc-impl-lm
               :hachee-data
               :fiveam))
