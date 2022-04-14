(asdf:defsystem :senn-user-dict-lm
  :serial t
  :pathname "src/"
  :components
  ((:file "user-dict-lm"))
  :depends-on (:senn-user-dict))
