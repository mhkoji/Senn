(asdf:defsystem :senn-user-dict-markov
  :serial t
  :pathname "src/"
  :components
  ((:file "user-dict-markov"))
  :depends-on (:senn-user-dict))
