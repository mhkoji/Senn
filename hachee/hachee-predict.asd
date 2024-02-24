(asdf:defsystem :hachee-predict
  :serial t
  :pathname "src/"
  :components
  ((:file "predict/prefix-dictionary"))
  :depends-on (:cl-trie))
