(asdf:defsystem :hachee-kkc
  :serial t
  :pathname "src/"
  :components
  ((:file "ja")
   (:file "kkc/origin")
   (:file "kkc/convert/viterbi")
   (:file "kkc/convert/viterbi-2nd")
   (:file "kkc/convert/convert")
   (:file "kkc/lookup")))
