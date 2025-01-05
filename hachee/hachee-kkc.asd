(asdf:defsystem :hachee-kkc
  :serial t
  :pathname "src/"
  :components
  ((:file "ja")
   (:file "kkc/origin")
   (:file "kkc/convert/viterbi-2gram")
   (:file "kkc/convert/viterbi-3gram")
   (:file "kkc/convert/convert")
   (:file "kkc/lookup")))
