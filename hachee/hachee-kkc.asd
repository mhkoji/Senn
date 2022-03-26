(asdf:defsystem :hachee-kkc
  :serial t
  :pathname "src/"
  :components
  ((:module :algorithm
    :pathname "algorithm"
    :components
    ((:file "longest-common-subsequence")))

   (:file "ja")

   (:module :kkc
    :pathname "kkc/"
    :components
    ((:file "origin")
     (:file "convert/viterbi")
     (:file "convert/convert")
     (:file "lookup")
     #+sbcl (:file "profile")
     (:file "eval")))))
