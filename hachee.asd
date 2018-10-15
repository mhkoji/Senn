(asdf:defsystem :hachee
  :serial t
  :pathname "src"
  :components
  ((:module :language-model
    :pathname "language-model"
    :components
    ((:file "n-gram")
     (:file "language-model")))

   (:module :kkc
    :pathname "kkc/cl"
    :components
    ((:file "vocabulary")
     (:file "file")
     (:file "kkc")
     (:file "word-kkc")
     (:file "word-pron-kkc")))

   (:module :algorithm
    :pathname "algorithm"
    :components
    ((:file "chu-liu-edmonds")))

   (:file "util/stream")
   (:module :dependency-parsing
    :pathname "dependency-parsing"
    :components
    ((:file "easy-first")
     (:file "shift-reduce")
     #+nil (:file "mst")))
   )
  :depends-on (:alexandria
               :anaphora
               :clazy
               :cl-annot
               :cl-fad
               :cl-ppcre
               :metabang-bind))
