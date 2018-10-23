(asdf:defsystem :hachee
  :serial t
  :pathname "src"
  :components
  ((:module :language-model
    :pathname "language-model"
    :components
    ((:file "language-model")
     (:file "freq")
     (:file "n-gram")))

   (:module :kkc
    :pathname "kkc/cl"
    :components
    ((:file "vocabulary")
     (:file "dictionary")
     (:file "models/unknown-word")
     (:file "converters/converter")
     (:file "converters/word")
     (:file "converters/word-pron")
     (:file "file")
     (:file "kkc")))

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
