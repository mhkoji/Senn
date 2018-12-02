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

   (:module :ja
    :pathname "ja"
    :components
    ((:file "packages")
     (:file "hiragana")))

   (:module :kkc
    :pathname "kkc/cl"
    :components
    ((:module :word
      :pathname "word"
      :components
      ((:file "word")
       (:file "vocabulary")
       (:file "dictionary")))
     (:file "models/unknown-word")
     (:file "file")
     (:module :kkc
      :pathname "kkc"
      :components
      ((:file "convert/convert")
       (:file "convert/score-fns")
       (:file "lookup")
       (:file "kkc")))))

   (:module :input-method
    :pathname "input-method/cl"
    :components
    ((:file "kkc-server")))

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
               :cl-arrows
               :cl-annot
               :cl-fad
               :cl-ppcre
               :jsown
               :log4cl
               :metabang-bind))
