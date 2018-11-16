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
     (:file "convert/convert")
     (:file "convert/cost-fns")
     (:file "file")
     (:file "kkc")
     (:file "server")))

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
