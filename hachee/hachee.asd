(asdf:defsystem :hachee
  :serial t
  :pathname "src"
  :components
  ((:module :language-model
    :pathname "language-model"
    :components
    ((:file "vocabulary")
     (:file "language-model")
     (:file "freq")
     (:file "n-gram")))

   (:module :ja
    :pathname "ja"
    :components
    ((:file "packages")
     (:file "hiragana")
     (:file "romaji")))

   (:module :ipc
    :pathname "ipc"
    :components
    ((:file "op")
     #+linux
     (:file "unix")
     #+win32
     (:file "named-pipe")))

   (:module :kkc
    :pathname "kkc/cl"
    :components
    ((:module :word
      :pathname "word"
      :components
      ((:file "word")
       (:file "dictionary")))

     (:file "file")

     (:module :kkc
      :pathname "kkc"
      :components
      ((:file "convert/convert")
       (:file "convert/score-fns")
       (:file "lookup")
       (:file "archive")
       (:file "kkc")
       (:file "profile")))))

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
               :bordeaux-threads
               :clazy
               :cl-arrows
               :cl-annot
               :cl-fad
               :cl-ppcre
               :flexi-streams
               :jsown
               :log4cl
               :metabang-bind
               #+win32
               :win32
               :zip))
