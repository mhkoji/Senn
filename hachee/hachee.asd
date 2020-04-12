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
    ((:file "ja")))

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
    ((:file "word/word")
     (:file "word/dictionary")
     (:file "util")
     (:file "convert")
     (:file "lookup")
     (:file "archive")
     (:file "build/file")
     (:file "build/build")
     (:file "kkc/entry")
     (:file "kkc/kkc")
     (:file "kkc/full/score-fns")
     (:file "kkc/full/full")
     (:file "kkc/full/factory")
     (:file "kkc/simple/convert")
     (:file "kkc/simple/simple")
     (:file "profile")))

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
