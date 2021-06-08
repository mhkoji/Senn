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

   (:module :algorithm
    :pathname "algorithm"
    :components
    ((:file "chu-liu-edmonds")
     (:file "longest-common-subsequence")))

   (:module :kkc
    :pathname "kkc/cl"
    :components
    ((:file "dictionary")
     (:file "convert/viterbi")
     (:file "convert/entry")
     (:file "lookup")
     (:file "kkc/origin")
     (:file "kkc/util")
     (:file "kkc/build/file")
     (:file "kkc/build/build")
     (:file "kkc/archive")
     (:file "kkc/kkc")
     (:file "profile")
     (:file "eval")))

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
