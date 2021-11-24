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
     (:file "origin")
     (:file "util")
     (:file "convert/viterbi")
     (:file "convert/convert")
     (:file "lookup")
     (:file "persist")
     (:file "build/file")
     (:file "build/build")
     (:file "kkc")
     (:file "kkc-convert")
     (:file "kkc-lookup")
     #+sbcl
     (:file "profile")
     (:file "eval")))

   #+nil
   (:file "util/stream")
   #+nil
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
               #+nil
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
