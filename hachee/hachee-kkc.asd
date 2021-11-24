(asdf:defsystem :hachee-kkc
  :serial t
  :pathname "src"
  :components
  ((:module :algorithm
    :pathname "algorithm"
    :components
    ((:file "longest-common-subsequence")))

   (:module :language-model
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
     (:file "eval"))))
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-ppcre
               :flexi-streams
               :jsown
               :log4cl
               #+win32
               :win32
               :zip))
