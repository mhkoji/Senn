(asdf:defsystem :hachee
  :serial t
  :pathname "src"
  :components ((:file "kkc"))
  :depends-on (:alexandria
               :anaphora
               :cl-fad
               :cl-ppcre
               :metabang-bind))
