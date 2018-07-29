(asdf:defsystem :hachee
  :serial t
  :pathname "src"
  :components
  ((:file "kkc")

   (:module :algorithm
    :pathname "algorithm"
    :components
    ((:file "mst"))))
  :depends-on (:alexandria
               :anaphora
               :cl-fad
               :cl-ppcre
               :metabang-bind))
