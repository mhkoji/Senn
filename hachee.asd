(asdf:defsystem :hachee
  :serial t
  :pathname "src"
  :components
  ((:module :kkc
    :pathname "kkc/cl"
   :components
   ((:file "kkc")))

   (:module :algorithm
    :pathname "algorithm"
    :components
    ((:file "chu-liu-edmonds"))))
  :depends-on (:alexandria
               :anaphora
               :cl-fad
               :cl-ppcre
               :metabang-bind))
