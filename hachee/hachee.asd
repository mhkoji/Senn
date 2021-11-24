(asdf:defsystem :hachee
  :serial t
  :pathname "src"
  :components
  ((:module :algorithm
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
               :clazy
               :cl-ppcre
               :flexi-streams
               :hachee-kkc))
