(asdf:defsystem :hachee
  :serial t
  :pathname "src"
  :components
  ((:module :algorithm
    :pathname "algorithm"
    :components
    ((:file "chu-liu-edmonds")))

   #+nil
   (:file "util/stream")

   #+nil
   (:module :dependency-parsing
    :pathname "dependency-parsing"
    :components
    ((:file "easy-first")
     (:file "shift-reduce")
     (:file "mst")))
   )
  :depends-on (:alexandria
               :clazy
               :cl-ppcre
               :flexi-streams
               :hachee-kkc))
