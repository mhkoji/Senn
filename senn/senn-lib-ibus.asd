(asdf:defsystem :senn-lib-ibus
  :serial t
  :components
  ((:file "src/lib/ibus")
   (:file "src/lib/ibus-engine")
   #+sbcl
   (:file "src/lib/ibus-sbcl"))
  :depends-on (:senn-ibus
               #+sbcl
               :sbcl-librarian))
