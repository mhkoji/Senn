(asdf:defsystem :senn-lib-ibus
  :serial t
  :components
  ((:file "src/lib/ibus")
   ; (:file "src/lib/ibus-sbcl")
   (:file "src/lib/ibus-engine"))
  :depends-on (:senn-ibus
               ;:sbcl-librarian
               ))
