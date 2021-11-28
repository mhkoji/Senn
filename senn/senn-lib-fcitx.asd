(asdf:defsystem :senn-lib-fcitx
  :serial t
  :components
  ((:file "src/lib/fcitx")
   #+ecl
   (:file "src/lib/fcitx-engine"))
  :depends-on (:senn-fcitx
               #+sbcl
               :sbcl-librarian))
