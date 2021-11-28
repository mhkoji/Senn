(asdf:defsystem :senn-lib-fcitx
  :serial t
  :components
  ((:file "src/lib/fcitx")
   #+nil
   (:file "src/lib/fcitx-engine"))
  :depends-on (:senn-fcitx))
