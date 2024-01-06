(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/fcitx"
  :components
  ((:file "keys")
   (:file "im/view")
   (:file "im/state/inputting")
   (:file "im/state/converting")
   (:file "im/state/katakana")
   (:file "im/state/selecting-from-predictions")
   (:file "im/immutable/ime")
   (:file "im/immutable/process-input")
   (:file "im/immutable/select-candidate")
   (:file "im/mutable")
   (:file "im/server"))
  :depends-on (:babel
               :jsown
               :senn))
