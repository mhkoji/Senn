(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/fcitx"
  :components
  ((:file "keys")
   (:file "im/view")
   (:file "im/immutable/state/inputting")
   (:file "im/immutable/state/converting")
   (:file "im/immutable/state/katakana")
   (:file "im/immutable/state/selecting-from-predictions")
   (:file "im/immutable/ime")
   (:file "im/immutable/impl/impl")
   (:file "im/immutable/impl/process-input")
   (:file "im/immutable/impl/select-candidate")
   (:file "im/mutable")
   (:file "im/server"))
  :depends-on (:babel
               :yason
               :senn))
