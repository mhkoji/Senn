(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/fcitx"
  :components
  ((:file "keys")
   (:file "view")
   (:file "im/state/inputting")
   (:file "im/state/converting")
   (:file "im/state/katakana")
   (:file "im/state/selecting-from-predictions")
   (:file "im/ime")
   (:file "im/impl/impl")
   (:file "im/impl/process-input")
   (:file "im/impl/select-candidate")
   (:file "stateful-ime")
   (:file "server"))
  :depends-on (:babel
               :yason
               :senn))
