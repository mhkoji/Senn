(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/fcitx"
  :components
  ((:file "keys")
   (:file "view")
   (:file "ime")
   (:file "stateful-ime")
   (:file "server")
   (:file "im/state/inputting")
   (:file "im/state/converting")
   (:file "im/state/katakana")
   (:file "im/state/selecting-from-predictions")
   (:file "im/im")
   (:file "im/process-input")
   (:file "im/select-candidate"))
  :depends-on (:babel
               :yason
               :senn))
