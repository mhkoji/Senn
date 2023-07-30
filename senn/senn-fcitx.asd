(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/fcitx"
  :components
  ((:file "keys")
   (:file "im/state")
   (:file "im/view")
   (:file "im/immutable/process-input")
   (:file "im/immutable/select-candidate")
   (:file "im/immutable/ime")
   (:file "im/mutable")
   (:file "im/server"))
  :depends-on (:babel
               :jsown
               :log4cl
               :senn))
