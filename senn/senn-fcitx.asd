(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/fcitx"
  :components
  ((:file "keys")
   (:file "im/im")
   (:file "im/process-input")
   (:file "im/select-candidate")
   (:file "stateful-ime")
   (:file "server"))
  :depends-on (:babel
               :jsown
               :log4cl
               :senn))
