(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/fcitx"
  :components
  ((:file "keys")
   (:file "im/view")
   (:file "im/im")
   (:file "im/process-input")
   (:file "im/select-candidate")
   (:file "im/json")
   (:file "stateful-ime")
   (:file "stateful-ime-engine")
   (:file "server"))
  :depends-on (:babel
               :jsown
               :log4cl
               :senn))
