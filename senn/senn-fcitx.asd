(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/fcitx"
  :components
  ((:file "keys")
   (:file "im/state")
   (:file "im/view")
   (:file "im/ime")
   (:file "im/process-input")
   (:file "im/select-candidate")
   (:file "im")
   (:file "stateful-ime")
   (:file "server"))
  :depends-on (:babel
               :jsown
               :log4cl
               :senn
	       :senn-im-kkc-engine))
