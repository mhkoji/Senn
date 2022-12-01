(asdf:defsystem :senn-im-kkc-engine
  :serial t
  :pathname "src/im/kkc/"
  :components
  ((:file "request")
   (:file "engine")
   (:file "store/engine"))
  :depends-on (:senn-im-kkc
               :jsown
               :log4cl))

