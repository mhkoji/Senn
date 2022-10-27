(asdf:defsystem :senn-kkc-engine
  :serial t
  :pathname "src/"
  :components
  ((:file "request")
   (:file "engine")
   (:file "store/store")
   (:file "store/engine"))
  :depends-on (:senn-kkc
               :jsown
               :log4cl))

