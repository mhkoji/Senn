(asdf:defsystem :senn-lib-win
  :serial t
  :components
  ((:file "src/lib/win")
   (:file "src/lib/win-engine"))
  :depends-on (:senn-win
               :senn-im-kkc-engine
               :log4cl))
