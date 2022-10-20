(asdf:defsystem :senn-lib-win
  :serial t
  :components
  ((:file "src/lib/win")
   (:file "src/lib/win-engine"))
  :depends-on (:log4cl
               :senn-win
               ;:senn-im-kkc-named-pipe
               ))
