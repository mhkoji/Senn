(asdf:defsystem :senn-bin-win-server-tcp
  :serial t
  :components
  ((:file "src/bin/win-server-tcp"))
  :depends-on (:senn-win
               :senn-server-tcp))
