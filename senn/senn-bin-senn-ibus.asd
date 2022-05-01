(asdf:defsystem :senn-bin-senn-ibus
  :serial t
  :components
  ((:file "src/bin/senn-ibus"))
  :depends-on (:senn-ibus
               :senn-ipc-server-stdio))
