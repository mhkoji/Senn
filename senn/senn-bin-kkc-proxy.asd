(asdf:defsystem :senn-bin-kkc-proxy
  :serial t
  :components
  ((:file "src/bin/kkc-proxy"))
  :depends-on (:senn-ipc-server-unix))
