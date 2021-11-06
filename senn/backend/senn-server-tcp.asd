(asdf:defsystem :senn-server-tcp
  :serial t
  :components
  ((:file "src/server/tcp"))
  :depends-on (:senn
               :usocket))
