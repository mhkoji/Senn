(asdf:defsystem :senn-bin-ibus-server-tcp
  :serial t
  :components
  ((:file "src/bin/ibus-server-tcp"))
  :depends-on (:senn-ibus
               :senn-server-tcp))
