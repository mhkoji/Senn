(asdf:defsystem :senn-bin-ibus-server
  :serial t
  :components
  ((:file "im/kkc/hachee")
   (:file "src/bin/ibus-server-tcp"))
  :depends-on (:senn-ibus
               :senn-server-tcp
               :senn-server-unix))
