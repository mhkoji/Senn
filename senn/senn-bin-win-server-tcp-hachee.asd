(asdf:defsystem :senn-bin-win-server-tcp-hachee
  :serial t
  :components
  ((:file "src/bin/win-server-tcp-hachee"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :senn-win
               :senn-im-kkc-hachee
               :senn-ipc-server-tcp))
