(asdf:defsystem :senn-bin-win-server-hachee
  :serial t
  :components
  ((:file "src/im/kkc/hachee")
   (:file "src/bin/win-server-hachee"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :senn-win
               :senn-ipc-server-tcp))
