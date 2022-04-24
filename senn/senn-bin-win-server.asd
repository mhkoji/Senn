(asdf:defsystem :senn-bin-win-server
  :serial t
  :components
  ((:file "im/kkc/hachee")
   (:file "src/bin/win-server"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :senn-win
               :senn-server-tcp))
