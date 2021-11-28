(asdf:defsystem :senn-bin-kkc-engine
  :serial t
  :components
  ((:file "src/bin/kkc-engine"))
  :depends-on (:hachee-kkc))
