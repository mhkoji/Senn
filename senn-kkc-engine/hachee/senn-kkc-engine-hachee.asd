(asdf:defsystem :senn-kkc-engine-hachee
  :serial t
  :components
  ((:file "engine"))
  :depends-on (:hachee-kkc
               :jsown))
