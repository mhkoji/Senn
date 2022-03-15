(asdf:defsystem :senn-kkc-engine-hachee-lm
  :serial t
  :components
  ((:file "engine")
   (:file "lm"))
  :depends-on (:hachee-kkc-impl-lm
               :alexandria
               :cl-fad
               :jsown
               :log4cl))
