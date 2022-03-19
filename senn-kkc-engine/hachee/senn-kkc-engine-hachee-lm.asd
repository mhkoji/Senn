(asdf:defsystem :senn-kkc-engine-hachee-lm
  :serial t
  :components
  ((:file "engine")
   (:file "lm"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :alexandria
               :cl-fad
               :jsown
               :log4cl))
