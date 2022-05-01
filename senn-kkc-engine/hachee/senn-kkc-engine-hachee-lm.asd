(asdf:defsystem :senn-kkc-engine-hachee-lm
  :serial t
  :pathname "src/engine/"
  :components
  ((:file "engine")
   (:file "lm"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :senn-ipc-server-stdio
               :alexandria
               :cl-fad
               :jsown
               :log4cl))
