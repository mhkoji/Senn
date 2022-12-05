(asdf:defsystem :hachee-kkc-impl-lm
  :serial t
  :pathname "src/kkc/impl/lm/"
  :components
  ((:file "unit")
   (:file "dictionary")
   (:file "build/file")
   (:file "build/build")
   (:file "persist")
   (:file "lm"))
  :depends-on (:hachee-kkc
               :hachee-language-model
               :cl-ppcre
               :flexi-streams
               :log4cl
               :zip))
