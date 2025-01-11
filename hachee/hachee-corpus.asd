(asdf:defsystem :hachee-corpus
  :serial t
  :components
  ((:file "src/corpus/corpus"))
  :depends-on (:cl-ppcre))
