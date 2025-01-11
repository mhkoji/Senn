(asdf:defsystem :hachee-corpus-data
  :serial t
  :components
  ((:file "src/corpus/data"))

  :perform (asdf:load-op (o s)
             (funcall (intern (symbol-name :set-data-path)
                              :hachee.corpus.data)
                      (merge-pathnames
                       "src/corpus/data/"
                       (asdf:system-source-directory :hachee-corpus-data)))))
