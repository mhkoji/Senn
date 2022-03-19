(asdf:defsystem :hachee-data
  :serial t
  :pathname "src/data/"
  :components
  ((:file "corpus"))

  :perform (asdf:load-op (o s)
             (funcall (intern (symbol-name :set-data-path)
                              :hachee.data.corpus)
                      (merge-pathnames
                       "src/data/"
                       (asdf:system-source-directory :hachee-data)))))
