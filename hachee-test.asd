(asdf:defsystem :hachee-test
  :serial t
  :pathname "t/unit"
  :components
  ((:module :scenario
    :pathname "scenario"
   :components
   ((:file "chu-liu-edmonds")
    (:file "dependency-parsing/easy-first")
    (:file "dependency-parsing/shift-reduce")
    (:file "kkc/word-pron-kkc")))

   (:file "fiveam"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :set-system-pathname)
                              :hachee.t.fiveam)
                      (asdf/pathname:pathname-directory-pathname
                       (asdf:system-definition-pathname :hachee-test)))
             (funcall (intern (symbol-name :run!) :fiveam) :hachee))

  :depends-on (:hachee :fiveam))
