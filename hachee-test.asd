(asdf:defsystem :hachee-test
  :serial t
  :pathname "t/unit"
  :components
  ((:module :scenario
    :pathname "scenario"
   :components
   ((:file "chu-liu-edmonds")
    (:file "dependency-parsing/easy-first")
    (:file "dependency-parsing/shift-reduce")))

   (:file "fiveam"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :run!) :fiveam) :hachee))

  :depends-on (:hachee :fiveam))
