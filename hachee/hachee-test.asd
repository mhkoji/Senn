(asdf:defsystem :hachee-test
  :serial t
  :pathname "t/cl"
  :components
  ((:module :scenario
    :pathname "scenario"
   :components
   ((:file "chu-liu-edmonds")
    #+nil
    (:file "dependency-parsing/easy-first")
    #+nil
    (:file "dependency-parsing/shift-reduce")
    (:file "kkc/word-pron")))

   (:file "fiveam"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :run!) :fiveam) :hachee))

  :depends-on (:hachee
               :hachee-kkc
               :hachee-language-model
               :hachee-kkc-impl-lm
               :fiveam))
