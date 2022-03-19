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

   (:file "main"))

  :depends-on (:hachee
               :hachee-kkc
               :hachee-language-model
               :hachee-kkc-impl-lm
               :hachee-data
               :fiveam))
