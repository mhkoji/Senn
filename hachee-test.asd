(asdf:defsystem :hachee-test
  :serial t
  :pathname "t"
  :components
  ((:module :scenario
    :pathname "scenario"
   :components
   ((:file "chu-liu-edmonds")))

   (:file "fiveam"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :run!) :fiveam) :hachee))

  :depends-on (:hachee :fiveam))
