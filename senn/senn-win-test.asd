(asdf:defsystem :senn-win-test
  :serial t
  :pathname "t"
  :components
  ((:file "scenario/win"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :run!) :fiveam) :senn.win))

  :depends-on (:senn-win :fiveam))
