(asdf:defsystem :senn-fcitx-test
  :serial t
  :pathname "t/cl"
  :components
  ((:module :scenario
    :pathname "scenario"
    :components
    ((:file "transit")))

   (:file "fiveam"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :run!) :fiveam) :senn.fcitx))

  :depends-on (:senn-fcitx :fiveam))
