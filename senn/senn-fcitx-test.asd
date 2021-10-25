(asdf:defsystem :senn-fcitx-test
  :serial t
  :pathname "t"
  :components
  ((:file "scenario/fcitx"))

  :perform (asdf:test-op (o s)
             (funcall (intern (symbol-name :run!) :fiveam) :senn.fcitx))

  :depends-on (:senn-fcitx :fiveam))
