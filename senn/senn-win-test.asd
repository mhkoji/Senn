(asdf:defsystem :senn-win-test
  :serial t
  :pathname "t"
  :components
  ((:file "scenario/win"))

  :depends-on (:senn-win
               :fiveam))
