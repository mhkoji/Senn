(asdf:defsystem :senn-win-test
  :serial t
  :pathname "t/"
  :components
  ((:file "win"))

  :depends-on (:senn-bin-win-server
               :fiveam))
