(asdf:defsystem :senn-ibus-test
  :serial t
  :pathname "t/"
  :components
  ((:file "ibus"))

  :depends-on (:senn-bin-ibus-server
               :fiveam))
