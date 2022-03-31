(asdf:defsystem :senn-user-dict
  :serial t
  :components
  ((:file "user-dict"))
  :depends-on (:cl-ppcre))
