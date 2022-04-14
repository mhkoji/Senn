(asdf:defsystem :senn-user-dict
  :serial t
  :pathname "src/"
  :components
  ((:file "cffi")
   (:file "user-dict"))
  :depends-on (:cffi))
