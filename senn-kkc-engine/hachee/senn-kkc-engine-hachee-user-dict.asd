(asdf:defsystem :senn-kkc-engine-hachee-user-dict
  :serial t
  :pathname "src/user-dict/"
  :components
  ((:file "cffi")
   (:file "user-dict"))
  :depends-on (:cffi))
