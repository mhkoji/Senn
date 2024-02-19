(asdf:defsystem :senn
  :serial t
  :pathname "src/"
  :components
  ((:file "ja")
   (:module :im
    :pathname "im"
    :components
    ((:module :im/predict
      :pathname "predict"
      :components
      ((:file "predict")
       (:file "katakana")))
     (:file "converting")
     (:file "buffer")
     (:file "inputting"))))
  :depends-on (:senn-im-kkc
               :alexandria))
