(asdf:defsystem :senn
  :serial t
  :pathname "src/"
  :components
  ((:file "ja")
   #+nil (:file "prefix-dictionary")
   (:module :im
    :pathname "im"
    :components
    ((:module :im/predict
      :pathname "predict"
      :components
      (#+nil (:file "prefix")
       (:file "predict")
       (:file "katakana")))
     (:file "converting")
     (:file "buffer")
     (:file "inputting"))))
  :depends-on (:senn-im-kkc
               :alexandria
               #+nil :cl-trie))
