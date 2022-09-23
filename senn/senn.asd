(asdf:defsystem :senn
  :serial t
  :pathname "src/"
  :components
  ((:file "ja")
   #+nil (:file "prefix-dictionary")
   (:module :im
    :pathname "im"
    :components
    ((:file "kkc/kkc")
     (:module :im/predict
      :pathname "predict"
      :components
      (#+nil (:file "prefix")
       (:file "predict")
       (:file "katakana")))
     (:file "converting")
     (:file "buffer")
     (:file "inputting")
     (:file "kkc-store/kkc-store"))))
  :depends-on (#+nil :cl-trie
               :alexandria))
