(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "ja")
   #+nil (:file "prefix-dictionary")
   (:module :im
    :pathname "im"
    :components
    ((:file "kkc")
     (:module :im/kkc
      :pathname "kkc"
      :components
      ((:file "request")
       (:file "engine")))
     (:file "predict")
     (:module :im/predict
      :pathname "predict"
      :components
      (#+nil (:file "prefix")
       (:file "katakana")))
     (:file "converting")
     (:file "buffer")
     (:file "inputting"))))
  :depends-on (#+nil :cl-trie
               :alexandria
               :jsown))
