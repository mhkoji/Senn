(asdf:defsystem :hachee-kkc-eval
  :serial t
  :components
  ((:file "src/algorithm/longest-common-subsequence")
   (:file "src/kkc/eval")
   #+sbcl
   (:file "src/kkc/profile"))
  :depends-on (:hachee-kkc))
