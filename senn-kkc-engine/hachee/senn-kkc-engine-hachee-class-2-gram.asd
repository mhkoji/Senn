(asdf:defsystem :senn-kkc-engine-hachee-class-2-gram
  :serial t
  :pathname "src/engine/"
  :components
  ((:file "engine")
   (:file "class-2-gram"))
  :depends-on (:hachee-kkc-impl-class-2-gram
               :senn-kkc-engine-hachee-user-dict
	       :senn-ipc-server-stdio
               :alexandria
               :jsown))
