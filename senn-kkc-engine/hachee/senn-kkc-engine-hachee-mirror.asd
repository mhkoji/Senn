(asdf:defsystem :senn-kkc-engine-hachee-mirror
  :serial t
  :pathname "src/engine/"
  :components
  ((:file "engine")
   (:file "mirror"))
  :depends-on (:hachee-kkc-impl-mirror
               :senn-kkc-engine-hachee-user-dict
	       :senn-ipc-server-stdio
               :alexandria
               :jsown))
