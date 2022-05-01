(asdf:defsystem :senn-ipc-named-pipe
  :serial t
  :components
  ((:file "src/ipc/named-pipe"))
  :depends-on (:win32))
