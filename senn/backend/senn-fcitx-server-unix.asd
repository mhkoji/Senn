(asdf:defsystem :senn-fcitx-server-unix
  :serial t
  :components
  ((:file "src/bin/fcitx-server-unix"))
  :depends-on (:senn-fcitx
               :senn-server-unix))
