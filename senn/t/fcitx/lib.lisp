(defpackage :senn.t.fcitx.lib
  (:use :cl)
  (:export :make-ime))
(in-package :senn.t.fcitx.lib)

(defmacro make-ime (&key test)
  `(let ((engine-path (merge-pathnames
                       "t/kkc-engine-today.py"
                       (asdf:system-source-directory :senn))))
     (let ((ime (senn.lib.fcitx:make-ime engine-path)))
       (unwind-protect
            (let ((state (senn.im.converting:convert
                          (senn.fcitx.im.mutable:get-ime ime)
                          "きょうは")))
              (,test (equal
                      (senn.t.im-util:converting-state-segment-strings
                       state)
                      '("今日/きょう" "は/は"))))
         (senn.lib.fcitx:close-ime ime)))))

(senn.t.fcitx:add-tests
 :lib
 make-ime)
