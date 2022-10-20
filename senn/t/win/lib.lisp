(defpackage :senn.t.win.lib
  (:use :cl)
  (:export :make-ime))
(in-package :senn.t.win.lib)

(defmacro make-ime (&key test)
  `(let ((engine-path (merge-pathnames
                       "bin/mock-kkc-engine.py"
                       (asdf:system-source-directory :senn-win))))
     (let ((ime (senn.lib.win:make-ime engine-path)))
       (unwind-protect
            (let ((state (senn.im.converting:convert ime "きょうは")))
              (,test (equal
                      (senn.t.im-util:converting-state-segment-strings
                       state)
                      '("今日/きょう" "は/は"))))
         (senn.lib.win:close-ime ime)))))

(senn.t.win:add-tests
 :lib
 make-ime)
