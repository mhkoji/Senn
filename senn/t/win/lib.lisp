(defpackage :senn.t.win.lib
  (:use :cl)
  (:export :make-ime))
(in-package :senn.t.win.lib)

(defmacro make-ime (&key test)
  `(progn
     (senn.lib.win:init (merge-pathnames
                         "t/kkc-engine-today.py"
                         (asdf:system-source-directory :senn)))
     (unwind-protect
          (let* ((ime (senn.lib.win:make-ime))
                 (state (senn.im.converting:convert ime "きょうは")))
            (,test (equal
                    (senn.t.im-util:converting-state-segment-strings state)
                    '("今日/きょう" "は/は"))))
       (senn.lib.win:destroy))))

(senn.t.win:add-tests
 :lib
 make-ime)
