(defpackage :senn.t.ibus.lib
  (:use :cl)
  (:export :make-ime))
(in-package :senn.t.ibus.lib)

(defmacro make-ime (&key test)
  `(let ((engine-path (merge-pathnames
                       "t/kkc-engine-today.py"
                       (asdf:system-source-directory :senn))))
     (let ((ime (senn.lib.ibus:make-ime engine-path)))
       (unwind-protect
            (let ((state (senn.im.converting:convert
                          (senn.ibus.stateful-ime:get-ime ime)
                          "きょうは")))
              (,test (equal
                      (senn.t.im-util:converting-state-segment-strings
                       state)
                      '("今日/きょう" "は/は"))))
         (senn.lib.ibus:close-ime ime)))))

(senn.t.ibus:add-tests
 :lib
 make-ime)
