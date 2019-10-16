(defpackage :senn.fcitx.t.fiveam
  (:use :cl :fiveam))
(in-package :senn.fcitx.t.fiveam)
(def-suite :senn.fcitx)
(in-suite* :senn.fcitx)

(progn
  #.(cons 'progn
          (mapcar (lambda (ops-test)
                    `(test ,(intern (symbol-name ops-test))
                       (,ops-test :test is)))
                  senn.fcitx.t.scenario.transit:*ops-tests*)))
