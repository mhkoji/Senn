(defpackage :hachee.t.fiveam
  (:use :cl :fiveam))
(in-package :hachee.t.fiveam)
(def-suite :hachee)
(in-suite* :hachee)


(test hachee.t.scenario.chu-liu-edmonds
  (hachee.t.scenario.chu-liu-edmonds:solve1 :test is)
  (hachee.t.scenario.chu-liu-edmonds:solve2 :test is)
  (hachee.t.scenario.chu-liu-edmonds:solve3 :test is))
