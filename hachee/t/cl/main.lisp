(defpackage :hachee.t.main
  (:use :cl)
  (:export :run))
(in-package :hachee.t.main)

(fiveam:def-suite :hachee)
(fiveam:in-suite* :hachee)

(fiveam:test hachee.t.scenario.chu-liu-edmonds
  (hachee.t.scenario.chu-liu-edmonds:solve1 :test fiveam:is)
  (hachee.t.scenario.chu-liu-edmonds:solve2 :test fiveam:is)
  (hachee.t.scenario.chu-liu-edmonds:solve3 :test fiveam:is))

#+nil
(test hachee.t.scenario.dependency-parsing.easy-first
  (hachee.t.scenario.dependency-parsing.easy-first:train!-typical :test fiveam:is)
  (hachee.t.scenario.dependency-parsing.easy-first:parse-typical :test fiveam:is))

#+nil
(fiveam:test hachee.t.scenario.dependency-parsing.shift-reduce
  (hachee.t.scenario.dependency-parsing.shift-reduce:list-ordered-vertices-from-arcs :test fiveam:is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:search-oracle-action-sequence-typical :test fiveam:is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:search-oracle-action-sequence-on-parsed-text :test fiveam:is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:search-oracle-action-sequence-on-parsed-text-removing-arc-manually :test fiveam:is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:parse-text :test fiveam:is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:parse-on-parsed-text :test fiveam:is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:parse-on-parsed-text-removing-arc-manually :test fiveam:is))

(fiveam:test hachee.t.scenario.kkc
  (hachee.t.scenario.kkc.word-pron:build-and-convert-pronunciations
   :test fiveam:is))

(defun run ()
  (fiveam:run! :hachee))
