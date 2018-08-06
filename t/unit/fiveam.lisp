(defpackage :hachee.t.fiveam
  (:use :cl :fiveam))
(in-package :hachee.t.fiveam)
(def-suite :hachee)
(in-suite* :hachee)

(test hachee.t.scenario.chu-liu-edmonds
  (hachee.t.scenario.chu-liu-edmonds:solve1 :test is)
  (hachee.t.scenario.chu-liu-edmonds:solve2 :test is)
  (hachee.t.scenario.chu-liu-edmonds:solve3 :test is))


(test hachee.t.scenario.dependency-parsing.easy-first
  (hachee.t.scenario.dependency-parsing.easy-first:train!-typical :test is)
  (hachee.t.scenario.dependency-parsing.easy-first:parse-typical :test is))

(test hachee.t.scenario.dependency-parsing.shift-reduce
  (hachee.t.scenario.dependency-parsing.shift-reduce:list-ordered-vertices-from-arcs :test is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:search-oracle-action-sequence-typical :test is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:search-oracle-action-sequence-on-parsed-text :test is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:search-oracle-action-sequence-on-parsed-text-removing-arc-manually :test is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:parse-text :test is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:parse-on-parsed-text :test is)
  (hachee.t.scenario.dependency-parsing.shift-reduce:parse-on-parsed-text-removing-arc-manually :test is))
