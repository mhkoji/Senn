(defpackage :hachee.t.fiveam
  (:use :cl :fiveam)
  (:export :set-system-pathname))
(in-package :hachee.t.fiveam)

(defvar *system-pathname*
  (asdf:system-source-directory :hachee))

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

(test hachee.t.scenario.kkc
  (hachee.t.scenario.kkc.word-pron:build-and-convert-pronunciations
   (cl-fad:list-directory
    (merge-pathnames "src/kkc/data/aozora/word-pron-utf8/"
                     *system-pathname*))
   :test is))
