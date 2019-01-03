(defpackage hachee.util.stream
  (:use :cl))
(in-package :hachee.util.stream)
(cl-annot:enable-annot-syntax)

@export
(defmacro cons-stream (head tail)
  `(clazy:cons-stream ,head ,tail))

@export
(defun head (stream)
  (clazy:head stream))

@export
(defun tail (stream)
  (clazy:tail stream))

@export
(defun stream-as-list (stream)
  (if (null stream)
      nil
      (cons (head stream) (stream-as-list (tail stream)))))
