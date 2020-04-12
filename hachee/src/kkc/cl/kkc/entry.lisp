(defpackage :hachee.kkc.entry
  (:use :cl)
  (:export :entry-word
           :entry-token
           :entry-origin
           :make-entry))
(in-package :hachee.kkc.entry)

(defstruct entry word token origin)
