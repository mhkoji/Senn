(defpackage :hachee.kkc.archive
  (:use :cl)
  (:export :save-object
           :load-object-as))
(in-package :hachee.kkc.archive)

(defgeneric save-object (obj stream))

(defmethod save-object ((obj hachee.language-model.n-gram:model)
                        stream)
  (hachee.language-model.n-gram:save-model obj stream))

(defmethod save-object ((obj hachee.kkc.dictionary:dictionary)
                        stream)
  (hachee.kkc.dictionary:save-dictionary obj stream))

(defmethod save-object ((obj hachee.language-model.vocabulary:vocabulary)
                        stream)
  (hachee.language-model.vocabulary:save-vocabulary obj stream))


(defgeneric load-object-as (type stream))

(defmethod load-object-as
    ((type (eql 'hachee.language-model.n-gram:model))
     stream)
  (hachee.language-model.n-gram:load-model type stream))

(defmethod load-object-as
    ((type (eql 'hachee.language-model.n-gram:class-model))
     stream)
  (hachee.language-model.n-gram:load-model type stream))

(defmethod load-object-as
    ((type (eql 'hachee.kkc.dictionary:dictionary))
     stream)
  (hachee.kkc.dictionary:load-dictionary stream))

(defmethod load-object-as
    ((type (eql 'hachee.language-model.vocabulary:vocabulary))
     stream)
  (hachee.language-model.vocabulary:load-vocabulary stream))
