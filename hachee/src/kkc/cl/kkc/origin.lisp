(defpackage :hachee.kkc.origin
  (:use :cl)
  (:export :+vocabulary+
           :+extended-dictionary+
           :+corpus+
           :+resource+
           :+out-of-dictionary+
           :+tankan+
           :+runtime-none+))
(in-package :hachee.kkc.origin)

(defparameter +vocabulary+          :vocabulary)
(defparameter +extended-dictionary+ :extended-dictionary)
(defparameter +corpus+              :corpus)
(defparameter +resource+            :resource)
(defparameter +out-of-dictionary+   :out-of-dictionary)
(defparameter +tankan+              :tankan)
(defparameter +runtime-none+        :runtime-none)
