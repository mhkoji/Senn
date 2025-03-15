(defpackage :senn.win.stateful-ime
  (:use :cl)
  (:export :get-input-mode
           :toggle-input-mode
           :can-process
           :process-input
           :history-overwrite-mixin
           :ime
           :ime-kkc
           :make-ime))
(in-package :senn.win.stateful-ime)

(defclass history-overwrite-mixin ()
  ((segments-fn
    :initform #'identity)))

(defmethod senn.im.kkc:convert ((kkc history-overwrite-mixin) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (let ((segs (call-next-method)))
    (let ((segments-fn (slot-value kkc 'segments-fn)))
      (or (funcall segments-fn segs) segs))))

(defgeneric history-overwrite-mixin-set-segments-fn (mixin fn)
  (:method (mixin fn)
    nil)
  (:method ((mixin history-overwrite-mixin) fn)
    (setf (slot-value mixin 'segments-fn) fn)))


;; application state
(defstruct state
  input-mode
  input-state
  history)

(defun make-initial-state ()
  (make-state
   :input-mode :direct
   :input-state :direct-state
   :history (senn.win.history:make-history)))

(defgeneric ime-state (ime))


(defun get-input-mode (ime)
  (format nil "~A" (state-input-mode (ime-state ime))))

(defun toggle-input-mode (ime)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (destructuring-bind (state mode)
        (senn.win.im.toggle-input-mode:execute input-state input-mode)
      (setf input-state state)
      (setf input-mode mode)))
  ;; It seems to need to consume output buffer..
  "OK")

(defun can-process (ime key)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (let ((can-process (senn.win.im.can-process:execute
                        input-state key input-mode)))
      (format nil "~A" (if can-process 1 0)))))

(defun process-input (ime key)
  (with-accessors ((history state-history)
                   (input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (let ((result (senn.win.im.process-input:execute
                   input-state ime key input-mode)))
      (destructuring-bind (can-process view
                           &key state committed-segments)
          result
        ;; update application state
        (when state
          (setf input-state state))
        (when committed-segments
          (dolist (seg committed-segments)
            (senn.win.history:history-put
             history
             (senn.im.converting:segment-pron seg)
             (senn.im.converting:segment-cursor-pos-form seg))))
        (format nil "~A ~A"
                (if (and can-process view) 1 0)
                (or view ""))))))

;;;

(defclass ime (senn.win.im:ime)
  ((state
    :initarg :state
    :reader ime-state)
   (kkc
    :initarg :kkc
    :reader ime-kkc)
   (predictor
    :initarg :predictor
    :reader ime-predictor)))

(defmethod senn.win.im:ime-kkc ((ime ime))
  (ime-kkc ime))

(defmethod senn.win.im:ime-predictor ((ime ime))
  (ime-predictor ime))

(defun make-ime (&key kkc predictor)
  (let ((state (make-initial-state)))
    (history-overwrite-mixin-set-segments-fn
     kkc (let ((history (state-history state)))
           (lambda (segs)
             (senn.win.history:history-apply history segs))))
    (make-instance 'ime
                   :state state
                   :kkc kkc
                   :predictor predictor)))
