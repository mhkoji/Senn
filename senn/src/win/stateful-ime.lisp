(defpackage :senn.win.stateful-ime
  (:use :cl)
  (:export :get-input-mode
           :toggle-input-mode
           :can-process
           :process-input
           :service
           :service-ime))
(in-package :senn.win.stateful-ime)

(defstruct history
  (hash (make-hash-table :test #'equal)))

(defun history-put (history pron form)
  (setf (gethash pron (history-hash history)) form))

;; application state
(defstruct state
  input-mode
  input-state
  history)

(defun make-initial-state ()
  (make-state
   :input-mode :direct
   :input-state :direct-state
   :history (make-history)))

(defclass service ()
  ((ime
    :initarg :ime
    :reader service-ime)
   (state
    :initform (make-initial-state)
    :accessor service-state)))

(defmethod senn.win.history:get-history ((holder service))
  (state-history (service-state holder)))

(defmethod senn.win.history:get-form ((history history) pron)
  (gethash pron (history-hash history)))


(defun get-input-mode (service)
  (format nil "~A" (state-input-mode (service-state service))))

(defun toggle-input-mode (service)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (service-state service)
    (destructuring-bind (state mode)
        (senn.win.ime:toggle-input-mode
         input-state (service-ime service) input-mode)
      (setf input-state state)
      (setf input-mode mode)))
  ;; It seems to need to consume output buffer..
  "OK")

(defun can-process (service key)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (service-state service)
    (let ((can-process
           (senn.win.ime:can-process
            input-state (service-ime service) key input-mode)))
      (format nil "~A" (if can-process 1 0)))))

(defun process-input (service key)
  (with-accessors ((history state-history)
                   (input-mode state-input-mode)
                   (input-state state-input-state)) (service-state service)
    (destructuring-bind (view &key state committed-segments)
        (senn.win.ime:process-input
         input-state (service-ime service) key input-mode)
      ;; update application state
      (when state
        (setf input-state state))
      (when committed-segments
        (dolist (seg committed-segments)
          (history-put history
                       (senn.im.converting:segment-pron seg)
                       (senn.im.converting:segment-cursor-pos-form seg))))
      (format nil "~A ~A"
              (if view 1 0)
              (or view "")))))
