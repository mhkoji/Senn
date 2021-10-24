(defpackage :senn.win.stateful-im
  (:use :cl)
  (:export :make-im
           :get-input-mode
           :toggle-input-mode
           :can-process
           :process-input))
(in-package :senn.win.stateful-im)

(defstruct state
  input-mode
  input-state)

(defstruct stateful-im
  ime
  state)

(defun make-im (ime)
  (make-stateful-im
   :ime ime
   :state (make-state :input-mode :hiragana
                      :input-state (senn.win.im:make-editing))))

(defun get-input-mode (stateful-im)
  (format nil "~A%" (state-input-mode (stateful-im-state stateful-im))))

(defun toggle-input-mode (stateful-im)
  (with-accessors ((state stateful-im-state)) stateful-im
    (with-accessors ((input-mode state-input-mode)
                     (input-state state-input-state)) state
      (ecase input-mode
        (:hiragana
         (setf input-mode :direct)
         (setf input-state nil))
        (:direct
         (setf input-mode :hiragana)
         (setf input-state (senn.win.im:make-editing)))))))

(defun can-process (stateful-im key)
  (with-accessors ((ime stateful-im-ime)
                   (state stateful-im-state)) stateful-im
    (with-accessors ((input-mode state-input-mode)
                     (input-state state-input-state)) state
      (let ((can-process (senn.win.im:can-process
                          ime input-state input-mode key)))
        (format nil "~A~%" (if can-process 1 0))))))

(defun process-input (stateful-im key)
  (with-accessors ((ime stateful-im-ime)
                   (state stateful-im-state)) stateful-im
    (with-accessors ((input-mode state-input-mode)
                     (input-state state-input-state)) state
      (destructuring-bind (new-input-state new-input-mode can-process view)
          (senn.win.im:process-input ime input-state input-mode key)
        (setf input-mode new-input-mode)
        (setf input-state new-input-state)
        (format nil "~A ~A~%" (if can-process 1 0) view)))))
