(defpackage :senn.win.stateful-ime
  (:use :cl)
  (:export :make-im
           :get-input-mode
           :toggle-input-mode
           :can-process
           :process-input))
(in-package :senn.win.stateful-ime)

(defstruct state
  input-mode
  input-state)

(defstruct stateful-ime
  ime
  state)

(defun make-im (ime)
  (make-stateful-ime
   :ime ime
   :state (make-state :input-mode :hiragana
                      :input-state (senn.win.ime:make-editing))))

(defun get-input-mode (stateful-ime)
  (format nil "~A%" (state-input-mode (stateful-ime-state stateful-ime))))

(defun toggle-input-mode (stateful-ime)
  (with-accessors ((state stateful-ime-state)) stateful-ime
    (with-accessors ((input-mode state-input-mode)
                     (input-state state-input-state)) state
      (ecase input-mode
        (:hiragana
         (setf input-mode :direct)
         (setf input-state nil))
        (:direct
         (setf input-mode :hiragana)
         (setf input-state (senn.win.ime:make-editing)))))))

(defun can-process (stateful-ime key)
  (with-accessors ((ime stateful-ime-ime)
                   (state stateful-ime-state)) stateful-ime
    (with-accessors ((input-mode state-input-mode)
                     (input-state state-input-state)) state
      (let ((can-process (senn.win.ime:can-process
                          ime input-state input-mode key)))
        (format nil "~A~%" (if can-process 1 0))))))

(defun process-input (stateful-ime key)
  (with-accessors ((ime stateful-ime-ime)
                   (state stateful-ime-state)) stateful-ime
    (with-accessors ((input-mode state-input-mode)
                     (input-state state-input-state)) state
      (destructuring-bind (new-input-state new-input-mode can-process view)
          (senn.win.ime:process-input ime input-state input-mode key)
        (setf input-mode new-input-mode)
        (setf input-state new-input-state)
        (format nil "~A ~A~%" (if can-process 1 0) view)))))
