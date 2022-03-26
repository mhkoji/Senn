(defpackage :senn.win.stateful-ime
  (:use :cl)
  (:export :get-input-mode
           :toggle-input-mode
           :can-process
           :process-input

           :ime
           :make-initial-state
           :hachee-make-ime
           :engine-make-ime
           :engine-close-ime))
(in-package :senn.win.stateful-ime)

(defstruct history
  (hash (make-hash-table :test #'equal)))

(defun history-put (history pron form)
  (setf (gethash pron (history-hash history)) form))

(defun history-get-form (history pron)
  (gethash pron (history-hash history)))

(defun history-apply (history segs)
  (mapcar (lambda (seg)
            (let* ((pron (senn.im.kkc:segment-pron seg))
                   (history-form (history-get-form history pron)))
              (if (not history-form)
                  seg
                  (senn.im.kkc:make-segment
                   :pron pron
                   :candidates
                   (cons (senn.im.kkc:make-candidate :form history-form)
                         (remove history-form
                                 (senn.im.kkc:segment-candidates seg)
                                 :key #'senn.im.kkc:candidate-form
                                 :test #'string=))))))
          segs))

(defclass history-overwrite-mixin ()
  ((history :initarg :history)))

(defmethod senn.im.kkc:convert ((kkc history-overwrite-mixin) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (let ((segs (call-next-method)))
    (history-apply (slot-value kkc 'history) segs)))

;; application state
(defstruct state
  input-mode
  input-state
  history)

(defun make-initial-state ()
  (make-state
   :input-mode :direct
   :input-state nil
   :history (make-history)))

(defgeneric ime-state (ime))


(defun get-input-mode (ime)
  (format nil "~A~%" (state-input-mode (ime-state ime))))

(defun toggle-input-mode (ime)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (ecase input-mode
      (:hiragana
       (setf input-mode :direct)
       (setf input-state nil))
      (:direct
       (setf input-mode :hiragana)
       (setf input-state (senn.im.inputting:make-state)))))
  ;; It seems to need to consume output buffer..
  (format nil "OK~%"))

(defun can-process (ime key)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (let ((can-process (senn.win.im.can-process:execute
                        input-state input-mode key)))
      (format nil "~A~%" (if can-process 1 0)))))

(defun process-input (ime key)
  (with-accessors ((history state-history)
                   (input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (let ((result (senn.win.im.process-input:execute
                   input-state input-mode ime key)))
      (destructuring-bind (can-process view
                           &key state committed-segments)
          result
        ;; update application state
        (when state
          (setf input-state state))
        (when committed-segments
          (dolist (seg committed-segments)
            (history-put
             history
             (senn.im.converting:segment-pron seg)
             (senn.im.converting:segment-cursor-pos-form seg))))
        (format nil "~A ~A~%"
                (if (and can-process view) 1 0)
                (or view ""))))))

;;;

(defclass ime (senn.win.im:ime)
  ((state
    :initarg :state
    :reader ime-state)
   (kkc
    :initarg :kkc
    :reader senn.win.im:ime-kkc)
   (predictor
    :initarg :predictor
    :reader senn.win.im:ime-predictor)))

;;;

(defclass kkc (history-overwrite-mixin
               senn.im.kkc.hachee:kkc)
  ())

(defun hachee-make-ime (kkc)
  (let ((state (make-initial-state)))
    (make-instance 'ime
     :kkc (make-instance 'kkc
           :history (state-history state)
           :kkc-impl kkc)
     :predictor (make-instance 'senn.im.predict.katakana:predictor)
     :state state)))

;;;

(defun engine-make-ime (engine-runner)
  (make-instance 'ime
   :kkc (make-instance 'senn.im.kkc.engine:kkc
         :engine-store
         (senn.im.kkc.engine:make-engine-store
          :engine (senn.im.kkc.engine:run-engine engine-runner)
          :engine-runner engine-runner))
   :predictor nil
   :state (make-initial-state)))

(defun engine-close-ime (ime)
  (senn.im.kkc.engine:close-kkc (slot-value ime 'kkc)))
