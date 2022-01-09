(defpackage :senn.win.stateful-ime
  (:use :cl)
  (:export :ime
           :make-initial-state
           :get-input-mode
           :toggle-input-mode
           :can-process
           :process-input

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
            (let* ((pron (senn.im.segment:segment-pron seg))
                   (history-form (history-get-form history pron)))
              (if (not history-form)
                  seg
                  (senn.im.segment:make-segment
                   :pron pron
                   :candidates (list (senn.im.segment:make-candidate
                                      :form history-form))
                   :current-index 0
                   :has-more-candidates-p t))))
          segs))

;; application state
(defstruct state
  input-mode
  input-state
  history
  extended-dictionary)

(defun make-initial-state ()
  (make-state
   :input-mode :direct
   :input-state nil
   :history (make-history)
   :extended-dictionary
   (hachee.kkc.dictionary:make-dictionary)))

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
       (setf input-state (senn.win.im:make-editing)))))
  ;; It seems to need to consume output buffer..
  (format nil "OK~%"))

(defun can-process (ime key)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (let ((can-process (senn.win.im.can-process:execute
                        ime input-state input-mode key)))
      (format nil "~A~%" (if can-process 1 0)))))

(defun process-input (ime key)
  (with-accessors ((history state-history)
                   (input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (let ((result (senn.win.im.process-input:execute
                   ime input-state input-mode key)))
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
             (senn.im.segment:segment-pron seg)
             (senn.im.segment:segment-current-form seg))))
        (format nil "~A ~A~%"
                (if (and can-process view) 1 0)
                (or view ""))))))

;;;

(defclass ime (senn.im.ime:ime)
  ((state :initarg :state)))

(defmethod ime-state ((ime ime))
  (slot-value ime 'state))

;;;

(defclass hachee-kkc (senn.im.kkc.hachee:kkc)
  ((history
    :initarg :history)))

(defmethod senn.im.kkc:convert ((kkc hachee-kkc) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (let ((segs (call-next-method)))
    (history-apply (slot-value kkc 'history) segs)))

(defclass stateful-hachee-ime (ime)
  ((kkc
    :initarg :kkc)
   (predictor
    :initform
    (make-instance 'senn.im.predict.katakana:predictor))))

(defmethod senn.im.ime:ime-kkc ((ime stateful-hachee-ime))
  (make-instance 'hachee-kkc
   :history (state-history (ime-state ime))
   :kkc (slot-value ime 'kkc)
   :extended-dictionary (state-extended-dictionary (ime-state ime))))

(defun hachee-make-ime (kkc)
  (let ((state (make-initial-state)))
    (make-instance 'stateful-hachee-ime :kkc kkc :state state)))

;;;

(defclass stateful-engine-ime (ime)
  ((engine-kkc :initarg :engine-kkc)))

(defmethod senn.im.ime:ime-kkc ((ime stateful-engine-ime))
  (slot-value ime 'engine-kkc))

(defun engine-make-ime (engine-runner)
  (make-instance 'stateful-engine-ime
   :state (make-initial-state)
   :engine-kkc (make-instance 'senn.im.kkc.engine:kkc
                :engine-store
                (senn.im.kkc.engine:make-engine-store
                 :engine (senn.im.kkc.engine:run-engine engine-runner)
                 :engine-runner engine-runner))))

(defun engine-close-ime (ime)
  (senn.im.kkc.engine:close-mixin (slot-value ime 'engine-kkc)))
