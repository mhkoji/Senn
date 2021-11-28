(defpackage :senn.win.stateful-ime
  (:use :cl)
  (:export :ime-state
           :get-input-mode
           :toggle-input-mode
           :can-process
           :process-input

           :stateful
           :make-initial-state
           :make-ime
           :make-engine-ime))
(in-package :senn.win.stateful-ime)

(defstruct history
  (hash (make-hash-table :test #'equal)))

(defun history-put (history pron form)
  (setf (gethash pron (history-hash history)) form))

(defun history-get-form (history pron)
  (gethash pron (history-hash history)))

(defun history-apply (history segs)
  (mapcar (lambda (seg)
            (let* ((pron (senn.segment:segment-pron seg))
                   (history-form (history-get-form history pron)))
              (if (not history-form)
                  seg
                  (senn.segment:make-segment
                   :pron pron
                   :candidates (list (senn.segment:make-candidate
                                      :form history-form
                                      :origin :history))
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
             (senn.segment:segment-pron seg)
             (senn.segment:segment-current-form seg))))
        (format nil "~A ~A~%"
                (if (and can-process view) 1 0)
                (or view ""))))))

;;;
   
(defclass effected-ime (senn.im:ime)
  ((kkc
    :initarg :kkc
    :reader effected-ime-kkc)))

(defmethod senn.im:convert ((ime effected-ime) (pron string)
                            &key 1st-boundary-index)
  (with-accessors ((kkc effected-ime-kkc)
                   (state ime-state)) ime
    (let ((kkc-convert (hachee.kkc:make-kkc-convert
                        :kkc kkc
                        :extended-dictionary
                        (state-extended-dictionary state))))
      (let ((segs (senn.im.mixin.hachee:convert
                   kkc-convert pron
                   :1st-boundary-index 1st-boundary-index)))
        (history-apply (state-history state) segs)))))

(defmethod senn.im:lookup ((ime effected-ime) (pron string)
                           &key next prev)
  (with-accessors ((kkc effected-ime-kkc)) ime
    (senn.im.mixin.hachee:lookup kkc pron :next next :prev prev)))

(defmethod senn.im:predict append ((ime effected-ime) (pron string))
  (list pron))

;;;

(defclass stateful ()
  ((state :initarg :state)))

(defmethod ime-state ((ime stateful))
  (slot-value ime 'state))

;;;

(defclass stateful-effected-ime (stateful
                                 effected-ime)
  ())

(defun make-ime (kkc state)
  (make-instance 'stateful-effected-ime
                 :kkc kkc
                 :state state))

;;;

(defclass stateful-engine-ime (stateful
                               senn.im:ime
                               senn.im.mixin.engine:convert
                               senn.im.mixin.engine:lookup)
  ())

(defun make-engine-ime (engine state)
  (make-instance 'stateful-engine-ime
                 :convert-engine-impl engine
                 :lookup-engine-impl engine
                 :state state))
