(defpackage :senn.win.stateful-ime
  (:use :cl)
  (:export :stateful-ime
           :get-input-mode
           :toggle-input-mode
           :can-process
           :process-input

           :make-initial-state
           :make-from-kkc))
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

(defclass stateful-ime ()
  ((state
    :initarg :state
    :accessor stateful-ime-state)))

(defun get-input-mode (stateful-ime)
  (format nil "~A~%" (state-input-mode (stateful-ime-state stateful-ime))))

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
         (setf input-state (senn.win.im:make-editing))))))
  ;; It seems to need to consume output buffer..
  (format nil "OK~%"))

(defun can-process (stateful-ime key)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state))
      (stateful-ime-state stateful-ime)
    (let ((can-process (senn.win.im.can-process:execute
                        stateful-ime input-state input-mode key)))
      (format nil "~A~%" (if can-process 1 0)))))

(defun process-input (stateful-ime key)
  (with-accessors ((history state-history)
                   (input-state state-input-state)
                   (input-mode state-input-mode))
      (stateful-ime-state stateful-ime)
    (let ((result (senn.win.im.process-input:execute
                   stateful-ime input-state input-mode key)))
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
                   (state stateful-ime-state)) ime
    (let ((kkc-convert (hachee.kkc:make-kkc-convert
                        :kkc kkc
                        :extended-dictionary
                        (state-extended-dictionary state))))
      (let ((segs (senn.im.kkc:convert
                   kkc-convert pron
                   :1st-boundary-index 1st-boundary-index)))
        (history-apply (state-history state) segs)))))

(defmethod senn.im:lookup ((ime effected-ime) (pron string)
                           &key next prev)
  (senn.im.kkc:lookup (effected-ime-kkc ime) pron
                      :next next :prev prev))

;;;

(defclass stateful-effected-ime (stateful-ime
                                 effected-ime)
  ())

(defun make-initial-state ()
  (make-state
   :input-mode :direct
   :input-state nil
   :history (make-history)
   :extended-dictionary
   (hachee.kkc.dictionary:make-dictionary)))

(defun make-from-kkc (kkc)
  (make-instance 'stateful-effected-ime
                 :state (make-initial-state)
                 :kkc kkc))
