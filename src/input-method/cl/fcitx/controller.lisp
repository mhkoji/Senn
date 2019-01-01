(defpackage :hachee.input-method.fcitx.controller
  (:use :cl
        :hachee.input-method.op
        :hachee.input-method.fcitx.states)
  (:export :process-client
           :make-controller)
  (:import-from :alexandria
                :when-let))
(in-package :hachee.input-method.fcitx.controller)

(defstruct controller id kkc)


(defgeneric transit-by-input (controller state code))

(defmethod transit-by-input ((c controller)
                             (s committed)
                             code)
  (transit-by-input (make-editing) code))


(defmethod transit-by-input ((c controller)
                             (s converting)
                             code)
  (make-committed
   :input (format nil "~{~A~}"
                  (mapcar #'segment-current-form
                          (converting-segments s)))))


(defmethod transit-by-input ((c controller)
                             (s editing)
                             code)
  (case code
    (32    ;; Space key
     (let ((pronunciation (editing-buffer s)))
       (let ((words (hachee.kkc:convert (controller-kkc c)
                                        pronunciation)))
         (make-converting
          :segments (mapcar (lambda (w)
                              (make-segment
                               :pron (hachee.kkc:word-pron w)
                               :forms (list (hachee.kkc:word-form w))
                               :current-index 0))
                            words)
          :pronunciation pronunciation))))
    (65293 ;; Enter key
     (make-committed :input (editing-buffer s)))
    (65361 ;; Left key
     (when (< 0 (editing-cursor-pos s))
       (decf (editing-cursor-pos s)))
     s)
    (65363 ;; Right key
     (when (< (editing-cursor-pos s) (length (editing-buffer s)))
       (incf (editing-cursor-pos s)))
     s)
    (t
     (destructuring-bind (new-buffer new-pos)
         (hachee.input-method.fcitx.states.editing:buffer-insert-char
          (editing-buffer s)
          (editing-cursor-pos s)
          (code-char code))
       (setf (editing-buffer s) new-buffer)
       (setf (editing-cursor-pos s) new-pos))
     s)))


(defgeneric make-response (s))


(defun editing-cursor-pos-in-utf-8 (editing)
  (length (sb-ext:string-to-octets
           (subseq (editing-buffer editing)
                   0
                   (editing-cursor-pos editing))
           :external-format :utf-8)))

(defmethod make-response ((s editing))
  (format nil "~A ~A ~A~%"
          :editing
          (editing-buffer s)
          (editing-cursor-pos-in-utf-8 s)))


(defmethod make-response ((s converting))
  (let ((input (format nil "~{~A~}"
                       (mapcar #'segment-current-form
                               (converting-segments s)))))
    (format nil "~A ~A ~A~%"
            :converting
            input
            (length (sb-ext:string-to-octets input
                     :external-format :utf-8)))))

(defmethod make-response ((s committed))
  (let ((input (committed-input s)))
    (format nil "~A ~A ~A~%"
            :committed
            input
            (length (sb-ext:string-to-octets input
                                             :external-format :utf-8)))))

(defun process-client (controller &key reader writer)
  (labels ((process-loop (state)
             (when-let ((line (funcall reader)))
               (let ((expr (as-expr line)))
                 (ecase (expr-op expr)
                   (:do-input
                       (let ((new-state
                               (transit-by-input controller
                                                 state
                                                 (expr-arg expr "code"))))
                         (let ((responce (make-response new-state)))
                           (funcall writer responce))
                         (process-loop new-state))))))))
    (process-loop (make-editing))))
