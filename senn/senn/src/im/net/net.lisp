(defpackage :senn.im.net
  (:use :cl :hachee.ipc.op)
  (:export :make-ime
           :read-message
           :send-message
           :loop-handling-request)
  (:import-from :alexandria
                :when-let))
(in-package :senn.im.net)

(defgeneric read-message (conn))
(defgeneric send-message (conn msg))

(defclass connection-holder ()
  ((connection :initarg :connection
               :accessor connection)))

(defun reconnectable-request (conn-holder msg reconnect-fn)
  (labels ((request (try-count)
             (send-message (connection conn-holder) msg)
             (let ((resp (read-message (connection conn-holder))))
               (cond (resp resp)
                     ((< try-count 2)
                      (log:info "Reconnecting")
                      (setf (connection conn-holder) (funcall reconnect-fn))
                      (log:info "Reconnected")
                      (request (1+ try-count)))
                     (t
                      (error "Failed to send request"))))))
    (log:info "Request: ~A" msg)
    (let ((resp (request 0)))
      (log:info "Response: ~A" resp)
      resp)))

;;; Client
(defclass client-ime (senn.im:ime
                      connection-holder)
  ((server-reconnect-fn
    :initarg :reconnect-fn
    :reader server-reconnect-fn)))

(defun make-ime (reconnect-fn)
  (let ((conn (funcall reconnect-fn)))
    (make-instance 'client-ime
     :connection conn
     :reconnect-fn reconnect-fn)))

(defmethod senn.im:convert ((ime client-ime) (pron string)
                            &key 1st-boundary-index)
  (let ((response
         (reconnectable-request
          ime
          (jsown:to-json
           (jsown:new-js
             ("op"   "convert")
             ("args" (jsown:new-js
                       ("text"               pron)
                       ("1st-boundary-index" 1st-boundary-index)))))
          (server-reconnect-fn ime))))
    (mapcar (lambda (s)
              (senn.segment:make-segment
               :pron (jsown:val s "pron")
               :candidates
               (list (senn.segment:make-candidate
                      :form (jsown:val s "form")
                      :origin (jsown:val s "origin")))
               :has-more-candidates-p t
               :current-index 0))
            (jsown:parse response))))

(defmethod senn.im:lookup ((ime client-ime) (pron string)
                           &key prev next)
  (declare (ignore next prev))
  (let ((response
         (reconnectable-request
          ime
          (jsown:to-json
           (jsown:new-js
             ("op"   "lookup")
             ("args" (jsown:new-js ("text" pron)))))
          (server-reconnect-fn ime))))
    (mapcar (lambda (c)
              (senn.segment:make-candidate
               :form (jsown:val c "form")
               :origin (jsown:val c "origin")))
            (jsown:parse response))))

(defmethod senn.im:predict ((ime client-ime) (pron string))
  (let ((response
         (reconnectable-request
          ime
          (jsown:to-json
           (jsown:new-js
             ("op"   "predict")
             ("args" (jsown:new-js ("text" pron)))))
          (server-reconnect-fn ime))))
    (when response
      (jsown:parse response))))

;;; Server
(defun expr->word (expr)
  (hachee.kkc.word:make-word
   :pron (jsown:val expr "pron")
   :form (jsown:val expr "form")))

(defun handle-request (expr ime)
  (ecase (expr-op expr)
    (:convert
     ;; {"op": "convert", "args": {"text": "あおぞらぶんこ"}}
     (let ((segments
            (senn.im:convert ime (expr-arg expr "text")
             :1st-boundary-index (expr-arg expr "1st-boundary-index"))))
       (jsown:to-json
        (mapcar (lambda (seg)
                  (let ((cand (car (senn.segment:segment-candidates seg))))
                    (jsown:new-js
                      ("pron" (senn.segment:segment-pron seg))
                      ("form" (senn.segment:candidate-form cand))
                      ("origin" (senn.segment:candidate-origin cand)))))
                segments))))
    (:lookup
     ;; {"op": "lookup", "args": {"text": "あお"}}
     (let ((candidates
            (senn.im:lookup ime (expr-arg expr "text")
             :prev (let ((prev (expr-arg expr "prev")))
                     (when prev (expr->word prev)))
             :next (let ((next (expr-arg expr "next")))
                     (when next (expr->word next))))))
       (jsown:to-json
        (mapcar (lambda (cand)
                  (jsown:new-js
                    ("form" (senn.segment:candidate-form cand))
                    ("origin" (senn.segment:candidate-origin cand))))
                candidates))))
    (:predict
     (let ((strings
            (senn.im:predict ime (expr-arg expr "text"))))
       (jsown:to-json strings)))))

(defun loop-handling-request (ime client-conn)
  (when-let ((msg (read-message client-conn)))
    (let ((expr (hachee.ipc.op:as-expr msg)))
      (send-message client-conn (handle-request expr ime)))
    (loop-handling-request ime client-conn)))
