(defpackage :senn.im.net.client
  (:use :cl)
  (:export :make-ime
           :read-message
           :send-message))
(in-package :senn.im.net.client)

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

(defclass ime (senn.im:ime
               connection-holder)
  ((server-reconnect-fn
    :initarg :reconnect-fn
    :reader server-reconnect-fn)))

(defun make-ime (reconnect-fn)
  (let ((conn (funcall reconnect-fn)))
    (make-instance 'ime
     :connection conn
     :reconnect-fn reconnect-fn)))

(defmethod senn.im:convert ((ime ime) (pron string) &key 1st-boundary-index)
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

(defmethod senn.im:lookup ((ime ime) (pron string) &key prev next)
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

(defmethod senn.im:predict ((ime ime) (pron string))
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
