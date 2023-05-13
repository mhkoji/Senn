;; convert/list-candidates depending on a (third-party) kkc engine
(defpackage :senn.im.kkc.engine
  (:use :cl)
  (:export :make-engine-runner
           :kkc
           :start-kkc
           :close-kkc))
(in-package :senn.im.kkc.engine)

(defgeneric store-get-engine (engine-store))
(defgeneric store-restart-engine (engine-store))

(defclass kkc ()
  ((engine-store
    :initarg :engine-store
    :reader engine-store)))

(defmethod senn.im.kkc:convert ((kkc kkc) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (with-accessors ((engine-store engine-store)) kkc
    (handler-case (senn.im.kkc.request:convert
                   (store-get-engine engine-store)
                   pron)
      (error (c)
        (log:warn c)
        (store-restart-engine engine-store)
        (list (senn.im.kkc:make-segment
               :pron pron
               :candidates (list (senn.im.kkc:make-candidate
                                  :form pron))))))))

(defmethod senn.im.kkc:list-candidates ((kkc kkc) (pron string))
  (with-accessors ((engine-store engine-store)) kkc
    (handler-case (senn.im.kkc.request:list-candidates
                   (store-get-engine engine-store)
                   pron)
      (error (c)
        (log:warn c)
        (store-restart-engine engine-store)
        nil))))

;;;

(defstruct engine-runner
  program args)

#+sbcl
(progn
  (defstruct engine
    process)

  (defun engine-send-recv (engine line)
    (let ((p (engine-process engine)))
      (let ((input (sb-ext:process-input p)))
        (write-line line input)
        (force-output input))
      (read-line (sb-ext:process-output p) nil nil nil)))

  (defun run-engine (runner)
    (let ((p (sb-ext:run-program
              (engine-runner-program runner)
              (engine-runner-args runner)
              :input :stream
              :output :stream
              :error t
              :external-format :utf8
              :wait nil)))
      (let ((output (sb-ext:process-output p)))
        (loop while (listen output)
              do (read-line output nil nil nil)))
      (make-engine :process p)))

  (defun kill-engine (engine)
    (ignore-errors
      (sb-ext:process-kill (engine-process engine) 9))))

;;;

#+ecl
(progn
  (defstruct engine
    stream process)

  (defun run-engine (runner)
    (multiple-value-bind (stream code process)
        (ext:run-program (engine-runner-program runner)
                         (engine-runner-args runner)
                         :input :stream
                         :output :stream
                         :error t
                         :external-format :utf8
                         :wait nil)
      (declare (ignore code))
      (make-engine :stream stream :process process)))

  (defun kill-engine (engine)
    (let ((process (engine-process engine)))
      (ext:terminate-process process t)
      ;; Wait the process to finish to prevent it from becoming a zombie.
      (ext:external-process-wait process t)))

  (defun engine-send-recv (engine line)
    (let ((stream (engine-stream engine)))
      (write-line line stream)
      (force-output stream)
      (read-line stream nil nil nil))))

;;;

(defmethod senn.im.kkc.request:send-line ((agent engine) (line string))
  (engine-send-recv agent line))

(defstruct engine-store engine engine-runner)

(defmethod store-get-engine ((store engine-store))
  (engine-store-engine store))

(defmethod store-restart-engine ((store engine-store))
  (with-accessors ((engine engine-store-engine)
                   (runner engine-store-engine-runner)) store
    (kill-engine engine)
    (setf engine (run-engine runner))))

;;;

(defun start-kkc (runner)
  (let ((initial-engine (run-engine runner)))
    (let ((store (make-engine-store
                  :engine initial-engine
                  :engine-runner runner)))
      (make-instance 'kkc :engine-store store))))

(defun close-kkc (kkc)
  (kill-engine (engine-store-engine (engine-store kkc))))
