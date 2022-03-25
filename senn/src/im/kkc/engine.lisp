;; convert/list-candidates depending on a (third-party) kkc engine
(defpackage :senn.im.kkc.engine
  (:use :cl)
  (:export :list-candidates
           :kkc
           :close-kkc
           :run-engine
           :kill-engine
           :make-engine-runner
           :make-engine-store))
(in-package :senn.im.kkc.engine)

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

(defstruct engine-store engine engine-runner)

(defun engine-store-rerun (engine-store)
  (with-accessors ((engine engine-store-engine)
                   (runner engine-store-engine-runner)) engine-store
    (kill-engine engine)
    (setf engine (run-engine runner))))

;;;

(defmethod senn.im.kkc.request:send-line ((agent engine) (line string))
  (engine-send-recv agent line))

;;;

(defclass kkc ()
  ((engine-store
    :initarg :engine-store
    :reader engine-store)))

(defmethod senn.im.kkc:convert ((kkc kkc) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (with-accessors ((engine-store engine-store)) kkc
    (handler-case (senn.im.kkc.request:convert
                   (engine-store-engine engine-store)
                   pron)
      (error ()
        (engine-store-rerun engine-store)
        (list (senn.im.kkc:make-segment
               :pron pron
               :candidates (list (senn.im.kkc:make-candidate
                                  :form pron))))))))
               
(defmethod senn.im.kkc:list-candidates ((kkc kkc) (pron string))
  (with-accessors ((engine-store engine-store)) kkc
    (handler-case (senn.im.kkc.request:list-candidates
                   (engine-store-engine engine-store)
                   pron)
      (error ()
        (engine-store-rerun engine-store)
        nil))))

(defun close-kkc (kkc)
  (kill-engine (engine-store-engine (engine-store kkc))))
