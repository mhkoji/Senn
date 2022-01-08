;; convert/lookup depending on a (third-party) kkc engine
(defpackage :senn.im.mixin.engine
  (:use :cl)
  (:export :convert
           :lookup
           :close-mixin
           :run-engine
           :kill-engine
           :with-engine
           :make-engine-runner
           :make-engine-store))
(in-package :senn.im.mixin.engine)

(defstruct engine-runner
  program args)

#+sbcl
(progn
  (defstruct engine
    process)

  (defun engine-convert (engine pron)
    (let ((p (engine-process engine)))
      (let ((input (sb-ext:process-input p)))
        (write-line (format nil "CONVERT ~A" pron) input)
        (force-output input))
      (let ((exp (read (sb-ext:process-output p) nil nil nil)))
        (destructuring-bind (logp . segs) exp
          (cons logp
                (mapcar (lambda (seg)
                          (destructuring-bind (form pron orig) seg
                            (list (symbol-name form)
                                  (symbol-name pron)
                                  (alexandria:make-keyword orig))))
                        segs))))))

  (defun engine-send-recv (engine line)
    (let ((p (engine-process engine)))
      (let ((input (sb-ext:process-input p)))
        (write-line line input)
        (force-output input))
      (read (sb-ext:process-output p) nil nil nil)))

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
    (ignore-errors
     (ext:terminate-process (engine-process engine) t)))

  (defun engine-send-recv (engine line)
    (let ((stream (engine-stream engine)))
      (write-line line stream)
      (force-output stream)
      (read stream nil nil nil))))

(defstruct engine-store engine engine-runner)

(defun engine-store-rerun (engine-store)
  (with-accessors ((engine engine-store-engine)
                   (runner engine-store-engine-runner)) engine-store
    (kill-engine engine)
    (setf engine (run-engine runner))))

;;;

(defun engine-convert (engine pron)
  (let ((exp (engine-send-recv
              engine
              (format nil "CONVERT ~A" pron))))
    (destructuring-bind (logp . segs) exp
      (cons logp
            (mapcar (lambda (seg)
                      (destructuring-bind (form pron orig) seg
                        (list (symbol-name form)
                              (symbol-name pron)
                              (alexandria:make-keyword orig))))
                    segs)))))

(defun engine-list-candidate (engine pron)
  (let ((cands (engine-send-recv
                engine
                (format nil "LIST_CANDIDATE ~A" pron))))
    (mapcar (lambda (cand)
              (destructuring-bind (logp form orig) cand
                (list logp
                      (symbol-name form)
                      (alexandria:make-keyword orig))))
            cands)))

(defmacro with-engine ((engine runner) &body body)
  `(let ((,engine (run-engine ,runner)))
     (unwind-protect (progn ,@body)
       (kill-engine ,engine))))



(defun convert (engine pron)
  (mapcar (lambda (seg)
            (destructuring-bind (form pron origin) seg
              (senn.segment:make-segment
               :pron pron
               :candidates (list (senn.segment:make-candidate
                                  :form form
                                  :origin origin))
               :current-index 0
               :has-more-candidates-p t)))
          (cdr (engine-convert engine pron))))

(defun lookup (engine pron)
  (mapcar (lambda (cand)
            (destructuring-bind (form origin) (cdr cand)
              (senn.segment:make-candidate
               :form form
               :origin origin)))
          (engine-list-candidate engine pron)))

;;;

(defclass convert ()
  ((engine-store
    :initarg :engine-store
    :reader engine-store)))

(defmethod senn.im:convert ((mixin convert) (pron string)
                            &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (with-accessors ((engine-store engine-store)) mixin
    (handler-case (convert (engine-store-engine engine-store) pron)
      (error ()
        (engine-store-rerun engine-store)
        (list (senn.segment:make-segment
               :pron pron
               :candidates (list (senn.segment:make-candidate
                                  :form pron :origin :um))
               :current-index 0
               :has-more-candidates-p t))))))

(defclass lookup ()
  ((engine-store
    :initarg :eingine-store
    :reader engine-store)))

(defmethod senn.im:lookup ((mixin lookup) (pron string)
                           &key prev next)
  (declare (ignore next prev))
  (with-accessors ((engine-store engine-store)) mixin
    (handler-case (lookup (engine-store-engine engine-store) pron)
      (error ()
        (engine-store-rerun engine-store)
        nil))))

(defun close-mixin (mixin)
  ;; Which engine-store method (convert or lookup) is called doesn't matter
  ;; when mixin is an instance of the convert and lookup classes.
  ;; Whichever method can work well because the convert and lookup classes share the same instance because the slot name is the same.
  (kill-engine (engine-store-engine (engine-store mixin))))
