;; convert/lookup depending on a (third-party) kkc engine
(defpackage :senn.im.mixin.engine
  (:use :cl)
  (:export :convert
           :lookup
           :run-engine
           :kill-engine
           :with-engine
           :make-engine-runner))
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
    (sb-ext:process-kill (engine-process engine) 9)))

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
    (ext:terminate-process (engine-process engine) t))

  (defun engine-send-recv (engine line)
    (let ((stream (engine-stream engine)))
      (write-line line stream)
      (force-output stream)
      (read stream nil nil nil))))

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
          (handler-case
              (cdr (engine-convert engine pron))
            (error ()
              (list (list pron pron :um))))))

(defun lookup (engine pron)
  (handler-case
      (mapcar (lambda (cand)
                (destructuring-bind (form origin) (cdr cand)
                  (senn.segment:make-candidate
                   :form form
                   :origin origin)))
              (engine-list-candidate engine pron))
    (error ()
      (list (senn.segment:make-candidate :form pron :origin :um)))))

;;;

(defclass convert ()
  ((engine-impl
    :initarg :convert-engine-impl
    :reader convert-engine-impl)))

(defmethod senn.im:convert ((mixin convert) (pron string)
                            &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (convert (convert-engine-impl mixin) pron))

(defclass lookup ()
  ((engine-impl
    :initarg :lookup-engine-impl
    :reader lookup-engine-impl)))

(defmethod senn.im:lookup ((mixin lookup) (pron string)
                           &key prev next)
  (declare (ignore next prev))
  (lookup (lookup-engine-impl mixin) pron))
