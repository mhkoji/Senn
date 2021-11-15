;; convert/lookup depending on a certain third-party kkc engine
(defpackage :senn.im.engine
  (:use :cl)
  (:export :convert
           :lookup
           :with-engine
           :make-engine-runner))
(in-package :senn.im.engine)

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

(defun engine-list-candidate (engine pron)
  (let ((p (engine-process engine)))
    (let ((input (sb-ext:process-input p)))
      (write-line (format nil "LIST_CANDIDATE ~A" pron) input)
      (force-output input))
    (let ((cands (read (sb-ext:process-output p) nil nil nil)))
      (mapcar (lambda (cand)
                  (destructuring-bind (logp form orig) cand
                    (list logp
                          (symbol-name form)
                          (alexandria:make-keyword orig))))
              cands))))

(defstruct engine-runner
  program args)

(defun run-engine (runner)
  (let ((p (sb-ext:run-program
            (engine-runner-program runner)
            (engine-runner-args runner)
            :input :stream
            :output :stream
            :error t
            :external-format :eucjp
            :wait nil)))
    (let ((output (sb-ext:process-output p)))
      (loop while (listen output)
            do (read-line output nil nil nil)))
    (make-engine :process p)))

(defmacro with-engine ((engine runner) &body body)
  `(let ((,engine (run-engine ,runner)))
     (unwind-protect (progn ,@body)
       (sb-ext:process-kill (engine-process ,engine) 9))))

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
