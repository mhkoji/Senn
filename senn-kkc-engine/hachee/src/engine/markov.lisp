(defpackage :senn-kkc-engine.hachee.engine.markov
  (:use :cl)
  (:export :set-kkc
           :main)
  (:local-nicknames (:user-dict :senn-kkc-engine.hachee.user-dict)))
(in-package :senn-kkc-engine.hachee.engine.markov)

;;; user-dict

(defmethod hachee.kkc.impl.markov.ex-dict-builder:item-pron
    ((item user-dict:entry))
  (user-dict:entry-pron item))

(defmethod hachee.kkc.impl.markov.ex-dict-builder:item-form
    ((item user-dict:entry))
  (user-dict:entry-form item))

(defmethod hachee.kkc.impl.markov.ex-dict-builder:list-items
    ((source user-dict:dict))
  (user-dict:dict-entries source))

(defun kkc-apply-user-dict (kkc)
  (let ((dict (user-dict:read-file)))
    (when dict
      (hachee.kkc.impl.markov:kkc-set-ex-dict kkc dict))))

;;;

(defvar *kkc*)

(defun kkc-dir-pathname ()
  (merge-pathnames ".senn/markov/" (user-homedir-pathname)))

(defun set-kkc (&optional dir)
  (setq *kkc* (hachee.kkc.impl.markov.read:read-kkc-dir
               (or dir (kkc-dir-pathname))))
  (values))

(defun main ()
  #-win32
  (handler-case
      (let ((dirs (list
                   "/usr/lib/senn/fcitx/kkc/"    ;; for fcitx
                   "/usr/lib/senn/ibus/kkc/")))  ;; for ibus
        (user-dict:with-library-loaded (dirs)
          (kkc-apply-user-dict *kkc*)))
    (error (e)
      (format *standard-output* "~A~%" e)))
  (labels ((handle (line)
             (senn-kkc-engine.hachee.engine:handle line *kkc*)))
    #-win32
    (senn-ipc.server.stdio:start-server #'handle)
    #+win32
    (senn-ipc.server.named-pipe:start-server
     (lambda (client)
       (senn-ipc.server:client-loop client :handle-fn #'handle))
     :pipe-name "\\\\.\\Pipe\\senn\\senn-kkc-engine")))
