(defpackage :senn.fcitx.im.immutable
  (:use :cl)
  (:export :ime
           :ime-max-candidate-count
           :ime-kkc
           :ime-predictor
           :process-input
           :select-candidate
           :make-initial-state
           :resp
           :view->output))
(in-package :senn.fcitx.im.immutable)

(defun view->output (view)
  (let ((consumed-p (and view t)))
    (list consumed-p view)))

(defun resp (view &key state)
  (list (view->output view) state))

;;;

(defclass ime (senn.fcitx.im.state.inputting:mixin
               senn.fcitx.im.state.converting:mixin)
  ())

(defgeneric ime-max-candidate-count (ime)
  (:method ((ime ime))
    nil))

(defgeneric ime-kkc (ime))

(defgeneric ime-predictor (ime)
  (:method ((ime ime))
    nil))

;;;

(defmethod senn.fcitx.im.state.inputting:ime-max-candidate-count ((ime ime))
  (ime-max-candidate-count ime))

(defmethod senn.fcitx.im.state.inputting:ime-predictor ((ime ime))
  (ime-predictor ime))

(defmethod senn.fcitx.im.state.converting:ime-max-candidate-count ((ime ime))
  (ime-max-candidate-count ime))

(defmethod senn.fcitx.im.state.converting:ime-kkc ((ime ime))
  (ime-kkc ime))

;;;

(defgeneric process-input (ime state key)
  (:documentation "This method processes user (keyboard) inputs. This process is described by state transition that includes:
- Latin-to-Hiragana conversion
- Kana-Kanji Conversion"))

(defgeneric select-candidate (state index))

(defun make-initial-state ()
  (senn.fcitx.im.state.inputting:make-state))
