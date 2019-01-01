(in-package :hachee.input-method.fcitx.controller)

(defvar +space-key+ 32)
(defvar +backspace-key+ 65288)
(defvar +enter-key+ 65293)
(defvar +left-key+  65361)
(defvar +right-key+ 65363)



(defmethod transit-by-input ((c controller)
                             (s committed)
                             code)
  (transit-by-input c (make-editing) code))


(defmethod transit-by-input ((c controller)
                             (s converting)
                             code)
  (cond ((= code +space-key+)
         (let ((segment (converting-current-segment s)))
           (when (segment-has-more-forms-p segment)
             (let ((words (hachee.kkc:lookup (controller-kkc c)
                                             (segment-pron segment))))
               (segment-append-forms!
                segment
                (mapcar #'hachee.kkc:word-form words))))
           (segment-try-move-cursor-pos! segment +1))
         s)
        ((= code +left-key+)
         (let ((segment (converting-current-segment s)))
           (segment-try-move-cursor-pos! segment -1))
         s)
        ((= code +right-key+)
         (let ((segment (converting-current-segment s)))
           (segment-try-move-cursor-pos! segment +1))
         s)
        (t
         (make-committed :input (converting-current-input s)))))


(defmethod transit-by-input ((c controller)
                             (s editing)
                             code)
  (cond ((= code +space-key+)
         (let ((pronunciation (editing-buffer s)))
           (let ((words (hachee.kkc:convert (controller-kkc c)
                                            pronunciation)))
             (make-converting
              :segments (mapcar (lambda (w)
                                  (make-segment
                                   :pron (hachee.kkc:word-pron w)
                                   :forms (list (hachee.kkc:word-form w))
                                   :has-more-forms-p t
                                   :current-index 0))
                                words)
              :pronunciation pronunciation))))
        ((= code +backspace-key+)
         (editing-delete-char s))
        ((= code +enter-key+)
         (make-committed :input (editing-buffer s)))
        ((= code +left-key+)
         (editing-try-move-cursor-pos s -1))
        ((= code +right-key+)
         (editing-try-move-cursor-pos s +1))
        (t
         (editing-insert-char s (code-char code)))))
