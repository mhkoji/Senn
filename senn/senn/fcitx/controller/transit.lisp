(in-package :senn.fcitx.controller)

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
         (senn.segment:append-forms!
          (converting-current-segment s)
          (lambda (pron)
            (let ((words (senn.kkc:lookup (controller-kkc c) pron)))
              (mapcar #'senn.kkc:word-form words))))
         (senn.segment:try-move-cursor-pos!
          (converting-current-segment s)
          +1)
         s)
        ((= code +left-key+)
         (converting-move-curret-segment s -1))
        ((= code +right-key+)
         (converting-move-curret-segment s +1))
        (t
         (make-committed :input (converting-current-input s)))))


(defmethod transit-by-input ((c controller)
                             (s editing)
                             code)
  (cond ((= code +space-key+)
         (let ((pronunciation (senn.buffer:buffer-string
                               (editing-buffer s))))
           (let ((words (senn.kkc:convert (controller-kkc c)
                                          pronunciation)))
             (let ((segments
                    (mapcar (lambda (w)
                              (senn.segment:make-segment
                               :pron (senn.kkc:word-pron w)
                               :forms (list (senn.kkc:word-form w))
                               :has-more-forms-p t
                               :current-index 0))
                            words)))
               (make-converting :segments segments
                                :pronunciation pronunciation)))))
        ((= code +backspace-key+)
         (if (string= (senn.buffer:buffer-string (editing-buffer s))
                      "")
             (list s nil)
             (progn
               (setf (editing-buffer s)
                     (senn.buffer:delete-char (editing-buffer s)))
               s)))
        ((= code +enter-key+)
         (make-committed :input (senn.buffer:buffer-string
                                 (editing-buffer s))))
        ((= code +left-key+)
         (setf (editing-buffer s)
               (senn.buffer:move-cursor-pos (editing-buffer s) -1))
         s)
        ((= code +right-key+)
         (setf (editing-buffer s)
               (senn.buffer:move-cursor-pos (editing-buffer s) +1))
         s)
        (t
         (setf (editing-buffer s)
               (senn.buffer:insert-char (editing-buffer s)
                                        (code-char code)))
         s)))
