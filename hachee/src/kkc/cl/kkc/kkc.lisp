(defpackage :hachee.kkc
  (:use :cl)
  (:export :+origin-vocabulary+
           :+origin-extended-dictionary+
           :+origin-corpus+
           :+origin-resource+
           :+origin-out-of-dictionary+
           :+origin-tankan+

           :kkc
           :kkc-extended-dictionary
           :save-kkc
           :load-kkc
           :save-to-files
           :make-from-files
           :convert
           :get-convert-score-fn
           :lookup
           :get-lookup-score-fn
           :profile))
(in-package :hachee.kkc)

(defparameter +origin-vocabulary+          :vocabulary)
(defparameter +origin-extended-dictionary+ :extended-dictionary)
(defparameter +origin-corpus+              :corpus)
(defparameter +origin-resource+            :resource)
(defparameter +origin-out-of-dictionary+   :out-of-dictionary)
(defparameter +origin-tankan+              :tankan)

;; word-pron pair n-gram model
(defstruct kkc
  n-gram-model
  vocabulary
  word-dictionary
  char-dictionary
  extended-dictionary)

(defgeneric save-to-files (kkc add-file-fn))

(defun save-kkc (kkc pathname)
  (zip:with-output-to-zipfile (writer pathname)
    (labels ((add-file (name data-string)
               (flexi-streams:with-input-from-sequence
                   (data-stream (flexi-streams:string-to-octets
                                 data-string
                                 :external-format :utf-8))
                 (zip:write-zipentry writer name data-stream
                                     :file-write-date
                                     (get-universal-time)))))
      (add-file "type.txt"
                (let ((sym (type-of kkc)))
                  (format nil "~A::~A"
                          (package-name (symbol-package sym))
                          (symbol-name sym))))
      (save-to-files kkc #'add-file)))
  (values))


(defgeneric make-from-files (kkc-type read-from-file-fn))

(defun load-kkc (pathname)
  (zip:with-zipfile (zip pathname)
    (labels ((read-from-file (filename read-from-stream-fn)
               (let ((entry (zip:get-zipfile-entry filename zip)))
                 (when entry
                   (let ((octets (zip:zipfile-entry-contents entry)))
                     (let ((string (babel:octets-to-string
                                    octets
                                    :encoding :utf-8)))
                       (with-input-from-string (s string)
                         (funcall read-from-stream-fn s))))))))
      (let ((kkc-type (read-from-file "type.txt"
                                      (lambda (s) (read s)))))
        (make-from-files kkc-type #'read-from-file)))))


;;; Convert
(defgeneric get-convert-score-fn (kkc)
  (:documentation "Returns a score function for conversion"))

(defun make-unknown-word-unit (pron)
  (hachee.kkc.dictionary:make-unit
   :pron pron
   :form (hachee.ja:hiragana->katakana pron)))

(defun list-entries (sub-pron &key dictionaries vocabulary)
  (let ((entries
         (mapcar
          (lambda (dictionary-entry)
            (let ((unit (hachee.kkc.dictionary:entry-unit dictionary-entry)))
              (hachee.kkc.convert:make-entry
               :unit unit
               :token (hachee.language-model.vocabulary:to-int-or-unk
                       vocabulary
                       (hachee.kkc.dictionary:unit->key unit))
               :origin
               (hachee.kkc.dictionary:entry-origin dictionary-entry))))
          (alexandria:mappend (lambda (dict)
                                (hachee.kkc.dictionary:lookup dict sub-pron))
                              dictionaries))))
    ;; Add unknown word node if necessary
    (when (< (length sub-pron) 8) ;; Length up to 8
      (let ((unk-unit (make-unknown-word-unit sub-pron)))
        (when (not (some (lambda (dict)
                           (hachee.kkc.dictionary:contains-p dict unk-unit))
                         dictionaries))
          (push (hachee.kkc.convert:make-entry
                 :unit unk-unit
                 :token (hachee.language-model.vocabulary:to-int
                         vocabulary
                         hachee.language-model.vocabulary:+UNK+)
                 :origin +origin-out-of-dictionary+)
                entries))))
    (nreverse entries)))

(defun convert (kkc pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi:execute pronunciation
   :begin-entry
   (hachee.kkc.convert:make-entry
    :unit hachee.language-model.vocabulary:+BOS+
    :token (hachee.language-model.vocabulary:to-int
            (kkc-vocabulary kkc)
            hachee.language-model.vocabulary:+BOS+)
    :origin +origin-vocabulary+)
   :end-entry
   (hachee.kkc.convert:make-entry
    :unit hachee.language-model.vocabulary:+EOS+
    :token (hachee.language-model.vocabulary:to-int
            (kkc-vocabulary kkc)
            hachee.language-model.vocabulary:+EOS+)
    :origin +origin-vocabulary+)
   :score-fn
   (get-convert-score-fn kkc)
   :list-entries-fn
   (lambda (sub-pron)
     (list-entries sub-pron
                   :dictionaries (list (kkc-word-dictionary kkc)
                                       (kkc-extended-dictionary kkc))
                   :vocabulary (kkc-vocabulary kkc)))
   :1st-boundary-index 1st-boundary-index))


;;; Lookup
(defgeneric get-lookup-score-fn (kkc prev-word next-word)
  (:documentation "Returns a score function for lookup"))

(defmethod get-lookup-score-fn ((kkc kkc) prev-word next-word)
  nil)

(defun lookup (kkc pronunciation &key prev next)
  (hachee.kkc.lookup:execute pronunciation
   :score-fn (when (and next prev)
               (get-lookup-score-fn kkc prev next))
   :word-dict (kkc-word-dictionary kkc)
   :char-dict (kkc-char-dictionary kkc)))
