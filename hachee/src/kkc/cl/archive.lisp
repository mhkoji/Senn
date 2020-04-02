(defpackage :hachee.kkc.archive
  (:use :cl)
  (:shadow :load)
  (:import-from :hachee.language-model.n-gram
                :load-model
                :save-model)
  (:import-from :hachee.kkc.word.dictionary
                :save-dictionary
                :load-dictionary)
  (:import-from :hachee.language-model.vocabulary
                :save-vocabulary
                :load-vocabulary)
  (:export :save :load))
(in-package :hachee.kkc.archive)

(defgeneric save-object (obj stream))

(defmethod save-object ((obj hachee.language-model.n-gram:model)
                        stream)
  (save-model obj stream))

(defmethod save-object ((obj hachee.kkc.word.dictionary:dictionary)
                        stream)
  (save-dictionary obj stream))

(defmethod save-object ((obj hachee.language-model.vocabulary:vocabulary)
                        stream)
  (save-vocabulary obj stream))

(defun save (pathname &key n-gram-model
                           vocabulary
                           vocabulary-dictionary
                           extended-dictionary
                           word-dictionary
                           tankan-dictionary
                           unknown-word-vocabulary
                           unknown-word-n-gram-model)
  (zip:with-output-to-zipfile (writer pathname)
    (labels ((add-to-zip (name data-string)
               (flexi-streams:with-input-from-sequence
                   (data-stream (flexi-streams:string-to-octets
                                 data-string
                                 :external-format :utf-8))
                 (zip:write-zipentry writer name data-stream
                                     :file-write-date
                                     (get-universal-time)))))
      (add-to-zip (if (typep n-gram-model
                             'hachee.language-model.n-gram:class-model)
                      "class-n-gram-model.txt"
                      "n-gram-model.txt")
                  (with-output-to-string (stream)
                    (save-model n-gram-model stream)))
      (loop for (filename object) in (list
                                      (list "vocabulary.txt"
                                            vocabulary)
                                      (list "vocabulary-dictionary.txt"
                                            vocabulary-dictionary)
                                      (list "extended-dictionary.txt"
                                            extended-dictionary)
                                      (list "word-dictionary.txt"
                                            word-dictionary)
                                      (list "tankan-dictionary.txt"
                                            tankan-dictionary)
                                      (list "unknown-word-vocabulary.txt"
                                            unknown-word-vocabulary)
                                      (list "unknown-word-n-gram-model.txt"
                                            unknown-word-n-gram-model))
            do (progn
                 (add-to-zip filename (with-output-to-string (s)
                                        (save-object object s)))))))
  (values))


(defgeneric load-object-as (type stream))

(defmethod load-object-as
    ((type (eql 'hachee.language-model.n-gram:model))
     stream)
  (load-model 'hachee.language-model.n-gram:model stream))

(defmethod load-object-as
    ((type (eql 'hachee.kkc.word.dictionary:dictionary))
     stream)
  (load-dictionary stream))

(defmethod load-object-as
    ((type (eql 'hachee.language-model.vocabulary:vocabulary))
     stream)
  (load-vocabulary stream))

(defun load (pathname)
  (zip:with-zipfile (zip pathname)
    (labels ((entry-as-string (name)
               (let ((entry (zip:get-zipfile-entry name zip)))
                 (let ((octets (zip:zipfile-entry-contents entry)))
                   (babel:octets-to-string octets :encoding :utf-8)))))
      (alexandria:alist-plist
       (cons
        (cons :n-gram-model
              (if (zip:get-zipfile-entry "class-n-gram-model.txt" zip)
                  (with-input-from-string (s (entry-as-string
                                              "class-n-gram-model.txt"))
                    (load-model 'hachee.language-model.n-gram:class-model s))
                  (with-input-from-string (s (entry-as-string
                                              "n-gram-model.txt"))
                    (load-model 'hachee.language-model.n-gram:model s))))
        (loop for (key filename type)
                  in (list
                      (list :vocabulary
                            "vocabulary.txt"
                            'hachee.language-model.vocabulary:vocabulary)
                      (list :vocabulary-dictionary
                            "vocabulary-dictionary.txt"
                            'hachee.kkc.word.dictionary:dictionary)
                      (list :extended-dictionary
                            "extended-dictionary.txt"
                            'hachee.kkc.word.dictionary:dictionary)
                      (list :word-dictionary
                            "word-dictionary.txt"
                            'hachee.kkc.word.dictionary:dictionary)
                      (list :tankan-dictionary
                            "tankan-dictionary.txt"
                            'hachee.kkc.word.dictionary:dictionary)
                      (list :unknown-word-vocabulary
                            "unknown-word-vocabulary.txt"
                            'hachee.language-model.vocabulary:vocabulary)
                      (list :unknown-word-n-gram-model
                            "unknown-word-n-gram-model.txt"
                            'hachee.language-model.n-gram:model))
              for object = (with-input-from-string
                               (s (entry-as-string filename))
                             (load-object-as type s))
              collect (cons key object)))))))
