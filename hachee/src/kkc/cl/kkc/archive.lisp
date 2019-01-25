(defpackage :hachee.kkc.archive
  (:use :cl)
  (:shadow :load)
  (:import-from :hachee.language-model.n-gram
                :model
                :load-model
                :save-model)
  (:import-from :hachee.kkc.word.dictionary
                :save-dictionary
                :load-dictionary)
  (:import-from :hachee.kkc.word.vocabulary
                :save-vocabulary
                :load-vocabulary)
  (:export :save :load))
(in-package :hachee.kkc.archive)

(defun save (pathname &key vocabulary language-model dictionary)
  (zip:with-output-to-zipfile (writer pathname)
    (labels ((add-to-zip (name data-string)
               (flexi-streams:with-input-from-sequence
                   (data-stream (flexi-streams:string-to-octets
                                 data-string
                                 :external-format :utf-8))
                 (zip:write-zipentry writer name data-stream
                                     :file-write-date
                                     (get-universal-time)))))
      (add-to-zip "language-model.txt"
                  (with-output-to-string (s)
                    (save-model language-model s)))
      (add-to-zip "vocabulary.txt"
                  (with-output-to-string (s)
                    (save-vocabulary vocabulary s)))
      (add-to-zip "dictionary.txt"
                  (with-output-to-string (s)
                    (save-dictionary dictionary s)))))
  (values))

(defun load (pathname)
  (zip:with-zipfile (zip pathname)
    (labels ((entry-as-string (name)
               (let ((octets (zip:zipfile-entry-contents
                              (zip:get-zipfile-entry name zip))))
                 (sb-ext:octets-to-string octets :external-format :utf-8))))
      (list
       :language-model
       (with-input-from-string (s (entry-as-string "language-model.txt"))
         (load-model 'model s))
       :vocabulary
       (with-input-from-string (s (entry-as-string "vocabulary.txt"))
         (load-vocabulary s))
       :dictionary
       (with-input-from-string (s (entry-as-string "dictionary.txt"))
         (load-dictionary s))))))
