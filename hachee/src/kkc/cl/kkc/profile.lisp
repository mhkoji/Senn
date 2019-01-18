(defpackage :hachee.kkc.profile
  (:use :cl)
  (:import-from :hachee.kkc
                :profile))
(in-package :hachee.kkc.profile)

(defun profile (kkc)
  (sb-profile:profile . #.(mapcar #'string
                                  (list :hachee.kkc
                                        :hachee.kkc.convert
                                        :hachee.kkc.convert.score-fns
                                        :hachee.kkc.word
                                        :hachee.kkc.word.dictionary
                                        :hachee.kkc.word.vocabulary
                                        :hachee.language-model.freq
                                        :hachee.language-model.n-gram)))
  (hachee.kkc:convert kkc "あおぞらぶんこ")
  (hachee.kkc:convert kkc "とうきょうにいきました")
  (hachee.kkc:convert kkc "きょうはいいてんきですね")
  (sb-profile:report))
