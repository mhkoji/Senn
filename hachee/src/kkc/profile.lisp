(defpackage :hachee.kkc.profile
  (:use :cl)
  (:export :execute))
(in-package :hachee.kkc.profile)

(defun execute (kkc)
  (sb-profile:profile . #.(mapcar #'string
                                  (list :cl
                                        :hachee.kkc
                                        :hachee.kkc.convert
                                        :hachee.kkc.convert.viterbi
                                        :hachee.kkc.full.score-fns
                                        :hachee.kkc.simple
                                        :hachee.kkc.dictionary
                                        :hachee.language-mdoel.vocabulary
                                        :hachee.language-model.freq
                                        :hachee.language-model.n-gram)))
  (hachee.kkc.convert:execute kkc "あおぞらぶんこ")
  (hachee.kkc.convert:execute kkc "とうきょうにいきました")
  (hachee.kkc.convert:execute kkc "きょうはいいてんきですね")
  (sb-profile:report))
