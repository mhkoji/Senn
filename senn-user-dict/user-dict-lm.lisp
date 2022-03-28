(in-package :senn-user-dict)

(defmethod hachee.kkc.impl.lm.ex-dict-builder:item-pron ((item entry))
  (entry-pron item))

(defmethod hachee.kkc.impl.lm.ex-dict-builder:item-form ((item entry))
  (entry-form item))

(defmethod hachee.kkc.impl.lm.ex-dict-builder:list-items ((source dict))
  (dict-entries source))
