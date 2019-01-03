(defpackage :hachee.t.scenario.dependency-parsing.shift-reduce
  (:use :cl :hachee.dependency-parsing.shift-reduce)
  (:export :list-ordered-vertices-from-arcs
           :search-oracle-action-sequence-typical
           :search-oracle-action-sequence-on-parsed-text
           :search-oracle-action-sequence-on-parsed-text-removing-arc-manually
           :parse-text
           :parse-on-parsed-text
           :parse-on-parsed-text-removing-arc-manually))
(in-package :hachee.t.scenario.dependency-parsing.shift-reduce)

(defmacro list-ordered-vertices-from-arcs (&key test)
  `(,test
    (equal (hachee.dependency-parsing.shift-reduce::list-ordered-vertices
            (list (make-arborescence-arc :child 2 :parent 3)
                  (make-arborescence-arc :child 1 :parent 2)
                  (make-arborescence-arc :child 5 :parent 2)
                  (make-arborescence-arc :child 7 :parent 5)
                  (make-arborescence-arc :child 6 :parent 7)
                  (make-arborescence-arc :child 4 :parent 3)
                  (make-arborescence-arc :child 8 :parent 4)
                  (make-arborescence-arc :child 9 :parent 3)))
           (list 1 2 5 6 7 3 4 8 9))))

(defmacro search-oracle-action-sequence-typical  (&key test)
  `(,test
    (equal
     (hachee.dependency-parsing.shift-reduce::search-oracle-action-sequence
      (list (make-arborescence-arc :child 2 :parent 3)
            (make-arborescence-arc :child 1 :parent 2)
            (make-arborescence-arc :child 5 :parent 2)
            (make-arborescence-arc :child 7 :parent 5)
            (make-arborescence-arc :child 6 :parent 7)
            (make-arborescence-arc :child 4 :parent 3)
            (make-arborescence-arc :child 8 :parent 4)
            (make-arborescence-arc :child 9 :parent 3)))
     (list
      :shift :shift :left-arc :shift :shift :shift :swap :swap
      :shift :shift :shift :swap :swap :shift :shift :shift :swap
      :swap :left-arc :right-arc :right-arc :shift :left-arc
      :shift :shift :right-arc :right-arc :shift :right-arc :right-arc))))

(defmacro parse-text (&key test)
  `(let* ((gold-arborescence-arc-list
           (list (make-arborescence-arc :child 2 :parent 3)
                 (make-arborescence-arc :child 1 :parent 2)
                 (make-arborescence-arc :child 5 :parent 2)
                 (make-arborescence-arc :child 7 :parent 5)
                 (make-arborescence-arc :child 6 :parent 7)
                 (make-arborescence-arc :child 4 :parent 3)
                 (make-arborescence-arc :child 8 :parent 4)
                 (make-arborescence-arc :child 9 :parent 3)))
          (actions
           (list
            :shift :shift :left-arc :shift :shift :shift :swap :swap
            :shift :shift :shift :swap :swap :shift :shift :shift :swap
            :swap :left-arc :right-arc :right-arc :shift :left-arc
            :shift :shift :right-arc :right-arc :shift :right-arc :right-arc))
          (actual-arborescence-arc-list
           (parse (list 1 2 3 4 5 6 7 8 9)
            :action-fn
            (lambda (c a)
              (declare (ignore c a))
              (pop actions)))))
     (,test (null (set-difference gold-arborescence-arc-list
                                  actual-arborescence-arc-list
                                  :test #'arborescence-arc=)))
     (,test (null (set-difference actual-arborescence-arc-list
                                  gold-arborescence-arc-list
                                  :test #'arborescence-arc=)))))


(defvar *arc-list*
  (list (make-arborescence-arc :child 29 :parent 32)
        (make-arborescence-arc :child 31 :parent 32)
        (make-arborescence-arc :child 30 :parent 32)
        (make-arborescence-arc :child 28 :parent 29)
        (make-arborescence-arc :child 27 :parent 29)
        (make-arborescence-arc :child 26 :parent 27)
        (make-arborescence-arc :child 25 :parent 26)
        (make-arborescence-arc :child 24 :parent 25)
        (make-arborescence-arc :child 22 :parent 24)
        (make-arborescence-arc :child 23 :parent 24)
        (make-arborescence-arc :child 19 :parent 23)
        (make-arborescence-arc :child 20 :parent 22)
        (make-arborescence-arc :child 21 :parent 22)
        (make-arborescence-arc :child 18 :parent 19)
        (make-arborescence-arc :child 17 :parent 18)
        (make-arborescence-arc :child 16 :parent 18)
        (make-arborescence-arc :child 13 :parent 17)
        (make-arborescence-arc :child 15 :parent 16)
        (make-arborescence-arc :child 14 :parent 16)
        (make-arborescence-arc :child 9 :parent 13)
        (make-arborescence-arc :child 12 :parent 13)
        (make-arborescence-arc :child 11 :parent 12)
        (make-arborescence-arc :child 10 :parent 12)
        (make-arborescence-arc :child 7 :parent 11)
        (make-arborescence-arc :child 3 :parent 10)
        (make-arborescence-arc :child 8 :parent 9)
        (make-arborescence-arc :child 12 :parent 9)
        (make-arborescence-arc :child 5 :parent 7)
        (make-arborescence-arc :child 6 :parent 7)
        (make-arborescence-arc :child 4 :parent 5)
        (make-arborescence-arc :child 2 :parent 3)
        (make-arborescence-arc :child 1 :parent 2)
        (make-arborescence-arc :child 0 :parent 2)))

(defvar *node-list*
  (sort (remove-duplicates
         (append (mapcar #'arborescence-arc-parent *arc-list*)
                 (mapcar #'arborescence-arc-child *arc-list*))
         :test #'=)
        #'<))

(defvar *gold-action-list*
  (list :SHIFT :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :LEFT-ARC :SHIFT
        :SHIFT :LEFT-ARC :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :SWAP
        :SWAP :SHIFT :SHIFT :SHIFT :SWAP :SWAP :LEFT-ARC :SHIFT :SHIFT
        :SHIFT :SWAP :LEFT-ARC :SHIFT :SHIFT :LEFT-ARC :SHIFT :LEFT-ARC
        :LEFT-ARC :RIGHT-ARC :SHIFT :LEFT-ARC :SHIFT :SWAP :SHIFT :SHIFT
        :SWAP :SHIFT :SHIFT :SWAP :LEFT-ARC :LEFT-ARC :SHIFT :SHIFT
        :LEFT-ARC :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :LEFT-ARC :SHIFT :SWAP
        :SHIFT :SHIFT :SWAP :SHIFT :SHIFT :SWAP :LEFT-ARC :LEFT-ARC :SHIFT
        :SHIFT :LEFT-ARC :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :LEFT-ARC
        :SHIFT :LEFT-ARC :SHIFT :LEFT-ARC :SHIFT :SHIFT :LEFT-ARC
        :LEFT-ARC :SHIFT :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC :LEFT-ARC
        :RIGHT-ARC))

(defmacro search-oracle-action-sequence-on-parsed-text (&key test)
  `(,test
    (equal
     (hachee.dependency-parsing.shift-reduce::search-oracle-action-sequence
      (select-only-tree-arborescence-arc-list *arc-list*))
     *gold-action-list*)))

(defmacro parse-on-parsed-text (&key test)
  `(let ((actual-arc-list
          (parse *node-list*
           :action-fn
           (let ((actions (copy-list *gold-action-list*)))
             (lambda (c a)
               (declare (ignore c a))
               (pop actions)))))
         (expected-arc-list
          (select-only-tree-arborescence-arc-list *arc-list*)))
     (,test (null (set-difference actual-arc-list expected-arc-list
                                  :test #'arborescence-arc=)))
     (,test (null (set-difference expected-arc-list actual-arc-list
                                  :test #'arborescence-arc=)))))


(defmacro search-oracle-action-sequence-on-parsed-text-removing-arc-manually
    (&key test)
  `(,test
    (equal
     (hachee.dependency-parsing.shift-reduce::search-oracle-action-sequence
      ;dagをつくるarcを削除して木に
      (remove-if (lambda (a)
                   (and (= (arborescence-arc-child a) 12)
                        (= (arborescence-arc-parent a) 9)))
                 *arc-list*))
     (list
      :SHIFT :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :LEFT-ARC :SHIFT
      :SHIFT :LEFT-ARC :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :SWAP
      :SWAP :SHIFT :SHIFT :SHIFT :SWAP :SWAP :LEFT-ARC :SHIFT :SHIFT
      :SHIFT :SWAP :LEFT-ARC :SHIFT :SHIFT :LEFT-ARC :SHIFT :LEFT-ARC
      :LEFT-ARC :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :SWAP :SHIFT :SHIFT
      :SWAP :SHIFT :SHIFT :SWAP :LEFT-ARC :LEFT-ARC :SHIFT :SHIFT
      :LEFT-ARC :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :LEFT-ARC :SHIFT
      :SWAP :SHIFT :SHIFT :SWAP :SHIFT :SHIFT :SWAP :LEFT-ARC :LEFT-ARC
      :SHIFT :SHIFT :LEFT-ARC :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :LEFT-ARC
      :SHIFT :LEFT-ARC :SHIFT :LEFT-ARC :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC
      :SHIFT :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC :LEFT-ARC :RIGHT-ARC))))

(defmacro parse-on-parsed-text-removing-arc-manually (&key test)
  `(let ((gold-actions
          (list
           :SHIFT :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :LEFT-ARC :SHIFT
           :SHIFT :LEFT-ARC :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :SWAP
           :SWAP :SHIFT :SHIFT :SHIFT :SWAP :SWAP :LEFT-ARC :SHIFT :SHIFT
           :SHIFT :SWAP :LEFT-ARC :SHIFT :SHIFT :LEFT-ARC :SHIFT :LEFT-ARC
           :LEFT-ARC :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :SWAP :SHIFT :SHIFT
           :SWAP :SHIFT :SHIFT :SWAP :LEFT-ARC :LEFT-ARC :SHIFT :SHIFT
           :LEFT-ARC :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :LEFT-ARC :SHIFT
           :SWAP :SHIFT :SHIFT :SWAP :SHIFT :SHIFT :SWAP :LEFT-ARC :LEFT-ARC
           :SHIFT :SHIFT :LEFT-ARC :SHIFT :LEFT-ARC :LEFT-ARC :SHIFT :LEFT-ARC
           :SHIFT :LEFT-ARC :SHIFT :LEFT-ARC :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC
           :SHIFT :SHIFT :SHIFT :LEFT-ARC :LEFT-ARC :LEFT-ARC :RIGHT-ARC)))
     (let ((actual-arcs (parse *node-list*
                         :action-fn
                         (lambda (c a)
                           (declare (ignore c a))
                           (pop gold-actions))))
           (expected-arcs (remove-if
                           (lambda (a)
                             (and (= (arborescence-arc-child a) 12)
                                  (= (arborescence-arc-parent a) 9)))
                           *arc-list*)))
       (,test (null (set-difference actual-arcs expected-arcs
                                    :test #'arborescence-arc=)))
       (,test (null (set-difference expected-arcs actual-arcs
                                    :test #'arborescence-arc=))))))
