;;; Non-Projective Dependency Parsing in Expected Linear Time
(defpackage :hachee.dependency-parsing.shift-reduce
  (:use :cl :hachee.util.stream)
  (:import-from :alexandria
                :when-let
                :if-let))
(in-package :hachee.dependency-parsing.shift-reduce)
(cl-annot:enable-annot-syntax)

;;; vertex
(defun vertex= (u v)
  (= u v))

(defun vertex< (u v)
  (< u v))

;;; arc
(defstruct arborescence-arc parent child)
(export 'make-arborescence-arc)
(export 'arborescence-arc-parent)
(export 'arborescence-arc-child)

@export
(defun arborescence-arc= (a1 a2)
  (and (vertex= (arborescence-arc-parent a1)
                (arborescence-arc-parent a2))
       (vertex= (arborescence-arc-child a1)
                (arborescence-arc-child a2))))

;;; transition system configuration
(defstruct configuration stack buffer arcs root)
(export 'configuration-root)
(export 'configuration-arcs)
(export 'configuration-stack)
(export 'configuration-buffer)


(defun assert-nodes (nodes)
  (assert (every (lambda (node) (<= 0 node)) nodes)))

(let ((root -1))
  (defun make-initial-configuration (vertices)
    (make-configuration :stack (list root) :buffer vertices :root root)))

(defun configuration-terminated-p (configuration)
  (with-accessors ((root configuration-root)
                   (stack configuration-stack)
                   (buffer configuration-buffer)) configuration
    (and (car stack) (null (cdr stack))
         (vertex= (car stack) root)
         (null buffer))))

(defun configuration-left-arc! (configuration)
  (with-accessors ((root configuration-root)
                   (arcs configuration-arcs)
                   (stack configuration-stack)) configuration
    (when-let ((j (first stack))
               (i (second stack)))
      (when (not (vertex= i root))
        (push (make-arborescence-arc :parent j :child i) arcs)
        (setf stack (cons j (cddr stack)))
        (return-from configuration-left-arc!))))
  (error "left-arc!"))

(defun configuration-right-arc! (configuration)
  (with-accessors ((root configuration-root)
                   (arcs configuration-arcs)
                   (stack configuration-stack)) configuration
    (when-let ((j (first stack))
               (i (second stack)))
      (push (make-arborescence-arc :parent i :child j) arcs)
      (setf stack (cons i (cddr stack)))
      (return-from configuration-right-arc!)))
  (error "right-arc!"))


(defun configuration-shift! (configuration)
  (with-accessors ((stack configuration-stack)
                   (buffer configuration-buffer)) configuration
    (when-let ((i (pop buffer)))
      (push i stack)
      (return-from configuration-shift!)))
  (error "shift!"))

(defun configuration-swap! (configuration)
  (with-accessors ((root configuration-root)
                   (stack configuration-stack)
                   (buffer configuration-buffer)) configuration
    (when-let ((j (first stack))
               (i (second stack)))
      (when (and (not (vertex= i root))
                 (not (vertex= j root))
                 (vertex< i j))
        (setf stack (cons j (cddr stack)))
        (setf buffer (cons i buffer))
        (return-from configuration-swap!))))
  (error "swap!"))

(defun copy-configuration-deeply (config)
  (let ((copied (copy-configuration config)))
    (setf (configuration-stack copied)
          (copy-list (configuration-stack config)))
    (setf (configuration-buffer copied)
          (copy-list (configuration-buffer config)))
    (setf (configuration-arcs copied)
          (copy-list (configuration-arcs config)))
    copied))

(defun configuration-transitioned (configuration action)
  (let ((copied (copy-configuration-deeply configuration)))
    (funcall (ecase action
               (:left-arc #'configuration-left-arc!)
               (:right-arc #'configuration-right-arc!)
               (:swap #'configuration-swap!)
               (:shift #'configuration-shift!))
             copied)
    copied))

(defun configuration-possible-actions (configuration)
  (let ((actions (list :right-arc)))
    (with-accessors ((root configuration-root)
                     (stack configuration-stack)
                     (buffer configuration-buffer)) configuration
      (when buffer
        (push :shift actions))
      (when-let ((j (first stack)) (i (second stack)))
        (when (not (vertex= i root))
          (push :left-arc actions))
        (when (and (not (vertex= i root))
                   (not (vertex= j root))
                   (vertex< i j))
          (push :swap actions))))
    actions))


;;; parse
(defun select-action (configuration action-fn)
  (let ((actions (configuration-possible-actions configuration)))
    (funcall action-fn configuration actions)))

(defun make-configuration-stream (configuration action-fn)
   (if (configuration-terminated-p configuration)
       nil
       (cons-stream
        configuration
        (let* ((action
                (select-action configuration action-fn))
               (next-configuration
                (configuration-transitioned configuration action)))
          (make-configuration-stream next-configuration action-fn)))))

(defun make-action-stream (configuration action-fn)
  (if (configuration-terminated-p configuration)
      nil
      (let ((action (select-action configuration action-fn)))
        (cons-stream
         action
         (let ((next-configuration
                (configuration-transitioned configuration action)))
           (make-action-stream next-configuration action-fn))))))

@export
(defun parse (vertices &key action-fn)
  (assert-nodes vertices)
  (let ((initial-configuration
         (make-initial-configuration vertices)))
    (remove (configuration-root initial-configuration)
      (configuration-arcs
       (alexandria:lastcar
        (stream-as-list
         (make-configuration-stream initial-configuration action-fn))))
      :key #'arborescence-arc-parent)))


;;; oracle
(defstruct oracle gold-arc-list ordered-vertices)

(defun is-arc-of (&key parent child)
  (lambda (arc)
    (and (if parent
             (vertex= (arborescence-arc-parent arc) parent)
             t)
         (if child
             (vertex= (arborescence-arc-child arc) child)
             t))))

(defun find-root (arborescence-arc-list)
  (let ((parents (mapcar #'arborescence-arc-parent arborescence-arc-list))
        (children (mapcar #'arborescence-arc-child arborescence-arc-list)))
    (labels ((child-vertex-p (vertex)
               (member vertex children :test #'vertex=)))
      (find-if-not #'child-vertex-p parents))))

(defun is-outgoing-arc-from (v)
  (is-arc-of :parent v))

(defun outgoing-arcs (arborescence-arc-list vertex)
  (let ((outgoing-arcs (remove-if-not (is-outgoing-arc-from vertex)
                                      arborescence-arc-list)))
    (sort outgoing-arcs #'vertex< :key #'arborescence-arc-child)))

(defun call/vertex-in-order (arborescence-arc-list root callback)
  (labels ((is-left-of (v1)
             (lambda (v2) (vertex< v2 v1)))
           (is-right-of (v1)
             (lambda (v2) (vertex< v1 v2)))
           (visit (vertex parent)
             (let ((children
                    (mapcar #'arborescence-arc-child
                            (outgoing-arcs arborescence-arc-list vertex)))
                   (visit-child
                    (lambda (child)
                      (visit child vertex))))
               (mapc visit-child
                     (remove-if-not (is-left-of vertex) children))
               (funcall callback vertex parent)
               (mapc visit-child
                     (remove-if-not (is-right-of vertex) children)))))
    (visit root nil)))

(defun list-ordered-vertices (arborescence-arc-list)
  (let ((reverse-ordered-vertices nil))
    (call/vertex-in-order arborescence-arc-list
                          (find-root arborescence-arc-list)
      (lambda (vertex parent)
        (declare (ignore parent))
        (push vertex reverse-ordered-vertices)))
    (reverse reverse-ordered-vertices)))

(defun oracle-for (initial-configuration gold-arborescence-arc-list)
  (make-oracle :gold-arc-list
               (cons (make-arborescence-arc
                      :parent (configuration-root initial-configuration)
                      :child (find-root gold-arborescence-arc-list))
                     gold-arborescence-arc-list)
               :ordered-vertices
               (list-ordered-vertices gold-arborescence-arc-list)))


(defun oracle-left-arc-p (oracle configuration)
  (with-accessors ((stack configuration-stack)) configuration
    (when-let ((j (first stack)) (i (second stack)))
      (and (find-if (is-arc-of :parent j :child i)
                    (oracle-gold-arc-list oracle))
           (null (set-difference
                  (remove-if-not (is-outgoing-arc-from i)
                                 (oracle-gold-arc-list oracle))
                  (configuration-arcs configuration)
                  :test #'arborescence-arc=))))))

(defun oracle-right-arc-p (oracle configuration)
  (with-accessors ((stack configuration-stack)) configuration
    (when-let ((j (first stack)) (i (second stack)))
      (and (find-if (is-arc-of :parent i :child j)
                    (oracle-gold-arc-list oracle))
           (null (set-difference
                  (remove-if-not (is-outgoing-arc-from j)
                                 (oracle-gold-arc-list oracle))
                  (configuration-arcs configuration)
                  :test #'arborescence-arc=))))))

(defun oracle-vertex< (oracle j i)
  (let ((ordered-vertices (oracle-ordered-vertices oracle)))
    (if-let ((pos-j (position j ordered-vertices :test #'vertex=))
             (pos-i (position i ordered-vertices :test #'vertex=)))
      (< pos-j pos-i)
      (error "invalid vertex: ~A" (if (null pos-j) j i)))))

(defun oracle-swap-p (oracle configuration)
  (with-accessors ((root configuration-root)
                   (stack configuration-stack)) configuration
    (when-let ((j (first stack)) (i (second stack)))
      (when (and (not (vertex= i root)) (not (vertex= j root)))
        (oracle-vertex< oracle j i)))))

(defun oracle-select-action (oracle configuration)
  (cond ((oracle-left-arc-p oracle configuration) :left-arc)
        ((oracle-right-arc-p oracle configuration) :right-arc)
        ((oracle-swap-p oracle configuration) :swap)
        (t :shift)))


;;; train
(defun list-vertices (arborescence-arc-list)
  (sort (remove-duplicates (alexandria:mappend
                             #'(lambda (a)
                                 (list (arborescence-arc-parent a)
                                       (arborescence-arc-child a)))
                             arborescence-arc-list)
                           :test #'vertex=)
        #'vertex<))

; visible for test
(defun search-oracle-action-sequence (gold-arborescence-arc-list)
  (let ((initial-configuration
         (make-initial-configuration
          (list-vertices gold-arborescence-arc-list))))
    (let ((oracle (oracle-for initial-configuration
                              gold-arborescence-arc-list)))
      (stream-as-list
       (make-action-stream initial-configuration
                           (lambda (configuration actions)
                             (declare (ignore actions))
                             (oracle-select-action oracle configuration)))))))

@export
(defun call-with-training-instance (gold-arborescence-arc-list callback)
  (let ((gold-action-sequence
         (search-oracle-action-sequence gold-arborescence-arc-list))
        (initial-configuration
         (make-initial-configuration
          (list-vertices gold-arborescence-arc-list))))
    (loop
      for gold-action
            in gold-action-sequence
      for prev-config
            = initial-configuration then next-config
      for next-config
            = (configuration-transitioned prev-config gold-action)
      do  (let* ((possible-actions
                  (configuration-possible-actions prev-config))
                 (gold-index
                  (position gold-action possible-actions)))
            (assert gold-index)
            (funcall callback prev-config possible-actions gold-index)))))

;;; For dag args
@export
(defun select-only-tree-arborescence-arc-list (arborescence-arc-list)
  (let ((visited-vertices nil)
        (removed-arborescence-arc-list nil))
    (call/vertex-in-order arborescence-arc-list
                          (find-root arborescence-arc-list)
      (lambda (vertex parent)
        (if (and (member vertex visited-vertices :test #'vertex=)
                 (not (member parent visited-vertices :test #'vertex=)))
            (push (make-arborescence-arc :parent parent :child vertex)
                  removed-arborescence-arc-list)
            (push vertex visited-vertices))))
    (set-difference arborescence-arc-list
                    removed-arborescence-arc-list
                    :test #'arborescence-arc=)))
