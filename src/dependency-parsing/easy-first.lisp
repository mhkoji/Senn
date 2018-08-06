;;; easy-first parser
(defpackage :hachee.dependency-parsing.easy-first
  (:use :cl :hachee.util.stream)
  (:import-from :alexandria
                :when-let
                :if-let))
(in-package :hachee.dependency-parsing.easy-first)
(cl-annot:enable-annot-syntax)

@export
(defvar *debug*
  nil)

;;; nodes
(defvar +root+ -1)


;;; actions
(defstruct action type arguments)
(export 'action-type)

@export
(defun make-nop ()
  (make-action :type :nop))

@export
(defun make-attach (source target)
  (assert (/= source +root+)) ;; rootからはattachしない
  (make-action :type :attach
               :arguments (list source target)))

@export
(defun attach-source (action)
  (car (action-arguments action)))

@export
(defun attach-target (action)
  (cadr (action-arguments action)))

@export
(defun attach= (a1 a2)
  (and (eql (action-type a1) :attach)
       (eql (action-type a2) :attach)
       (= (attach-source a1) (attach-source a2))
       (= (attach-target a1) (attach-target a2))))


@export
(defun make-move (node1 node2)
  (assert (< node1 node2))
  (assert (/= node1 +root+)) ;; rootは動かさない
  (make-action :type :move :arguments (list node1 node2)))

@export
(defun move-nodes (action)
  (action-arguments action))

@export
(defun move= (a1 a2)
  (and (eql (action-type a1) :move)
       (eql (action-type a2) :move)
       (equal (move-nodes a1) (move-nodes a2))))


;;; configuration
(defstruct arc source target)
(export 'make-arc)
(export 'arc-source)
(export 'arc-target)

(defstruct configuration pendings arcs)

(defun configuration-terminated-p (configuration)
  (with-accessors ((pendings configuration-pendings)) configuration
    (and (null (cdr pendings))
         (= (car pendings) +root+))))

(defun configuration-attach (configuration attach)
  (with-accessors ((arcs configuration-arcs)
                   (pendings configuration-pendings)) configuration
    (with-accessors ((source attach-source)
                     (target attach-target)) attach
      (make-configuration
       :arcs (let ((new-arc (make-arc :source source :target target)))
               (cons new-arc arcs))
       :pendings (remove source pendings :test #'=)))))

(defun list-swap! (list item1 item2)
  (let ((index1 (position item1 list))
        (index2 (position item2 list)))
    (setf (nth index2 list) item1)
    (setf (nth index1 list) item2))
  list)

(defun configuration-move (configuration move)
  (destructuring-bind (v1 v2) (move-nodes move)
    (with-accessors ((arcs configuration-arcs)
                     (pendings configuration-pendings)) configuration
      (make-configuration
       :arcs arcs
       :pendings (list-swap! (copy-list pendings) v1 v2)))))

(defun configuration-nop (configuration nop)
  (declare (ignore nop))
  configuration)

(defun configuration-transitioned (configuration action)
  (funcall (ecase (action-type action)
             (:attach #'configuration-attach)
             (:move   #'configuration-move)
             (:nop    #'configuration-nop))
           configuration action))


(defun collect-attaches (pendings)
  (let ((actions nil))
    (dotimes (i (1- (length pendings)))
      (let ((v-left (nth i pendings)) (v-right (nth (1+ i) pendings)))
        ;; attach right
        (when (/= v-left +root+) ;; rootからはattachしない
          (push (make-attach v-left v-right) actions))
        ;; attach left
        (push (make-attach v-right v-left) actions)))
    actions))

(defun collect-moves (pendings)
  (loop for i from 1 below (1- (length pendings)) ;; rootは動かさない
        for v-left = (nth i pendings)
        for v-right = (nth (1+ i) pendings)
     when (< v-left v-right)
       collect (make-move v-left v-right)))

(defun select-action (configuration action-fn)
  (let ((pendings (configuration-pendings configuration)))
    (let ((actions (append (collect-attaches pendings)
                           (collect-moves pendings))))
      (funcall action-fn configuration actions))))

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


;;; oracle
(defstruct oracle gold-arc-list)

(defun is-arc-of (&key source target)
  (lambda (arc)
    (and (if source
             (= (arc-source arc) source)
             t)
         (if target
             (= (arc-target arc) target)
             t))))

(defun oracle-gold-attach-p (oracle c p)
  (find-if (is-arc-of :source c :target p) (oracle-gold-arc-list oracle)))

(defun oracle-attach-to-child-finished-p (oracle child attached-arc-list)
  (let ((grandchildren
         (mapcar #'arc-source
                 (remove-if-not (is-arc-of :target child)
                                (oracle-gold-arc-list oracle)))))
    (every (lambda (grandchild)
             (find-if (is-arc-of :source grandchild :target child)
                      attached-arc-list))
           grandchildren)))

(defun oracle-valid-attach-p (oracle c p attached-arc-list)
  (and (oracle-gold-attach-p oracle c p)
       ;; cをparentにするattachがすべて終わっているか確認する。
       ;; p -> cのattachをすると、
       ;; cがpendingsから消え、今後cをparentにするattachをできない。
       (oracle-attach-to-child-finished-p oracle c attached-arc-list)))

(defun oracle-allowed-attaches (oracle configuration)
  (with-accessors ((arcs configuration-arcs)
                   (pendings configuration-pendings)) configuration
    (loop for i from 0 below (1- (length pendings))
          for v1 = (nth i pendings) for v2 = (nth (1+ i) pendings)
          when (oracle-valid-attach-p oracle v1 v2 arcs)
            collect (make-attach v1 v2)
          when (oracle-valid-attach-p oracle v2 v1 arcs)
            collect (make-attach v2 v1))))

(defun oracle-valid-move-p (oracle i pendings)
  (when-let ((v-left (nth i pendings))
             (v-right (nth (1+ i) pendings)))
    (when (and (< v-left v-right) (/= v-left +root+))
      (let ((v-left-left
             (and (< 0 (1- i)) (nth (1- i) pendings)))
            (v-right-right
             (nth (+ i 2) pendings))
            (gold-arc-list
             (oracle-gold-arc-list oracle)))
        ;; swapすることでchild-parentが隣接すればmoveしてよい
        (or (when v-left-left
              (find-if (is-arc-of :source v-right :target v-left-left)
                       gold-arc-list))
            (when v-left-left
              (find-if (is-arc-of :source v-left-left :target v-right)
                       gold-arc-list))
            (when v-right-right
              (find-if (is-arc-of :source v-left :target v-right-right)
                       gold-arc-list))
            (when v-right-right
              (find-if (is-arc-of :source v-right-right :target v-left)
                       gold-arc-list)))))))

(defun oracle-no-children-p (oracle v pendings)
  (let ((gold-arc-list (oracle-gold-arc-list oracle)))
    (dolist (child (remove v pendings :test #'=))
      (when (find-if (is-arc-of :source child :target v) gold-arc-list)
        (return nil))))
  t)

(defun oracle-allowed-moves (oracle configuration)
  (with-accessors ((arcs configuration-arcs)
                   (pendings configuration-pendings)) configuration
    (loop for i from 0 to (1- (length pendings))
          for v = (nth i pendings)
          when (and (oracle-no-children-p oracle v pendings)
                    (oracle-valid-move-p oracle i pendings))
            collect (make-move v (nth (1+ i) pendings)))))


;;; train
(defstruct scorer
  (weights (make-hash-table :test #'equal))
  feature-string-extractor)
(export 'make-scorer)
(export 'scorer-weights)

(defmacro inchash (key hash &optional (delta 1))
  `(incf (gethash ,key ,hash 0) ,delta))

(defun scorer-update-weights! (scorer configuration good-action choice-action)
  (with-accessors
        ((weights scorer-weights)
         (feature-string-extractor scorer-feature-string-extractor)) scorer
    (dolist (good-feature (funcall feature-string-extractor
                                   configuration
                                   good-action))
      (inchash good-feature weights))
    (dolist (choice-feature (funcall feature-string-extractor
                                     configuration
                                     choice-action))
      (inchash choice-feature weights +root+))))

(defun scorer-action-score (scorer action configuration)
  (with-accessors
        ((weights scorer-weights)
         (feature-string-extractor scorer-feature-string-extractor)) scorer
    (reduce
     #'+
     (mapcar (lambda (str) (gethash str weights 0))
             (funcall feature-string-extractor configuration action)))))

(defun select-in (actions configuration &key scorer)
  (let ((scores
         (mapcar (lambda (action)
                   (scorer-action-score scorer action configuration))
                 actions)))
    (let ((action-score-alist
           (sort (mapcar #'cons actions scores) #'> :key #'cdr)))
      (car (first action-score-alist)))))

(defun select (configuration &key scorer)
  (with-accessors ((pendings configuration-pendings)) configuration
    (let ((actions (append (collect-attaches pendings)
                           (collect-moves pendings))))
      (select-in actions configuration :scorer scorer))))

(defun make-action-fn-updating-score! (oracle scorer)
  (lambda (configuration actions)
    (declare (ignore actions))
    (let ((allowed-moves (oracle-allowed-moves oracle configuration))
          (allowed-attaches (oracle-allowed-attaches oracle configuration)))
      (if (null (or allowed-attaches allowed-moves))
          (progn
            (when *debug*
              (print configuration))
            (error "no allowed actions!"))
          (let ((choice (select configuration :scorer scorer)))
            (if (or (find choice allowed-attaches :test #'attach=)
                    (find choice allowed-moves :test #'move=))
                choice
                (let ((good (select-in (or allowed-attaches allowed-moves)
                                       configuration
                                       :scorer scorer)))
                  (scorer-update-weights! scorer configuration good choice)
                  (make-nop))))))))

(defun find-root (arcs)
  (let ((sources (mapcar #'arc-source arcs))
        (targets (mapcar #'arc-target arcs)))
    (labels ((source-node-p (node)
               (member node sources :test #'=)))
      (find-if-not #'source-node-p targets))))

(defun transit-collecting-actions (initial-configuration action-fn)
  (loop for config = initial-configuration then next-config
        while (not (configuration-terminated-p config))
        for action = (select-action config action-fn)
        for next-config = (configuration-transitioned config action)
        collect action
        when (and *debug* (not (eql (action-type action) :nop)))
          do (progn
               (print config)
               (print action))))


(defun assert-nodes (nodes)
  (assert (every (lambda (node) (<= 0 node)) nodes)))

@export
(defun train! (nodes gold-arc-list &key scorer)
  (assert-nodes nodes)
  (let ((oracle (make-oracle :gold-arc-list
                             (cons (make-arc
                                    :source (find-root gold-arc-list)
                                    :target +root+)
                                   gold-arc-list))))
    (transit-collecting-actions
     (make-configuration :pendings (cons +root+ nodes))
     (make-action-fn-updating-score! oracle scorer))))



;;; parse
(defun make-action-fn (scorer)
  (lambda (configuration actions)
    (let ((scores (mapcar (lambda (action)
                            (scorer-action-score
                             scorer action configuration))
                          actions)))
      (let ((score-action-alist (sort (mapcar #'cons scores actions) #'>
                                      :key #'car)))
        (cdar score-action-alist)))))

@export
(defun parse (nodes &key scorer)
  (assert-nodes nodes)
  (setq nodes (sort nodes #'<))
  (configuration-arcs
   (alexandria:lastcar
    (stream-as-list (make-configuration-stream
                     (make-configuration :pendings (cons +root+ nodes))
                     (make-action-fn scorer))))))
