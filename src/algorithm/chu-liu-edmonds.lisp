(defpackage :hachee.algorithm.chu-liu-edmonds
  (:use :cl)
  (:export :get-score :solve)
  (:import-from :alexandria :when-let))
(in-package :hachee.algorithm.chu-liu-edmonds)

(defun get-score (score-hash i j)
  (gethash (list i j) score-hash))

(defun (setf get-score) (score score-hash i j)
  (setf (gethash (list i j) score-hash ) score))

(defun print-scores (score-hash &optional current-vertices)
  (labels ((pr (front rear score)
             (format t "s(~A -> ~A) = ~A~%" front rear score)))
    (maphash (lambda (key score)
               (destructuring-bind (front rear) key
                 (if current-vertices
                     (when (and (/= rear 0)
                                (subsetp key current-vertices :test #'=))
                       (pr front rear score))
                     (pr front rear score))))
             score-hash)))

(defun solve (num-vertices score-hash current-vertices)
  (let ((front (make-array num-vertices :initial-element -1)))
    ;;; best pathを探す
    (dolist (i current-vertices)
      (when (/= i 0) ;id = 0はroot
        (let ((max-score -1))
          (dolist (j current-vertices)
            (when (/= j i)
              (when-let ((score (get-score score-hash j i)))
                (when (< max-score score)
                  (setf max-score score)
                  (setf (aref front i) j))))))))
    ;;; cycleを探す (親をたどっていって自分自身に出会うかどうか)
    (let ((cycle nil)
          (checked-vertices nil))
      (loop for i in current-vertices
            while (null cycle) do
        (when (not (member i checked-vertices :test #'=))
          (let ((maybe-cycle-vertices nil))
            (loop for l = i then (aref front l) do
              (cond ((member l maybe-cycle-vertices :test #'=)
                     ;cycle発見
                     (setq cycle (make-hash-table :test #'equal))
                     (loop for l1 = (aref front l) then (aref front l1) do
                       (setf (gethash (aref front l1) cycle) l1)
                       (when (= l1 l) (return)))
                     (return))
                    ((or (= (aref front l) -1)
                         (member l checked-vertices :test #'=))
                     ;親をたどっていくとrootに来たorすでに見た
                     (return))
                    (t
                     (push l checked-vertices)
                     (push l maybe-cycle-vertices)))))))
      (if (null cycle)
          ;;;; cycleがなかった -> 終わり
          (progn
            ;; (print current-vertices)
            (loop for i in current-vertices
              when (/= (aref front i) -1)
                collect (list (aref front i) i)))
          ;;;; cycleがあった -> 再帰呼び出し
          (let* ((cycle-vertices (alexandria:hash-table-keys cycle))
                 (cycle-weight (loop for i in cycle-vertices
                                 sum (get-score score-hash
                                                (aref front i) i)))
                 ;; cycleを表現する頂点 (適当に設定)
                 (cycle-representing-vertex (car cycle-vertices))
                 ;; cycle外に出て行っているvertexの実態
                 (real-departure-vertices (make-hash-table :test #'equal))
                 ;; cycle内に入ってくるvertexの実態
                 (real-arrival-vertices (make-hash-table :test #'equal)))
            ;; (terpri)
            ;; (format t "vertices: ~A~%" current-vertices)
            ;; (format t "weight:   ~A~%" cycle-weight)
            ;; (format t "cycle: ~A~%" cycle-vertices)
            ;; (format t "rep  : ~A~%" cycle-representing-vertex)
            ;; 新しいedgeのコストを計算してscore-hashを更新
            (dolist (i current-vertices)
              (when (not (member i cycle-vertices))
                (let ((max-departure-score -1)
                      (max-arrival-score   -1)
                      ;; iに向かってエッジを投げるcycle内のvertexの実態
                      (real-departure-vertex -1)
                      ;; iからくるエッジを受け止めるcycle内のvertexの実態
                      (real-arrival-vertex   -1))
                  ;;;; cycle-representing-vertex -> i と
                  ;;;; i -> cycle-representing-vertex のスコアを計算
                  (dolist (j cycle-vertices)
                    ;; cycle-representing-vertex -> i
                    (when-let ((departure-score (get-score score-hash j i)))
                      (when (< max-departure-score departure-score)
                        (setq max-departure-score departure-score)
                        (setq real-departure-vertex j)))
                    ;; i -> cycle-representing-vertex 
                    (when-let ((in-score (get-score score-hash i j)))
                      (let ((arrival-score (+ cycle-weight
                                              in-score
                                              (- (get-score
                                                  score-hash
                                                  (aref front j) j)))))
                        (when (< max-arrival-score arrival-score)
                          (setq max-arrival-score arrival-score)
                          (setq real-arrival-vertex j)))))
                  ;; score-hash更新
                  (when (< -1 max-departure-score)
                    (setf (get-score score-hash cycle-representing-vertex i)
                          max-departure-score)
                    (setf (gethash i real-departure-vertices)
                          real-departure-vertex))
                  (when (< -1 max-arrival-score)
                    (setf (get-score score-hash i cycle-representing-vertex)
                          max-arrival-score)
                    (setf (gethash i real-arrival-vertices)
                          real-arrival-vertex)))))
            ;; (maphash (lambda (outside-vertex departure-vertex)
            ;;            (format t "(~A ~A) => (~A ~A)~%"
            ;;                    cycle-representing-vertex outside-vertex
            ;;                    departure-vertex outside-vertex))
            ;;          real-departure-vertices)
            ;; (maphash (lambda (outside-vertex arrival-vertex)
            ;;            (format t "(~A ~A) => (~A ~A)~%"
            ;;                    outside-vertex cycle-representing-vertex
            ;;                    outside-vertex arrival-vertex))
            ;;          real-arrival-vertices)
            (dolist (cycle-vertex cycle-vertices)
              ;; cycleのvertexを、サイクルを表現するものを除いて除去
              (when (/= cycle-vertex cycle-representing-vertex)
                (setq current-vertices
                      (remove cycle-vertex current-vertices))))
            ;; (print-scores score-hash current-vertices)
            ;; 縮約したグラフで再帰呼び出し
            (let ((edges (solve num-vertices score-hash current-vertices))
                  (final-edges nil))
              (if (null edges)
                  (let ((min-score 0)
                        (min-score-edge-start -1))
                    (maphash (lambda (start end)
                               (let ((score (get-score score-hash
                                                       start end)))
                                 (when (or (= min-score-edge-start -1)
                                           (< score min-score))
                                   (setq min-score score)
                                   (setq min-score-edge-start start))))
                             cycle)
                    (maphash (lambda (start end)
                               (when (/= start min-score-edge-start)
                                 (push (list start end) final-edges)))
                             cycle))
                  (progn
                    ;; cycle-representing-nodeを展開
                    (loop for (start end) in edges do
                      (when (= start cycle-representing-vertex)
                        (setq start (gethash end real-departure-vertices)))
                      (when (= end cycle-representing-vertex)
                        (setq end (gethash start real-arrival-vertices)))
                      (push (list start end) final-edges))
                    ;; cycle内のエッジを追加
                    ;; ただし、rearの頂点が外からもエッジを受けていれば、
                    ;; そのエッジは追加しない
                    (dolist (i cycle-vertices)
                      (when (not (member i final-edges
                                         :test #'= :key #'second))
                        (push (list (aref front i) i) final-edges)))))
              final-edges))))))
