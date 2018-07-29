(defpackage :hachee.dependency-parsing.mst.feature
  (:use :cl :hachee.dependency-parsing)
  (:export :extract))
(in-package :hachee.dependency-parsing.mst.feature)

(defun map-length (len)
  (cond ((<  len  0)  "-1")
        ((=  len  0)   "0")
        ((<= len  1)   "1")
        ((<= len  2)   "2")
        ((<= len  4)   "4")
        ((<= len  8)   "8")
        ((<= len 16)  "16")
        ((<= len 32)  "32")
        (t           "33+")))

(defun mkstr (&rest args)
  (format nil "~{~A~}" args))

(let ((EOS "EOS")
      (BOS "BOS"))
  (defun extract (sentence source target)
    (let ((rows (sentence-rows sentence))
          (length (sentence-length sentence)))
      (let ((w (row-form (elt rows source)))
            (wr1 (if (< (+ source 1) length)
                     (row-form (elt rows (+ source 1)))
                     EOS))
            (wr2 (if (< (+ source 2)  length)
                     (row-form (elt rows (+ source 2)))
                     EOS))
            (wr3 (if (< (+ source 3)  length)
                     (row-form (elt rows (+ source 3)))
                     EOS))
            (wl1 (if (<= 1 (- source 1))
                     (row-form (elt rows (- source 1)))
                     BOS))
            (wl2 (if (<= 1 (- source 2))
                     (row-form (elt rows (- source 2)))
                     EOS))
            (wl3 (if (<= 1 (- source 3))
                     (row-form (elt rows (- source 3)))
                     EOS))
            (h (row-form (elt rows target)))
            (hr1 (if (< (+ target 1) length)
                     (row-form (elt rows (+ target 1)))
                     EOS))
            (hr2 (if (< (+ target 2)  length)
                     (row-form (elt rows (+ target 2)))
                     EOS))
            (hr3 (if (< (+ target 3)  length)
                     (row-form (elt rows (+ target 3)))
                     EOS))
            (hl1 (if (<= 1 (- target 1))
                     (row-form (elt rows (- target 1)))
                     BOS))
            (hl2 (if (<= 1 (- target 2))
                     (row-form (elt rows (- target 2)))
                     EOS))
            (hl3 (if (<= 1 (- target 3))
                     (row-form (elt rows (- target 3)))
                     EOS))
            (wp (row-postag (elt rows source)))
            (wrp1 (if (< (+ source 1) length)
                      (row-postag (elt rows (+ source 1)))
                      EOS))
            (wlp1 (if (<= 1 (- source 1))
                      (row-postag (elt rows (- source 1)))
                      BOS))
            (hp (row-postag (elt rows target)))
            (hrp1 (if (< (+ target 1) length)
                      (row-postag (elt rows (+ target 1)))
                      EOS))
            (hlp1 (if (<= 1 (- target 1))
                      (row-postag (elt rows (- target 1)))
                      BOS))
            (wc (row-cluster (elt rows source)))
            (hc (row-cluster (elt rows target)))
            (dist (map-length (- target source))))
        (list (mkstr "dist_" dist " ")
              (mkstr "w_" w " ")
              (mkstr "wr1_" wr1 " ")
              (mkstr "wr2_" wr2 " ")
              (mkstr "wr3_" wr3 " ")
              (mkstr "wl1_" wl1 " ")
              (mkstr "wl2_" wl2 " ")
              (mkstr "wl3_" wl3 " ")
              (mkstr "h_" h " ")
              (mkstr "hr1_" hr1 " ")
              (mkstr "hr2_" hr2 " ")
              (mkstr "hr3_" hr3 " ")
              (mkstr "hl1_" hl1 " ")
              (mkstr "hl2_" hl2 " ")
              (mkstr "hl3_" hl3 " ")
              (mkstr "w_dist_" w "_" dist " ")
              (mkstr "w_h_" w "_" h " ")
              (mkstr "w_hr1_" w "_" hr1 " ")
              (mkstr "w_hr2_" w "_" hr2 " ")
              (mkstr "w_hl1_" w "_" hl1 " ")
              (mkstr "w_hl2_" w "_" hl2 " ")
              (mkstr "wr1_h_" wr1 "_" h " ")
              (mkstr "wr1_hr1_" wr1 "_" hr1 " ")
              (mkstr "wr1_hr2_" wr1 "_" hr2 " ")
              (mkstr "wr1_hl1_" wr1 "_" hl1 " ")
              (mkstr "wr1_hl2_" wr1 "_" hl2 " ")
              (mkstr "wl1_h_" wl1 "_" h " ")
              (mkstr "wl1_hr1_" wl1 "_" hr1 " ")
              (mkstr "wl1_hr2_" wl1 "_" hr2 " ")
              (mkstr "wl1_hl1_" wl1 "_" hl1 " ")
              (mkstr "wl1_hl2_" wl1 "_" hl2 " ")
              (mkstr "wc_hc_" wc "_" hc " ")
              (mkstr "w_hc_" w "_" hc " ") 
              (mkstr "wc_h_" wc "_" h " ")
              (mkstr "wp_hp_" wp "_" hp " ")
              (mkstr "w_hp_" w "_" hp " ")
              (mkstr "wp_h_" wp "_" h " ")
              (mkstr "wp_wc_hp_hc_" wp "_" wc "_" hp "_" hc " ")
              (mkstr "wrp1_hp_" wrp1 "_" hp " ")
              (mkstr "wlp1_hp_" wlp1 "_" hp " ")
              (mkstr "wp_hrp1_" wp "_" hrp1 " ")
              (mkstr "wp_hlp1_" wp "_" hlp1 " ")
              (mkstr "w_hrp1_" w "_" hrp1 " ")
              (mkstr "w_hlp1_" w "_" hlp1 " "))))))
