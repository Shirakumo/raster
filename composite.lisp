(in-package #:org.shirakumo.raster)

(declaim (inline alpha-blend))
(declaim (ftype (function (channel channel channel) channel) alpha-blend))
(defun alpha-blend (src dst alpha)
  (declare (type (unsigned-byte 8) src dst alpha))
  ;; Simple source-over compositing, using r = (src_r * a) + (dst_r * (1-a))
  ;; but with integer arithmetic.
  (truncate (+ (* src alpha) (* dst (- 255 alpha))) 255))

(defmacro do-composite ((j i ti si) (sx sy sw sh sc tx ty tw th tc w h) &body body)
  (let ((rows (gensym "ROWS"))
        (cols (gensym "COLS")))
    `(locally (declare (type index ,sx ,sy ,sw ,sh ,tx ,ty ,tw ,th ,w ,h)
                       (optimize speed (safety 1)))
       (let ((,si (* ,sc (the (unsigned-byte 32) (+ ,sx (* ,sy ,sw)))))
             (,ti (* ,tc (the (unsigned-byte 32) (+ ,tx (* ,ty ,tw)))))
             (,rows (+ ,sx (min h (- sh sy) (- th ty))))
             (,cols (+ ,sy (min w (- sw sx) (- tw tx)))))
         (declare (type (unsigned-byte 32) ,si ,ti ,rows ,cols))
         ;; TODO: SIMD
         (loop for ,j of-type (unsigned-byte 32) from ,sx below ,rows
               do (loop for ,i of-type (unsigned-byte 32) from ,sy below ,cols
                        do (progn ,@body))
                  (incf ,ti (* ,sc ,tw))
                  (incf ,si (* ,tc ,sw)))))))

(defun composite (source sw sh target tw th &key (tx 0) (ty 0) (sx 0) (sy 0) (w sw) (h sh))
  (declare (type buffer source target))
  (do-composite (j i ti si) (sx sy sw sh 4 tx ty tw th 4 w h)
    (let ((alpha (aref source (+ 3 si i))))
      (setf (aref target (+ 0 ti i)) (alpha-blend (aref source (+ 0 si i)) (aref target (+ 0 ti i)) alpha))
      (setf (aref target (+ 1 ti i)) (alpha-blend (aref source (+ 1 si i)) (aref target (+ 1 ti i)) alpha))
      (setf (aref target (+ 2 ti i)) (alpha-blend (aref source (+ 2 si i)) (aref target (+ 2 ti i)) alpha))
      (setf (aref target (+ 3 ti i)) (alpha-blend 255 (aref target (+ 3 ti i)) alpha)))))

(defun composite-mask (sampler source sw sh target tw th &key (tx 0) (ty 0) (sx 0) (sy 0) (w sw) (h sh))
  (declare (type buffer source target))
  (declare (type sampler sampler))
  (do-composite (j i ti si) (sx sy sw sh 1 tx ty tw th 4 w h)
    (let ((mask (aref source (+ si i))))
      (when (< 0 mask)
        (let* ((color (funcall sampler i j))
               (alpha (* mask (ldb (byte 8 24) color))))
          (setf (aref target (+ 0 ti i)) (alpha-blend (ldb (byte 8  0) color) (aref target (+ 0 ti i)) alpha))
          (setf (aref target (+ 1 ti i)) (alpha-blend (ldb (byte 8 16) color) (aref target (+ 1 ti i)) alpha))
          (setf (aref target (+ 2 ti i)) (alpha-blend (ldb (byte 8 24) color) (aref target (+ 2 ti i)) alpha))
          (setf (aref target (+ 3 ti i)) (alpha-blend 255 (aref target (+ 3 ti i)) alpha)))))))

(defun composite-sdf (sampler sdf sw sh target tw th &key (tx 0) (ty 0) (sx 0) (sy 0) (w sw) (h sh))
  (declare (type buffer target))
  (declare (type sampler sampler))
  (declare (type sdf sdf))
  (do-composite (j i ti si) (sx sy sw sh 1 tx ty tw th 4 w h)
    (let ((sdf (funcall sdf (coordinate i) (coordinate j))))
      ;; FIXME: we can be much smarter here to find the edge based on the SDF value and its neighbourhood
      ;; FIXME: anti-alias edges by computing extra edge alpha
      (when (<= sdf 0)
        (let* ((color (funcall sampler i j))
               (alpha (ldb (byte 8 24) color)))
          (setf (aref target (+ 0 ti i)) (alpha-blend (ldb (byte 8  0) color) (aref target (+ 0 ti i)) alpha))
          (setf (aref target (+ 1 ti i)) (alpha-blend (ldb (byte 8 16) color) (aref target (+ 1 ti i)) alpha))
          (setf (aref target (+ 2 ti i)) (alpha-blend (ldb (byte 8 24) color) (aref target (+ 2 ti i)) alpha))
          (setf (aref target (+ 3 ti i)) (alpha-blend 255 (aref target (+ 3 ti i)) alpha)))))))
