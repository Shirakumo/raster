(in-package #:org.shirakumo.raster)

(defun draw-line (ax ay bx by buffer bw bh &key sampler (line-width 1) feather)
  (let* ((sampler (ensure-sampler sampler))
         (lw (float line-width 0f0))
         (w (- bx ax)) (h (- by ay))
         (sdf (line lw lw (+ w lw) (+ h lw) :thickness lw)))
    (declare (dynamic-extent sampler sdf))
    (composite-sdf sampler sdf (+ w lw lw) (+ h lw lw) buffer bw bh
                   :tx (- ax lw) :ty (- ay lw) :feather feather)))

(defun draw-curve (ax ay bx by buffer bw bh &key sampler (line-width 1) feather)
  (let* ((sampler (ensure-sampler sampler))
         (lw (float line-width 0f0))
         (w (- bx ax)) (h (- by ay))
         (sdf (bezier lw lw (+ w lw) (+ h lw) :thickness lw)))
    (declare (dynamic-extent sampler sdf))
    (composite-sdf sampler sdf (+ w lw lw) (+ h lw lw) buffer bw bh
                   :tx (- ax lw) :ty (- ay lw) :feather feather)))

(defun draw-lines (points buffer bw bh &key sampler (line-width 1) line-style join-style cap-style feather)
  ;; TODO: implement draw-lines
  )

(defun draw-curves (points buffer bw bh &key sampler (line-width 1) line-style join-style cap-style feather)
  ;; TODO: implement draw-curves
  )

(defun draw-rectangle (x y w h buffer bw bh &key sampler line-width corner-radii feather)
  (let ((sampler (ensure-sampler sampler))
        (lw (if line-width (float line-width 0f0) 0f0))
        (sdf (rectangle (/ w 2) (/ h 2) (/ w 2) (/ h 2) :corner-radii corner-radii)))
    (declare (dynamic-extent sampler sdf))
    (when line-width
      (setf sdf (outline sdf lw)))
    (composite-sdf sampler sdf (+ w lw lw) (+ h lw lw) buffer bw bh
                   :tx (- x lw) :ty (- y lw) :feather feather)))

(defun draw-ellipse (x y w h buffer bw bh &key sampler line-width feather (start 0) (end (* 2 PI)) (inner-radius 0))
  (let ((sampler (ensure-sampler sampler))
        (lw (if line-width (float line-width 0f0) 0f0))
        (sdf (ellipse (/ w 2) (/ h 2) (/ w 2) (/ h 2) :start start :end end :inner-radius inner-radius)))
    (declare (dynamic-extent sampler sdf))
    (when line-width
      (setf sdf (outline sdf lw)))
    (composite-sdf sampler sdf (+ w lw lw) (+ h lw lw) buffer bw bh
                   :tx (- x lw) :ty (- y lw) :feather feather)))

(defun draw-polygon (points buffer bw bh &key sampler line-width feather)
  (let* ((sampler (ensure-sampler sampler))
         (lw (if line-width (float line-width 0f0) 0f0))
         (sdf (polygon points))
         (x- (aref points 0)) (x+ x-)
         (y- (aref points 1)) (y+ y-))
    (declare (dynamic-extent sampler sdf))
    (when line-width
      (setf sdf (outline sdf lw)))
    (loop for i from 2 below (length points) by 2
          for x = (aref points (+ 0 i))
          for y = (aref points (+ 1 i))
          do (setf x- (min x- x)) (setf x+ (max x+ x))
             (setf y- (min y- y)) (setf y+ (max y+ y)))
    (composite-sdf sampler sdf (+ (- x+ x-) lw lw) (+ (- y+ y-) lw lw) buffer bw bh
                   :sx (- x- lw) :sy (- y- lw)
                   :tx (- x- lw) :ty (- y- lw) :feather feather)))

(defun draw-image (image x y buffer bw bh &key)
  (composite (image-buffer image) (image-width image) (image-height image)
             buffer bw bh :tx x :ty y))

(defun draw-text (string font buffer &key size sampler valign halign direction wrap markup feather)
  ;; TODO: implement draw-text
  )
