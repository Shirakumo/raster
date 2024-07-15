(in-package #:org.shirakumo.raster)

(declaim (ftype (function (coordinate coordinate) single-float) no-clip))
(defun no-clip (nx ny)
  (declare (ignore nx ny))
  0f0)

(defvar *clip-stack* (list #'no-clip))

(defun clip (sdf)
  (intersect (first *clip-stack*) sdf))

(defun push-clip (sdf)
  (let ((prev (first *clip-stack*)))
    (push (intersect prev sdf) *clip-stack*)
    (first *clip-stack*)))

(defun pop-clip ()
  (unless *clip-stack*
    (error "Clip stack is empty."))
  (pop *clip-stack*)
  (first *clip-stack*))

(defmacro with-clip (sdf &body body)
  `(let ((*clip-stack* (list* (intersect ,sdf (first *clip-stack*)))))
     ,@body))

(defmacro with-rect-clip ((x y w h) &body body)
  `(with-clip (rectangle ,x ,y ,w ,h)
     ,@body))

(defmacro with-no-clipping (&body body)
  `(let ((*clip-stack* (list #'no-clip)))
     ,@body))

(defun draw-line (ax ay bx by buffer bw bh &key sampler (line-width 1) feather)
  (let* ((sampler (ensure-sampler sampler))
         (lw (float line-width 0f0))
         (w (- bx ax)) (h (- by ay))
         (sdf (line ax ay bx by :thickness lw)))
    (declare (dynamic-extent sampler sdf))
    (composite-sdf sampler (clip sdf) (+ w lw lw) (+ h lw lw) buffer bw bh
                   :sx (- ax lw) :sy (- ay lw) :tx (- ax lw) :ty (- ay lw) :feather feather)))

(defun draw-curve (ax ay wx wy vx vy bx by buffer bw bh &key sampler (line-width 1) feather)
  (let* ((sampler (ensure-sampler sampler))
         (lw (float line-width 0f0))
         (w (- bx ax)) (h (- by ay))
         (sdf (bezier ax ay wx wy vx vy bx by :thickness lw)))
    (declare (dynamic-extent sampler sdf))
    (composite-sdf sampler (clip sdf) (+ w lw lw) (+ h lw lw) buffer bw bh
                   :sx (- ax lw) :sy (- ay lw) :tx (- ax lw) :ty (- ay lw) :feather feather)))

(defun draw-lines (points buffer bw bh &key sampler (line-width 1) feather line-style join-style cap-style)
  ;; TODO: implement draw-lines
  )

(defun draw-curves (points buffer bw bh &key sampler (line-width 1) feather line-style join-style cap-style)
  ;; TODO: implement draw-curves
  )

(defun draw-rectangle (x y w h buffer bw bh &key sampler line-width corner-radii feather)
  (let ((sampler (ensure-sampler sampler))
        (lw (if line-width (float line-width 0f0) 0f0))
        (sdf (rectangle x y w h :corner-radii corner-radii)))
    (declare (dynamic-extent sampler sdf))
    (when line-width
      (setf sdf (outline sdf lw)))
    (composite-sdf sampler (clip sdf) (+ w lw lw) (+ h lw lw) buffer bw bh
                   :sx (- x lw) :sy (- y lw) :tx (- x lw) :ty (- y lw) :feather feather)))

(defun draw-ellipse (x y w h buffer bw bh &key sampler line-width feather (start 0) (end (* 2 PI)) (inner-radius 0))
  (let ((sampler (ensure-sampler sampler))
        (lw (if line-width (float line-width 0f0) 0f0))
        (sdf (ellipse x y w h :start start :end end :inner-radius inner-radius)))
    (declare (dynamic-extent sampler sdf))
    (when line-width
      (setf sdf (outline sdf lw)))
    (composite-sdf sampler (clip sdf) (+ w lw lw) (+ h lw lw) buffer bw bh
                   :sx (- x lw) :sy (- y lw) :tx (- x lw) :ty (- y lw) :feather feather)))

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
          for x = (elt points (+ 0 i))
          for y = (elt points (+ 1 i))
          do (setf x- (min x- x)) (setf x+ (max x+ x))
             (setf y- (min y- y)) (setf y+ (max y+ y)))
    (composite-sdf sampler (clip sdf) (+ (- x+ x-) lw lw) (+ (- y+ y-) lw lw) buffer bw bh
                   :sx (- x- lw) :sy (- y- lw)
                   :tx (- x- lw) :ty (- y- lw) :feather feather)))

(defun draw-image (image x y buffer bw bh &key transform)
  ;; FIXME: Pretty sure this does not interact correctly between TRANSFORM and CLIP and so on besides being horribly inefficient.
  (let ((sampler (sampler (image-buffer image) (image-width image) (image-height image) :transform transform)))
    (composite-sdf sampler (first *clip-stack*) (image-width image) (image-height image) buffer bw bh
                   :tx x :ty y)))
