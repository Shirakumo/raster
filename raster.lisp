(in-package #:org.shirakumo.raster)

(declaim (ftype (function (coordinate coordinate) single-float) no-clip))
(defun no-clip (nx ny)
  (declare (ignore nx ny))
  most-negative-single-float)

(declaim (type sdf *clip*))
(defvar *clip* #'no-clip)

(defun clip (sdf)
  (intersect *clip* sdf))

(defmacro with-clip (sdf &body body)
  (let ((prev (gensym "PREV"))
        (next (gensym "NEXT")))
    `(let* ((,prev (the sdf *clip*))
            (,next (with-sdf () (max (funcall (the sdf ,sdf) nx ny) (funcall ,prev nx ny))))
            (*clip* ,next))
       (declare (dynamic-extent ,next))
       ,@body)))

(defmacro with-rect-clip ((x y w h) &body body)
  `(with-clip (rectangle ,x ,y ,w ,h)
     ,@body))

(defmacro with-no-clipping (&body body)
  `(let ((*clip* #'no-clip))
     ,@body))

(defmacro define-sdf-draw (name args &body body)
  (let ((reqargs (loop for arg = (pop args) until (eq '&key arg) collect arg)))
    `(defun ,name (,@reqargs buffer bw bh &key sampler feather ,@args)
       (let* ((sampler (ensure-sampler sampler))
              (lw (if line-width (float line-width 0f0) 0f0))
              ,@body)
         (declare (dynamic-extent sampler sdf))
         (let ((x (floor (- x lw)))
               (y (floor (- y lw)))
               (w (ceiling (+ w lw lw)))
               (h (ceiling (+ h lw lw))))
           (composite-sdf sampler (clip sdf) (+ y w) (+ y h) buffer bw bh
                          :sx x :sy y :tx x :ty y :w w :h h
                          :feather feather))))))

(define-sdf-draw draw-line (ax ay bx by &key (line-width 1))
  (x ax) (y ay) (w (- bx ax)) (h (- by ay))
  (sdf (line ax ay bx by :thickness lw)))

(define-sdf-draw draw-curve (ax ay wx wy vx vy bx by &key (line-width 1))
  (x ax) (y ay) (w (- bx ax)) (h (- by ay))
  (sdf (curve ax ay wx wy vx vy bx by :thickness lw)))

(define-sdf-draw draw-rectangle (x y w h &key line-width corner-radii)
  (sdf (rectangle x y w h :corner-radii corner-radii))
  (sdf (if line-width (outline sdf lw) sdf)))

(define-sdf-draw draw-ellipse (x y w h &key line-width (start 0) (end (* 2 PI)) (inner-radius 0))
  (sdf (ellipse x y w h :start start :end end :inner-radius inner-radius))
  (sdf (if line-width (outline sdf lw) sdf)))

(define-sdf-draw draw-polygon (points &key line-width)
  (sdf (polygon points))
  (sdf (if line-width (outline sdf lw) sdf))
  (x- (aref points 0)) (x+ x-)
  (y- (aref points 1)) (y+ y-)
  (_ (loop for i from 2 below (length points) by 2
           for x = (elt points (+ 0 i))
           for y = (elt points (+ 1 i))
           do (setf x- (min x- x)) (setf x+ (max x+ x))
              (setf y- (min y- y)) (setf y+ (max y+ y))))
  (x x-) (y y-) (w (- x+ x-)) (h (- y+ y-)))

(defun draw-image (image x y buffer bw bh &key (w (image-width image)) (h (image-height image)) (sizing :fit) (valign :middle) (halign :middle) transform)
  ;; FIXME: Pretty sure this does not interact correctly between TRANSFORM and CLIP and so on besides being horribly inefficient.
  (let ((sampler (sampler (image-buffer image) (image-width image) (image-height image) :transform transform))
        (sx 0) (sy 0))
    (composite-sdf sampler (first *clip-stack*) (image-width image) (image-height image) buffer bw bh
                   :sx sx :sy sy :tx x :ty y :w w :h h)))

(defun draw-lines (points buffer bw bh &key sampler (line-width 1) feather line-style join-style cap-style)
  ;; TODO: implement draw-lines
  )

(defun draw-curves (points buffer bw bh &key sampler (line-width 1) feather line-style join-style cap-style)
  ;; TODO: implement draw-curves
  )

(defmacro with-drawing (image &body body)
  (let ((i (gensym "IMAGE"))
        (b (gensym "BUFFER"))
        (w (gensym "WIDTH"))
        (h (gensym "HEIGHT"))
        (k (gensym "KEYS"))
        (funs '((draw-line ax ay bx by)
                (draw-curve ax ay wx wy vx vy bx by)
                (draw-lines points)
                (draw-curves points)
                (draw-rectangle x y w h)
                (draw-ellipse x y w h)
                (draw-polygon points)
                (draw-image image x y))))
    `(let* ((,i ,(if (listp image) `(make-image ,@image) image))
            (,b (image-buffer ,i))
            (,w (image-width ,i))
            (,h (image-height ,i)))
       (flet ,(loop for (fun . args) in funs
                    collect `(,fun (,@args &rest ,k) (apply #',fun ,@args ,b ,w ,h ,k)))
         (declare (inline ,@(loop for (fun) in funs collect fun)))
         (declare (ignorable ,@(loop for (fun) in funs collect `#',fun)))
         ,@body)
       ,i)))
