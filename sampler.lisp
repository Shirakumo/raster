(in-package #:org.shirakumo.raster)

(deftype sampler ()
  '(function (index index) color))

(defun sampler (buffer w h &key (border 0) (wrapping :clamp) (x 0) (y 0))
  (declare (type buffer buffer))
  (declare (type index w h x y))
  (lambda (nx ny)
    (declare (type index nx ny))
    (color-ref* buffer (- nx x) (- ny y) w h :wrapping wrapping :border border)))

(defun solid-color (r g b &optional (a 255))
  (let ((c (encode-color r g b a)))
    (lambda (x y)
      (declare (ignore x y))
      c)))

(declaim (inline lerp-color))
(declaim (ftype (function (color color (single-float 0.0 1.0)) color) lerp-color))
(defun lerp-color (a b x)
  (declare (type (single-float 0.0 1.0) x))
  (declare (type color a b))
  (flet ((lerp (a b)
           (declare (type channel a b))
           (round (+ a (* (- b a) x)))))
    (multiple-value-bind (ab ag ar aa) (decode-color a)
      (multiple-value-bind (bb bg br ba) (decode-color b)
        (encode-color (lerp ab bb) (lerp ag bg) (lerp ar br) (lerp aa ba))))))

(declaim (inline evaluate-gradient))
(defun evaluate-gradient (stops i)
  (declare (type single-float i))
  ;; TODO: optimise via binary search
  (loop for last-stop = 0.0 then next-stop
        for last-color = (second (first stops)) then next-color
        for (next-stop next-color) in stops
        do (when (< i next-stop)
             (return (lerp-color last-color next-color (/ (- i last-stop) (- next-stop last-stop)))))
        finally (return next-color)))

(defun radial-gradient (stops x y)
  (declare (type index x y))
  (lambda (nx ny)
    (declare (type index nx ny))
    (let ((i (sqrt (+ (expt (- nx x) 2) (expt (- ny y) 2)))))
      (declare (type single-float i))
      (evaluate-gradient stops i))))

(defun linear-gradient (stops ax ay bx by)
  (declare (type index ax ay bx by))
  (let* ((abx (- bx ax))
         (aby (- by ay))
         (s (float (/ (+ (* abx abx) (* aby aby))) 0f0)))
    (lambda (x y)
      (declare (type index x y))
      (let* ((acx (- x ax))
             (acy (- y ay))
             (i (* (+ (* abx acx) (* aby acy)) s)))
        (declare (type single-float i))
        (evaluate-gradient stops i)))))

(defun bilinear-gradient (stops ax ay bx by)
  (declare (type index ax ay bx by))
  (let* ((abx (- bx ax))
         (aby (- by ay))
         (s (float (/ (+ (* abx abx) (* aby aby))) 0f0)))
    (lambda (x y)
      (declare (type index x y))
      (let* ((acx (- x ax))
             (acy (- y ay))
             (i (abs (* (+ (* abx acx) (* aby acy)) s))))
        (declare (type single-float i))
        (evaluate-gradient stops i)))))

(defun diamond-gradient (stops x y)
  (declare (type index x y))
  (lambda (nx ny)
    (declare (type index nx ny))
    (let ((i (float (min (- ny y) (- nx x)) 0f0)))
      (declare (type single-float i))
      (evaluate-gradient stops i))))

(defun conical-gradient (stops x y)
  (declare (type index x y))
  (lambda (nx ny)
    (declare (type index nx ny))
    (let ((i (mod (atan (- ny y) (- nx x)) (float (* 2 PI) 0f0))))
      (declare (type single-float i))
      (evaluate-gradient stops i))))

(declaim (inline ensure-sampler))
(defun ensure-sampler (sampler)
  (etypecase sampler
    (sampler
     sampler)
    (color
     (lambda (x y)
       (declare (ignore x y))
       sampler))
    (image
     (sampler (image-buffer sampler) (image-width sampler) (image-height sampler) :wrapping :repeat))
    (null
     (load-time-value
      (ensure-sampler (encode-color 0 0 0))))))
