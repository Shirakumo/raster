(in-package #:org.shirakumo.raster)

(defun draw-lines (points buffer bw bh &key sampler line-width line-style join-style cap-style feather)
  (let ((sampler (ensure-sampler sampler))
        (sdf ))
    (declare (dynamic-extent sampler))
    ;; TODO: implement draw-lines
    ))

(defun draw-bezier (points buffer bw bh &key sampler line-width line-style join-style cap-style feather)
  (let ((sampler (ensure-sampler sampler)))
    (declare (dynamic-extent sampler))
    ;; TODO: implement draw-bezier
    ))

(defun draw-rectangle (x y w h buffer bw bh &key sampler line-width corner-radii feather)
  (let ((sampler (ensure-sampler sampler))
        (sdf (rectangle (/ w 2) (/ h 2) (/ w 2) (/ h 2) :corner-radii corner-radii)))
    (declare (dynamic-extent sampler sdf))
    (when line-width
      (setf sdf (outline sdf line-width)))
    (composite-sdf sampler sdf w h buffer bw bh :tx x :ty y :feather feather)))

(defun draw-ellipse (x y w h buffer bw bh &key sampler line-width feather (start 0) (end (* 2 PI)) (inner-radius 0))
  (let ((sampler (ensure-sampler sampler))
        (sdf (ellipse (/ w 2) (/ h 2) (/ w 2) (/ h 2) :start start :end end :inner-radius inner-radius)))
    (declare (dynamic-extent sampler sdf))
    (when line-width
      (setf sdf (outline sdf line-width)))
    (composite-sdf sampler sdf w h buffer bw bh :tx x :ty y :feather feather)))

(defun draw-polygon (points buffer bw bh &key sampler line-width feather)
  (let ((sampler (ensure-sampler sampler))
        (sdf (polygon points)))
    (declare (dynamic-extent sampler sdf))
    (when line-width
      (setf sdf (outline sdf line-width)))
    (composite-sdf sampler sdf w h buffer bw bh :tx x :ty y :feather feather)))

(defun draw-image (image x y buffer bw bh &key)
  (composite (image-buffer image) (image-width image) (image-height image)
             buffer bw bh :tx x :ty y))

(defun draw-text (string font buffer &key size sampler valign halign direction wrap markup feather)
  (let ((sampler (ensure-sampler sampler)))
    (declare (dynamic-extent sampler))
    ;; TODO: implement draw-text
    ))
