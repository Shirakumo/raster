(in-package #:org.shirakumo.raster)

(defun draw-lines (points buffer bw bh &key sampler line-width line-style join-style cap-style feather-radius)
  (let ((sampler (ensure-sampler sampler)))
    (declare (dynamic-extent sampler))
    ;; TODO: implement draw-lines
    ))

(defun draw-bezier (points buffer bw bh &key sampler line-width line-style join-style cap-style feather-radius)
  (let ((sampler (ensure-sampler sampler)))
    (declare (dynamic-extent sampler))
    ;; TODO: implement draw-bezier
    ))

(defun draw-rectangle (x y w h buffer bw bh &key sampler line-width border-radius feather-radius)
  (let ((sampler (ensure-sampler sampler)))
    (declare (dynamic-extent sampler))
    ;; TODO: implement draw-rectagle
    ))

(defun draw-ellipse (x y w h buffer bw bh &key sampler line-width feather-radius)
  (let ((sampler (ensure-sampler sampler)))
    (declare (dynamic-extent sampler))
    ;; TODO: implement draw-ellipse
    ))

(defun draw-polygon (points buffer bw bh &key sampler)
  (let ((sampler (ensure-sampler sampler)))
    (declare (dynamic-extent sampler))
    ;; TODO: implement draw-polygon
    ))

(defun draw-image (image x y buffer bw bh &key)
  (composite (image-buffer image) (image-width image) (image-height image)
             buffer bw bh :tx x :ty y))

(defun draw-text (string font buffer &key size sampler valign halign direction wrap markup feather-radius)
  (let ((sampler (ensure-sampler sampler)))
    (declare (dynamic-extent sampler))
    ;; TODO: implement draw-text
    ))
