(defpackage #:org.shirakumo.raster
  (:use #:cl)
  ;; buffer.lisp
  (:export
   #:buffer
   #:color
   #:channel
   #:index
   #:coordinate
   #:image
   #:image-buffer
   #:image-width
   #:image-height
   #:make-image
   #:make-buffer
   #:clear
   #:encode-color
   #:decode-color
   #:color-ref
   #:color-ref*
   #:lerp-color)
  ;; composite.lisp
  (:export
   #:alpha-blend
   #:blit-buffer
   #:composite-buffer
   #:composite-mask
   #:composite-sdf)
  ;; raster.lisp
  (:export
   #:draw-line
   #:draw-curve
   #:draw-lines
   #:draw-curves
   #:draw-rectangle
   #:draw-ellipse
   #:draw-polygon
   #:draw-image)
  ;; sampler.lisp
  (:export
   #:sampler
   #:sample-color
   #:sampler
   #:solid-color
   #:evaluate-gradient
   #:radial-gradient
   #:linear-gradient
   #:bilinear-gradient
   #:diamond-gradient
   #:conical-gradient
   #:ensure-sampler)
  ;; sdf.lisp
  (:export
   #:sdf
   #:rectangle
   #:ellipse
   #:line
   #:bezier
   #:polygon
   #:subtract
   #:combine
   #:intersect
   #:outline
   #:translate
   #:scale
   #:rotate
   #:skew
   #:transform))
