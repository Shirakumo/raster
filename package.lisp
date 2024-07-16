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
   #:lerp-color
   #:convert-buffer)
  ;; composite.lisp
  (:export
   #:alpha-blend
   #:blit-buffer
   #:composite-buffer
   #:composite-mask
   #:composite-sdf)
  ;; raster.lisp
  (:export
   #:clip
   #:push-clip
   #:pop-clip
   #:with-clip
   #:with-rect-clip
   #:with-no-clipping
   #:draw-line
   #:draw-curve
   #:draw-lines
   #:draw-curves
   #:draw-rectangle
   #:draw-ellipse
   #:draw-polygon
   #:draw-image
   #:with-drawing)
  ;; sampler.lisp
  (:export
   #:sampler
   #:transform
   #:make-transform
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
   #:curve
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
