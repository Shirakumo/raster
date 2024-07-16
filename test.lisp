(defpackage #:org.shirakumo.raster.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:raster #:org.shirakumo.raster))
  (:export #:raster))

(in-package #:org.shirakumo.raster.test)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname*)))

(defun file (file)
  (merge-pathnames file *here*))

(defun load-image (file)
  (let* ((image (pngload:load-file file :flatten T))
         (buffer (raster:convert-buffer (pngload:data image)
                                        (pngload:width image)
                                        (pngload:height image)
                                        (ecase (pngload:color-type image)
                                          (:greyscale :r)
                                          (:greyscale-alpha :ra)
                                          (:truecolour :rgb)
                                          (:truecolour-alpha :rgba)))))
    (raster:make-image (pngload:width image) (pngload:height image) buffer)))

(define-test raster
  :defun T)

(define-test buffer
  :parent raster
  (finish (raster:make-buffer 100 100))
  (fail (raster:make-buffer (expt 2 32) 1))
  (finish (raster:make-image 1 1))
  (fail (raster:make-image 0 1))
  (is = 4 (length (raster:make-buffer 1 1)))
  (is = 0 (aref (raster:make-buffer 1 1) 0))
  (is = 255 (aref (raster:clear (raster:make-buffer 1 1) :white) 0)))

(define-test color-manipulation
  :parent raster
  (is = #xFF000000 (raster:encode-color 0 0 0))
  (is = #xFFFF0000 (raster:encode-color 255 0 0))
  (is = #xFF00FF00 (raster:encode-color 0 255 0))
  (is = #xFF0000FF (raster:encode-color 0 0 255))
  (is-values (raster:decode-color #xDEADBEEF)
    (= #xAD) (= #xBE) (= #xEF) (= #xDE))
  (is = #xFF000000 (raster:lerp-color #xFF000000 #xFFFFFFFF 0.0))
  (is = #xFFFFFFFF (raster:lerp-color #xFF000000 #xFFFFFFFF 1.0))
  (is = #xFF808080 (raster:lerp-color #xFF000000 #xFFFFFFFF 0.5))
  (is = #xFF (raster:alpha-blend #xFF #x00 #xFF))
  (is = #x00 (raster:alpha-blend #xFF #x00 #x00))
  (is = #x80 (raster:alpha-blend #x80 #x00 #xFF))
  (is = #x40 (raster:alpha-blend #x80 #x00 #x80)))

(define-test color-ref
  :parent raster
  (let ((buf (raster:make-buffer 2 2)))
    (is = 0 (raster:color-ref buf 0))
    (is = 0 (raster:color-ref buf 1))
    (is = 255 (setf (raster:color-ref buf 0) 255))
    (is = 255 (raster:color-ref* buf 0 0 2 2))
    (is = 255 (raster:color-ref* buf -1 0 2 2))
    (is = 255 (raster:color-ref* buf 0 -1 2 2))
    (is = 255 (raster:color-ref* buf -1 -1 2 2))
    (is = 0 (raster:color-ref* buf 1 0 2 2))
    (is = 0 (raster:color-ref* buf 2 0 2 2))
    (is = 0 (raster:color-ref* buf 2 2 2 2))
    (is = 255 (raster:color-ref* buf 2 0 2 2 :border :repeat))
    (is = 0 (raster:color-ref* buf 3 0 2 2 :border :repeat))
    (is = 128 (raster:color-ref* buf 2 0 2 2 :border 128))))
