(in-package #:org.shirakumo.raster)

(deftype buffer ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype color ()
  '(unsigned-byte 32))

(deftype channel ()
  '(unsigned-byte 8))

(deftype index ()
  '(unsigned-byte 32))

(deftype coordinate ()
  'single-float)

(declaim (inline coordinate))
(defun coordinate (x)
  (float x 0f0))

(declaim (inline %make-image))
(defstruct (image
            (:constructor %make-image (width height buffer))
            (:copier NIL)
            (:predicate NIL))
  (buffer NIL :type buffer)
  (width index)
  (height index))

(defmethod print-object ((image image) stream)
  (print-unreadable-object (image stream :type T :identity T)
    (format stream "~dx~d" (image-width image) (image-height image))))

(declaim (inline make-image))
(defun make-image (w h &optional buffer)
  (assert (<= 1 w (1- (ash 1 30))))
  (assert (<= 1 h (1- (ash 1 30))))
  (assert (<= 1 (* w h) (1- (ash 1 30))))
  (let ((buffer (etypecase buffer
                  (buffer buffer)
                  (vector (make-buffer w h buffer))
                  (null (make-buffer w h)))))
    (%make-image w h buffer)))

(defun make-buffer (w h &optional contents)
  (assert (<= 1 w (1- (ash 1 30))))
  (assert (<= 1 h (1- (ash 1 30))))
  (assert (<= 1 (* w h) (1- (ash 1 30))))
  (if contents
      (make-array (* 4 w h) :element-type '(unsigned-byte 8) :initial-contents contents)
      (make-array (* 4 w h) :element-type '(unsigned-byte 8) :initial-element 0)))

(defun clear (buffer &optional (color :black))
  (declare (type buffer buffer))
  (let ((color (ecase color
                 (:black 0)
                 (:white 255))))
    #+cffi
    (cffi:with-pointer-to-vector-data (ptr buffer)
      (cffi:foreign-funcall "memset" :pointer ptr :int color :size (length buffer)))
    #-cffi
    (fill buffer color)
    buffer))

(declaim (inline encode-color))
(declaim (ftype (function (channel channel channel &optional channel) color) encode-color))
(defun encode-color (r g b &optional (a 255))
  (+ (ash b  0)
     (ash g  8)
     (ash r 16)
     (ash a 24)))

(declaim (inline decode-color))
(declaim (ftype (function (color) (values channel channel channel channel)) decode-color))
(defun decode-color (c)
  (values (ldb (byte 8 16) c)
          (ldb (byte 8  8) c)
          (ldb (byte 8  0) c)
          (ldb (byte 8 24) c)))

(declaim (inline color-ref))
(declaim (ftype (function (buffer index) color) color-ref))
(defun color-ref (buffer i)
  (nibbles:ub32ref/le buffer (* 4 i)))

(declaim (inline (setf color-ref)))
(declaim (ftype (function (color buffer index) color) (setf color-ref)))
(defun (setf color-ref) (value buffer i)
  (setf (nibbles:ub32ref/le buffer (* 4 i)) value))

(declaim (inline color-ref*))
(declaim (ftype (function (buffer (signed-byte 32) (signed-byte 32) index index &key (:border (or color (member :repeat :clamp)))) color) color-ref*))
(defun color-ref* (buffer x y w h &key (border :clamp))
  (declare (type index w h))
  (etypecase border
    ((eql :repeat)
     (color-ref buffer (+ (mod x w) (* (mod y h) w))))
    ((eql :clamp)
     (color-ref buffer (+ (max 0 (min x (1- w))) (* (max 0 (min y (1- h))) w))))
    (color
     (if (or (< x 0) (< y 0) (<= w x) (<= h y))
         border
         (color-ref buffer (+ x (* y w)))))))

(declaim (inline (setf color-ref*)))
(declaim (ftype (function (color buffer (signed-byte 32) (signed-byte 32) index index &key (:border (or color (member :repeat :clamp)))) color) (setf color-ref*)))
(defun (setf color-ref*) (color buffer x y w h &key (border :clamp))
  (declare (type index w h))
  (ecase border
    (:repeat
     (setf (color-ref buffer (+ (mod x w) (* (mod y h) w))) color))
    ((:border :clamp)
     (if (or (< x 0) (< y 0)
             (<= w x) (<= h y))
         color
         (setf (color-ref buffer (+ x (* y w))) color)))))

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

(defun convert-to-buffer (buffer width height layout &optional target)
  (unless target
    (setf target (make-buffer width height)))
  (macrolet ((with-pixels ((bi ti) &body body)
               `(loop for i from 0 below (* width height)
                      for ,bi from 0 by (length (symbol-name channels))
                      for ,ti from 0 by 4
                      do ,@body)))
    (ecase layout
      (:r
       (with-pixels (bi ti)
         (let ((color (aref buffer (+ 0 bi))))
           (setf (aref target (+ 0 ti)) color)
           (setf (aref target (+ 1 ti)) color)
           (setf (aref target (+ 2 ti)) color)
           (setf (aref target (+ 3 ti)) 255))))
      (:ra
       (with-pixels (bi ti)
         (let ((color (aref buffer (+ 0 bi))))
           (setf (aref target (+ 0 ti)) color)
           (setf (aref target (+ 1 ti)) color)
           (setf (aref target (+ 2 ti)) color)
           (setf (aref target (+ 3 ti)) (aref buffer (+ 1 bi))))))
      (:rgb
       (with-pixels (bi ti)
         (setf (aref target (+ 0 ti)) (aref buffer (+ 2 bi)))
         (setf (aref target (+ 1 ti)) (aref buffer (+ 1 bi)))
         (setf (aref target (+ 2 ti)) (aref buffer (+ 0 bi)))
         (setf (aref target (+ 3 ti)) 255)))
      (:bgr
       (with-pixels (bi ti)
         (setf (aref target (+ 0 ti)) (aref buffer (+ 0 bi)))
         (setf (aref target (+ 1 ti)) (aref buffer (+ 1 bi)))
         (setf (aref target (+ 2 ti)) (aref buffer (+ 2 bi)))
         (setf (aref target (+ 3 ti)) 255)))
      (:bgra
       (with-pixels (bi ti)
         (setf (aref target (+ 0 ti)) (aref buffer (+ 0 bi)))
         (setf (aref target (+ 1 ti)) (aref buffer (+ 1 bi)))
         (setf (aref target (+ 2 ti)) (aref buffer (+ 2 bi)))
         (setf (aref target (+ 3 ti)) (aref buffer (+ 3 bi)))))
      (:rgba
       (with-pixels (bi ti)
         (setf (aref target (+ 0 ti)) (aref buffer (+ 2 bi)))
         (setf (aref target (+ 1 ti)) (aref buffer (+ 1 bi)))
         (setf (aref target (+ 2 ti)) (aref buffer (+ 0 bi)))
         (setf (aref target (+ 3 ti)) (aref buffer (+ 3 bi)))))
      (:abgr
       (with-pixels (bi ti)
         (setf (aref target (+ 0 ti)) (aref buffer (+ 1 bi)))
         (setf (aref target (+ 1 ti)) (aref buffer (+ 2 bi)))
         (setf (aref target (+ 2 ti)) (aref buffer (+ 3 bi)))
         (setf (aref target (+ 3 ti)) (aref buffer (+ 0 bi)))))
      (:argb
       (with-pixels (bi ti)
         (setf (aref target (+ 0 ti)) (aref buffer (+ 3 bi)))
         (setf (aref target (+ 1 ti)) (aref buffer (+ 2 bi)))
         (setf (aref target (+ 2 ti)) (aref buffer (+ 1 bi)))
         (setf (aref target (+ 3 ti)) (aref buffer (+ 0 bi))))))
    target))

(defun luminance (r g b)
  (round (+ (* r 0.299f0)
            (* g 0.587f0)
            (* b 0.114f0))))

(defun convert-from-buffer (buffer width height layout &optional target)
  (let ((channels (length (symbol-name layout))))
    (unless target
      (setf target (make-array (* width height channels) :element-type '(unsigned-byte 8))))
    (macrolet ((with-pixels ((ti r g b a) &body body)
                 `(loop for i from 0 below (* width height)
                        for ,ti from 0 by channels
                        do (multiple-value-bind (,r ,g ,b ,a) (decode-color (color-ref buffer i))
                             (declare (ignorable ,a))
                             ,@body))))
      (ecase layout
        (:r
         (with-pixels (ti r g b a)
           (setf (aref target (+ 0 ti)) (luminance r g b))))
        (:ra
         (with-pixels (ti r g b a)
           (setf (aref target (+ 0 ti)) (luminance r g b))
           (setf (aref target (+ 1 ti)) a)))
        (:rgb
         (with-pixels (ti r g b a)
           (setf (aref target (+ 0 ti)) r)
           (setf (aref target (+ 1 ti)) g)
           (setf (aref target (+ 2 ti)) b)))
        (:bgr
         (with-pixels (ti r g b a)
           (setf (aref target (+ 0 ti)) b)
           (setf (aref target (+ 1 ti)) g)
           (setf (aref target (+ 2 ti)) r)))
        (:bgra
         (with-pixels (ti r g b a)
           (setf (aref target (+ 0 ti)) b)
           (setf (aref target (+ 1 ti)) g)
           (setf (aref target (+ 2 ti)) r)
           (setf (aref target (+ 3 ti)) a)))
        (:rgba
         (with-pixels (ti r g b a)
           (setf (aref target (+ 0 ti)) r)
           (setf (aref target (+ 1 ti)) g)
           (setf (aref target (+ 2 ti)) b)
           (setf (aref target (+ 3 ti)) a)))
        (:abgr
         (with-pixels (ti r g b a)
           (setf (aref target (+ 0 ti)) a)
           (setf (aref target (+ 1 ti)) b)
           (setf (aref target (+ 2 ti)) g)
           (setf (aref target (+ 3 ti)) r)))
        (:argb
         (with-pixels (ti r g b a)
           (setf (aref target (+ 0 ti)) a)
           (setf (aref target (+ 1 ti)) r)
           (setf (aref target (+ 2 ti)) g)
           (setf (aref target (+ 3 ti)) b))))
      target)))
