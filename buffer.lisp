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

(declaim (inline make-image))
(defun make-image (w h &optional buffer)
  (check-type w index)
  (check-type h index)
  (let ((buffer (etypecase buffer
                  (buffer
                   buffer)
                  (vector
                   (make-array (* 4 w h) :element-type '(unsigned-byte 8) :initial-contents buffer))
                  (null
                   (make-array (* 4 w h) :element-type '(unsigned-byte 8) :initial-element 0)))))
    (%make-image w h buffer)))

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
(defun encode-color (b g r &optional (a 255))
  (+ (ash b  0)
     (ash g  8)
     (ash r 16)
     (ash a 24)))

(declaim (inline decode-color))
(declaim (ftype (function (color) (values channel channel channel channel)) decode-color))
(defun decode-color (c)
  (values (ldb (byte 8  0) c)
          (ldb (byte 8  8) c)
          (ldb (byte 8 16) c)
          (ldb (byte 8 24) c)))

(declaim (inline color-ref))
(declaim (ftype (function (buffer index) color) color-ref))
(defun color-ref (buffer i)
  (nibbles:ub32ref/le buffer i))

(declaim (inline (setf color-ref)))
(declaim (ftype (function (color buffer index) color) (setf color-ref)))
(defun (setf color-ref) (value buffer i)
  (setf (nibbles:ub32ref/le buffer i) value))

(declaim (inline color-ref*))
(declaim (ftype (function (buffer index index index index &key (:border color) (:wrapping (member :repeat :clamp :border))) color) color-ref*))
(defun color-ref* (buffer x y w h &key (border :clamp))
  (declare (type index x y w h))
  (ecase border
    (:repeat
     (setf x (mod x w)
           y (mod y h)))
    (:clamp
     (setf x (max 0 (min x (1- w)))
           y (max 0 (min y (1- h)))))
    (T
     (when (or (< x 0) (< y 0)
               (<= w x) (<= h y))
       (return-from color-ref* border))))
  (color-ref buffer (+ x (* y w))))

(declaim (inline (setf color-ref*)))
(declaim (ftype (function (color buffer index index index index &key (:border color) (:wrapping (member :repeat :clamp :border))) color) (setf color-ref*)))
(defun (setf color-ref*) (color buffer x y w h &key (border :clamp))
  (declare (type index x y w h))
  (ecase border
    (:repeat
     (setf x (mod x w)
           y (mod y h)))
    ((:border :clamp)
     (when (or (< x 0) (< y 0)
               (<= w x) (<= h y))
       (return-from color-ref* color))))
  (setf (color-ref buffer (+ x (* y w))) color))

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

(defun bilinear (bl br tl tr x y)
  (declare (type color bl br tl tr))
  (declare (type (single-float 0.0 1.0) x y))
  (lerp-color
   (lerp-color bl br x)
   (lerp-color tl tr x)
   y))

(defun sample-color (buffer x y w h &key (border :clamp))
  (declare (type index w h))
  (declare (type coordinate x y))
  (ecase border
    (:repeat
     (setf x (mod x w)
           y (mod y h)))
    (:clamp
     (setf x (max 0 (min x (1- w)))
           y (max 0 (min y (1- h)))))
    (T
     (when (or (< x 0) (< y 0)
               (<= w x) (<= h y))
       (return-from sample-color border))))
  (multiple-value-bind (x- xt) (floor x)
    (multiple-value-bind (y- yt) (floor y)
      (let ((yl (* y- w)))
        (if (and (= 0 xt) (= 0 yt))
            (color-ref buffer (+ x- yl))
            (ecase border
              (:repeat
               (let* ((x+ (mod (+ x- 1) w))
                      (y+ (mod (+ y- 1) h))
                      (bl (color-ref buffer (+ x- yl)))
                      (br (color-ref buffer (+ x+ yl)))
                      (tl (color-ref buffer (+ x- (* y+ h))))
                      (tr (color-ref buffer (+ x+ (* y+ h)))))
                 (bilinear bl br tl tr xt yt)))
              (:clamp
               (let* ((bl (color-ref buffer (+ x- yl)))
                      (br (if (< (1+ x-) w) (color-ref buffer (+ x- 1 yl)) bl))
                      (tl (if (< (1+ y-) h) (color-ref buffer (+ x- yl w)) bl))
                      (tr (if (and (< (1+ x-) w) (< (1+ y-) h)) (color-ref buffer (+ x- 1 yl w)) tl)))
                 (bilinear bl br tl tr xt yt)))
              (T
               (let* ((bl (color-ref buffer (+ x- yl)))
                      (br (if (< (1+ x-) w) (color-ref buffer (+ x- 1 yl)) border))
                      (tl (if (< (1+ y-) h) (color-ref buffer (+ x- yl w)) border))
                      (tr (if (and (< (1+ x-) w) (< (1+ y-) h)) (color-ref buffer (+ x- 1 yl w)) border)))
                 (bilinear bl br tl tr xt yt)))))))))
