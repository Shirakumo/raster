(in-package #:org.shirakumo.raster)

(deftype sdf ()
  '(function (single-float single-float) single-float))

(declaim (inline vlen))
(defun vlen (x y)
  (sqrt (+ (expt x 2) (expt y 2))))

(declaim (inline dot))
(defun dot (ax ay bx by)
  (+ (* ax bx) (* ay by)))

(declaim (inline clamp))
(defun clamp (x min max)
  (min max (max min x)))

(defmacro with-sdf (args &body body)
  (let ((args (or args '(nx ny))))
    `(lambda ,args
       (declare (type coordinate ,@args))
       (declare (optimize speed (safety 0)))
       (float (locally ,@body)
              0f0))))

(defun rectangle (x y w h &key corner-radii)
  (let* ((w (* 0.5f0 (coordinate w))) (h (* 0.5f0 (coordinate h)))
         (x (+ (coordinate x) w)) (y (+ (coordinate y) h)))
    (etypecase corner-radii
      (null
       ;; Basic aligned rec case
       (with-sdf ()
         (let ((dx (- (abs (- nx x)) w))
               (dy (- (abs (- ny y)) h)))
           (+ (min 0f0 (max dx dy))
              (sqrt (+ (expt (max dx 0f0) 2)
                       (expt (max dy 0f0) 2)))))))
      (T
       ;; Rounded rect case
       (let ((rtl (coordinate (elt corner-radii 0)))
             (rtr (coordinate (elt corner-radii 1)))
             (rbr (coordinate (elt corner-radii 2)))
             (rbl (coordinate (elt corner-radii 3))))
         (with-sdf ()
           (let* ((px (- nx x))
                  (py (- ny y))
                  (r (if (< 0 px)
                         (if (< 0 py) rtr rbr)
                         (if (< 0 py) rtl rbl)))
                  (qx (+ r (- (abs px) w)))
                  (qy (+ r (- (abs py) h))))
             (+ (min 0f0 (max qx qy))
                (sqrt (+ (expt (max qx 0f0) 2)
                         (expt (max qy 0f0) 2)))
                (- r)))))))))

(declaim (inline punch-hole))
(defun punch-hole (value ir)
  (- (abs (+ value ir)) ir))

(defun ellipse (x y w h &key (start 0) (end (* 2 PI)) (inner-radius 0.0))
  (let* ((w (* 0.5f0 (coordinate w))) (h (* 0.5f0 (coordinate h)))
         (x (+ (coordinate x) w)) (y (+ (coordinate y) h))
         (ir (* 0.5 (- w (coordinate inner-radius))))
         (start (coordinate start)) (end (coordinate end)))
    (declare (type coordinate x y w h ir start end))
    (cond ((and (<= (* 2 PI) (- end start)) (= w h))
           ;; Ring case
           (with-sdf ()
             (punch-hole (- (vlen (- nx x) (- ny y)) w) ir)))
          ((= w h)
           ;; Partial ring case
           (let ((start (- start (float (/ PI 2) 0f0)))
                 (aperture (* (abs (- end start)) 0.5)))
             (when (< end start)
               (decf start (+ (* 2 aperture) (float PI 0f0)))
               (decf aperture (float PI 0f0)))
             (let ((cx (sin aperture))
                   (cy (cos aperture)))
               (when (<= PI aperture)
                 (setf cx 0f0 cy -1f0))
               (let ((cstart (cos (+ aperture start)))
                     (sstart (sin (+ aperture start))))
                 (with-sdf ()
                   (let* ((nx (- nx x)) (ny (- ny y))
                          (nx (abs (+ (* cstart nx) (- (* sstart ny)))))
                          (ny (+ (* sstart nx) (* cstart ny)))
                          (l (- (vlen nx ny) 0.5))
                          (n (clamp (dot nx ny cx cy) 0.0 0.5))
                          (m (vlen (- nx (* cx n)) (- ny (* cy n))))
                          (sdf (max l (* m (signum (- (* cy nx) (* cx ny)))))))
                     (punch-hole sdf ir)))))))
          ((<= (* 2 PI) (- end start))
           ;; Ellipse case
           (with-sdf ()
             (let ((px (abs (- nx x))) (py (abs (- ny y))))
               (when (< py px) (rotatef px py) (rotatef w h))
               (let* ((l (- (* w w) (* h h)))
                      (m (/ (* w px) l)) (m2 (* m m))
                      (n (/ (* h py) l)) (n2 (* n n))
                      (c (/ (+ m2 n2 -1) 3))
                      (c3 (* c c c))
                      (q (+ c3 (* m2 n2 2)))
                      (d (+ c3 (* m2 n2)))
                      (g (+ m (* m n2)))
                      (co (if (< d 0)
                              (let* ((h (/ (the single-float (acos (/ q c3))) 3))
                                     (s (cos h))
                                     (tt (* (sin h) (sqrt 3f0)))
                                     (rx (the single-float (sqrt (- m2 (* c (+ s tt +2))))))
                                     (ry (the single-float (sqrt (- m2 (* c (- s tt -2)))))))
                                (* 0.5 (+ ry (* rx (signum l)) (/ (abs g) (* rx ry)) (- m))))
                              (let* ((h (* 2 m n (sqrt d)))
                                     (s (* (signum (+ q h)) (expt (abs (+ q h)) 1/3)))
                                     (u (* (signum (- q h)) (expt (abs (- q h)) 1/3)))
                                     (rx (- 0 s u (* 4 c) (* m2 -2)))
                                     (ry (* (- s u) (sqrt 3f0)))
                                     (rm (the single-float (sqrt (+ (* rx rx) (* ry ry))))))
                                (* 0.5 (+ (/ ry (the single-float (sqrt (- rm rx)))) (/ (* 2 g) rm) (- m))))))
                      (rx (* w co))
                      (ry (* h (the single-float (sqrt (- 1 (* co co)))))))
                 (punch-hole (* (vlen (- rx px) (- ry py)) (signum (- py ry))) ir)))))
          (T
           ;; TODO: General ellipse case
           (error "IMPLEMENT ME")))))

(defun line (ax ay bx by &key (thickness 1f0))
  (let* ((ax (coordinate ax)) (ay (coordinate ay))
         (bax (- (coordinate bx) ax)) (bay (- (coordinate by) ay))
         (len (+ (* bax bax) (* bay bay)))
         (thick (coordinate thickness)))
    (with-sdf ()
      (let* ((nax (- nx ax)) (nay (- ny ay))
             (h (min 1f0 (max 0f0 (/ (+ (* nax bax) (* nay bay)) len))))
             (lx (- nax (* h bax)))
             (ly (- nay (* h bay))))
        (- (sqrt (+ (* lx lx) (* ly ly))) thick)))))

(defmacro define-cubic-helper (name () &body body)
  `(progn (declaim (inline ,name))
          (defun ,name (tt p0x p0y p1x p1y p2x p2y p3x p3y)
            (declare (type single-float tt p0x p0y p1x p1y p2x p2y p3x p3y))
            (let ((m1 (- 1 tt)))
              (values (let ((p0 p0x) (p1 p1x) (p2 p2x) (p3 p3x))
                        ,@body)
                      (let ((p0 p0y) (p1 p1y) (p2 p2y) (p3 p3y))
                        ,@body))))))

(define-cubic-helper b3 ()
  (+ (* m1 m1 m1 p0) (* 3 m1 tt (+ (* m1 p1) (* tt p2))) (* tt tt tt p3)))

(define-cubic-helper b3-prime ()
  (* 3 (+ (* m1 m1 (- p1 p0)) (* 2 m1 tt (- p2 p1)) (* tt tt (- p3 p2)))))

(define-cubic-helper b3-second ()
  (+ (* 6 m1 (+ p2 (* -2 p1) p0)) (* 6 tt (+ p3 (* -2 p2) p1))))

(defun d3-prime (xx xy tt p0x p0y p1x p1y p2x p2y p3x p3y)
  (multiple-value-bind (b3x b3y) (b3 tt p0x p0y p1x p1y p2x p2y p3x p3y)
    (multiple-value-bind (p3x p3y) (b3-prime tt p0x p0y p1x p1y p2x p2y p3x p3y)
      (dot (- xx b3x) (- xy b3y) p3x p3y))))

(defun quadratic-zeros (a b c)
  (if (= 0 a)
      (let ((x (- (/ c b))))
        (values x x))
      (let ((d (- (* b b) (* 4 a c))))
        (if (< d 0)
            (values 1.0e4 1.0e4)
            (let ((x (/ (- (sqrt d) b) (* 2 a))))
              (values x (- x)))))))

(defun cubic-zeros (a b c d)
  (if (= 0 a)
      (let ((x (- (/ c b))))
        (values x x x))
      (let* ((aix (/ b a))
             (aiy (/ c a))
             (aiz (/ d a))
             (tau (/ aix 3))
             (p (- aiy (* tau aix)))
             (q (- aiz (* tau (+ (* tau tau) p))))
             (dis (+ (* q q 1/4) (* p p p 1/27))))
        (if (< 0 dis)
            (let* ((kix (- (sqrt dis) (* .5 q)))
                   (kiy (- (- (sqrt dis)) (* .5 q)))
                   (uix (* (signum kix) (expt (abs kix) 1/3)))
                   (uiy (* (signum kiy) (expt (abs kiy) 1/3)))
                   (x (+ uix (- uiy tau))))
              (values x x x))
            (let* ((fac (sqrt (* p -4/3)))
                   (arg (/ (acos (* -.5 q (sqrt (/ -27 p p p)))) 3))
                   (x (* fac (cos (+ arg (* c pi 1/3))))))
              (values (- tau x) (- x tau) (- tau x)))))))

(defun quartic-zeros (a b c d e)
  (if (= 0 a)
      (let ((x (- (/ c b))))
        (values x x x x))
      (let* ((_b (/ b a))
             (_c (/ c a))
             (_d (/ d a))
             (_e (/ e a))
             (p (/ (- (* 8 _c) (* 3 _b _b)) 8))
             (q (/ (- (* _b _b _b) (* 4 _b _c) (* -8 _d)) 8))
             (r (/ (- (* 256 _e) (* 3 _b _b _b _b) (* 64 _b _d) (* -16 _b _b _c)) 256)))
        (multiple-value-bind (resx resy resz) (cubic-zeros 8 (* 8 p) (- (* 2 p p) (* 8 r)) (- (* q q)))
          (let ((m (if (= 0 resx) (if (= 0 resy) resz resy) resx)))
            (dotimes (i 2)
              (let* ((a2 (+ p m))
                     (a1 (+ (* p p 1/4) (- r) (* m a2)))
                     (b2 (+ a2 m))
                     (f (+ (* q q -1/8) (* m a1)))
                     (f1 (+ a1 (* m b2))))
                (decf m (/ f f1))))
            (multiple-value-bind (ax ay) (quadratic-zeros 1 (sqrt (* 2 m)) (+ (/ p 2) m (/ q (* -2 (sqrt (* 2 m))))))
              (multiple-value-bind (bx by) (quadratic-zeros 1 (- (sqrt (* 2 m))) (+ (/ p 2) m (/ q (* +2 (sqrt (* 2 m))))))
                (values (- ax (/ b 4)) (- ay (/ b 4))
                        (- bx (/ b 4)) (- by (/ b 4))))))))))

(defun curve (ax ay wx wy vx vy bx by &key (thickness 1f0))
  (let* ((ax (coordinate ax)) (ay (coordinate ay))
         (bx (coordinate bx)) (by (coordinate by))
         (vx (coordinate vx)) (vy (coordinate vy))
         (wx (coordinate wx)) (wy (coordinate wy))
         (thick (coordinate thickness)))
    (decf bx ax) (decf vx ax) (decf wx ax)
    (decf by ay) (decf vy ay) (decf wy ay)
    (with-sdf ()
      (let* ((x (- nx ax)) (y (- ny ay))
             (ax 0f0) (ay 0f0)
             (tmin -0.5f0) (tmax 1.5f0)
             (tnew 0f0))
        (declare (type single-float tmin tmax tnew))
        (dotimes (i 20)
          (setf tnew (+ (* 0.5 tmin) (* 0.5 tmax)))
          (if (< 0 (the single-float (d3-prime x y tnew ax ay wx wy vx vy bx by)))
              (setf tmin tnew)
              (setf tmax tnew)))
        (let* ((pax (- vx wx)) (pay (- vy wy))
               (w-pax (- wx pax)) (w-pay (- wy pay))
               (a5 (- (* 3 (dot pax pay (- (* 2 bx) (* 3 pax)) (- (* 2 by) (* 3 pay))) (dot bx by bx by))))
               (a4 (+ (* 5 (dot w-pax w-pay bx by)) (* 15 (dot pax pay (- w-pax) (- w-pay)))))
               (a3 (- (* 4 (dot wx wy (- (* 9 pax) bx) (- (* 9 pay) by))) (* 6 (dot vx vy vx vy))))
               (a2 (+ (dot (- bx (* 3 pax)) (- by (* 3 pay)) x y) (* 9 (dot wx wy w-pax w-pay))))
               (a1 (- (* 2 (dot (- w-pax) (- w-pay) x y)) (* 3 (dot wx wy wx wy))))
               (_a a5)
               (_b (+ a4 (* _a tmin)))
               (_c (+ a3 (* _b tmin)))
               (_d (+ a2 (* _c tmin)))
               (_e (+ a1 (* _d tmin)))
               (tmin (clamp tmin 0f0 1f0)))
          (multiple-value-bind (tx ty tz tw) (quartic-zeros _a _b _c _d _e)
            (flet ((try (tt)
                     (multiple-value-bind (bx by) (b3 tt ax ay wx wy vx vy bx by)
                       (vlen (- x bx) (- y by)))))
              (- (min (try tmin) (try tx) (try ty) (try tz) (try tw)) thick))))))))

(defun polygon (points)
  (let* ((v (make-array (length points) :element-type 'single-float))
         (n (length v)))
    (map-into v #'coordinate points)
    (with-sdf ()
      (let ((d (+ (expt (- nx (aref v 0)) 2)
                  (expt (- ny (aref v 1)) 2)))
            (s 1f0))
        (declare (type single-float d s))
        (loop for j = (* 2 (1- n)) then i
              for i from 0 below (* 2 n) by 2
              for ex of-type single-float = (- (aref v (+ 0 j)) (aref v (+ 0 i)))
              for ey of-type single-float = (- (aref v (+ 1 j)) (aref v (+ 1 i)))
              for wx of-type single-float = (- nx (aref v (+ 0 i)))
              for wy of-type single-float = (- ny (aref v (+ 1 i)))
              for tt of-type single-float = (clamp (/ (dot wx wy ex ey) (dot ex ey ex ey)) 0f0 1f0)
              for bx of-type single-float = (- wx (* ex tt))
              for by of-type single-float = (- wy (* ey tt))
              do (setf d (min d (dot bx by bx by)))
                 (let ((a (<= ny (aref v (+ 1 i))))
                       (b (< ny (aref v (+ 1 i))))
                       (c (< (* ey wx) (* ex wy))))
                   (when (or (and a b c) (not (or a b c)))
                     (setf s (- s)))))
        (* s (sqrt (the (single-float 0f0) d)))))))

(defun subtract (a b)
  (declare (type sdf a b))
  (with-sdf ()
    (max (funcall a nx ny)
         (- (funcall b nx ny)))))

(defun combine (a b)
  (declare (type sdf a b))
  (with-sdf ()
    (min (funcall a nx ny)
         (funcall b nx ny))))

(defun intersect (a b)
  (declare (type sdf a b))
  (with-sdf ()
    (max (funcall a nx ny)
         (funcall b nx ny))))

(defun outline (sdf thickness)
  (declare (type sdf sdf))
  (let ((th (coordinate thickness)))
    (with-sdf ()
      (let ((v (funcall sdf nx ny)))
        (- (abs v) th)))))

(defun translate (sdf x y)
  (declare (type sdf sdf))
  (let ((x (coordinate x))
        (y (coordinate y)))
    (with-sdf ()
      (funcall sdf (- nx x) (- ny y)))))

(defun scale (sdf x y)
  (declare (type sdf sdf))
  (let ((x (coordinate x))
        (y (coordinate y)))
    (with-sdf ()
      (funcall sdf (* nx x) (* ny y)))))

(defun rotate (sdf angle)
  (declare (type sdf sdf))
  (let ((cos (cos (coordinate angle)))
        (sin (sin (coordinate angle))))
    (with-sdf ()
      (funcall sdf
               (+ (* nx cos) (* ny sin))
               (+ (* nx (- sin)) (* ny cos))))))

(defun skew (sdf x y)
  (declare (type sdf sdf))
  (let ((x (tan (coordinate x)))
        (y (tan (coordinate y))))
    (with-sdf ()
      (funcall sdf (+ nx (round (* ny y))) (+ ny (round (* nx x)))))))

(defun transform (sdf mat)
  (declare (type sdf sdf))
  (declare (type transform mat))
  (with-sdf ()
    (funcall sdf
             (+ (* (aref mat 0) nx) (* (aref mat 1) ny) (aref mat 2))
             (+ (* (aref mat 3) nx) (* (aref mat 4) ny) (aref mat 5) ))))

(declaim (ftype (function (sdf coordinate coordinate &optional coordinate) single-float) dsdf))
(defun dsdf (sdf x y &optional (step 1f0))
  (declare (type sdf sdf))
  (declare (type coordinate x y step))
  (let ((sdf-0 (funcall sdf x y))
        (sdf-x- (funcall sdf (- x step) y))
        (sdf-x+ (funcall sdf (+ x step) y))
        (sdf-y- (funcall sdf x (- y step)))
        (sdf-y+ (funcall sdf x (+ y step))))
    (* (+ (abs (- sdf-0 sdf-x-))
          (abs (- sdf-x+ sdf-0))
          (abs (- sdf-0 sdf-y-))
          (abs (- sdf-y+ sdf-0)))
       0.25)))
