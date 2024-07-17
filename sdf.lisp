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

(defun curve (ax ay wx wy vx vy bx by &key (thickness 1f0))
  (let* ((ax (coordinate ax)) (ay (coordinate ay))
         (bx (coordinate bx)) (by (coordinate by))
         (vx (coordinate vx)) (vy (coordinate vy))
         (wx (coordinate wx)) (wy (coordinate wy))
         (thick (coordinate thickness)))
    (with-sdf ()
      )))

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
