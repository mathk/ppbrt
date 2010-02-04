(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
	(unless (minusp disc)
	  (let ((discrt (sqrt disc))
		(aa (* a 2)))
	    (min (/ (+ (- b) discrt) aa)
		 (/ (- (- b) discrt) aa)))))))

(defstruct surface color-red color-green color-blue)


(defparameter *world* nil)

(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P3 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
	  ((< (- 50 y) inc))
	(do ((x -50 (+ x inc)))
	    ((< (- 50 x) inc))
	  (multiple-value-bind (r g b)
	      (color-at x y)
	    (print r p)
	    (print g p)
	    (print b p)))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
		   (- y (y eye))
		   (- 0 (z eye)))
    (multiple-value-bind (r g b)
	(sendray eye xr yr zr)
      (values (round (* r 255)) (round (* g 255)) (round (* b 255))))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
	(let ((l (lambert s int xr yr zr)))
	  (values (* l (surface-color-red s)) (* l (surface-color-green s)) (* l (surface-color-blue s))))
	(values 0 0 0))))



(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
	(when h
	  (let ((d (distance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

(defstruct (sphere (:include surface))
  radius center)


(defun defsphere (x y z r cr cg cb)
  (let ((s (make-sphere
	    :radius r
	    :center (make-point :x x :y y :z z)
	    :color-red cr
	    :color-green cg
	    :color-blue cb)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
	   s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
	 (n (minroot (+ (sq xr) (sq yr) (sq zr))
		     (* 2 (+ (* (- (x pt) (x c)) xr)
			     (* (- (y pt) (y c)) yr)
			     (* (- (z pt) (z c)) zr)))
		     (+ (sq (- (x pt) (x c)))
			(sq (- (y pt) (y c)))
			(sq (- (z pt) (z c)))
			(- (sq (sphere-radius s)))))))
    (if n
	(make-point :x (+ (x pt) (* n xr))
		    :y (+ (y pt) (* n yr))
		    :z (+ (z pt) (* n zr))))))


(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))


(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1600 150 .6 .5 .8)
  (defsphere -80 -150 -1200 100 .7 .8 .2)
  (defsphere 70 -100 -1200 200 .9 .3 .4)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
	((> z 7))
	(defsphere (* x 200) 300 (* z -400) 40 .75 .56 .87)))
  (tracer (make-pathname :name "spheres.pgm") res))