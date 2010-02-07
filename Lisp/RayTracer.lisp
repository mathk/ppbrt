(declaim (inline ray-direction-x ray-direction-y ray-direction-z ray-origin-x ray-origin-y ray-origin-z))

(defstruct (point (:conc-name p))
  x y z)

(defstruct (vect (:conc-name v))
  x y z)

(defstruct ray origin direction)

(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (v)
  (let ((d (mag (vx v) (vy v) (vz v))))
    (make-vect :x (/ (vx v) d) :y (/ (vy v) d) :z (/ (vz v) d))))

(defun ray-direction-x (ray)
  (vx (ray-direction ray)))

(defun ray-direction-y (ray)
  (vy (ray-direction ray)))

(defun ray-direction-z (ray)
  (vz (ray-direction ray)))

(defun ray-origin-x (ray)
  (px (ray-origin ray)))

(defun ray-origin-y (ray)
  (py (ray-origin ray)))

(defun ray-origin-z (ray)
  (pz (ray-origin ray)))

(defun dot (v1 v2)
  (+ (* (vx v1) (vx v2)) (* (vy v1) (vy v2)) (* (vz v1) (vz v2))))

(defun ray-at (ray n)
  (make-point :x (+ (ray-origin-x ray) (* n (ray-direction-x ray)))
	      :y (+ (ray-origin-y ray) (* n (ray-direction-y ray)))
	      :z (+ (ray-origin-z ray) (* n (ray-direction-z ray)))))

(defun distance (p1 p2)
  (mag (- (px p1) (px p2))
       (- (py p1) (py p2))
       (- (pz p1) (pz p2))))

(defun distance-v (p1 p2)
  (make-vect :x (- (px p1) (px p2))
	     :y (- (py p1) (py p2))
	     :z (- (pz p1) (pz p2))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
	(unless (minusp disc)
	  (let ((discrt (sqrt disc))
		(aa (* a 2)))
	    (let ((first (/ (+ (- b) discrt) aa))
		  (second (/ (- (- b) discrt) aa)))
	      (values (min first second) (max first second))))))))

(defstruct surface color-red color-green color-blue)


(defparameter *world* nil)
(defparameter *light* nil)

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
	      (color-at (make-point  :x x  :y y :z 0))
	    (print r p)
	    (print g p)
	    (print b p)))))))

(defun color-at (p)
  (multiple-value-bind (r g b)
      (sendray (make-ray :origin eye :direction (unit-vector (distance-v p eye))))
    (values (round (* r 255)) (round (* g 255)) (round (* b 255)))))

(defun sendray (ray)
  (multiple-value-bind (s int) (first-hit ray)
    (if s
	(multiple-value-bind (light-s light-int) (first-hit (make-ray :origin int :direction (distance-v *light* int)))
	  (if (not light-s)
	      (let ((l (min 1 (+ 0.1 (lambert s int (ray-direction ray))))))
		(values (* l (surface-color-red s)) (* l (surface-color-green s)) (* l (surface-color-blue s))))
	      (values (* 0.1 (surface-color-red s)) (* 0.1 (surface-color-green s)) (* 0.1 (surface-color-blue s)))))
	(values 0 0 0))))

(defun first-hit (ray)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s ray)))
	(when h
	  (let ((d (distance h (ray-origin ray))))
	      (when (or (null dist) (< d dist))
		(setf surface s hit h dist d))))))
      (values surface hit)))

(defun lambert (s intersection incoming)
  (let* ((n (normal s intersection))
	(light-i (dot n (distance-v intersection *light*))))
    (if (> 0 light-i)
	(max 0  (* (dot n (unit-vector (distance-v *light* intersection)))  (dot n incoming)))
	(max 0  (* (dot n (unit-vector (distance-v intersection *light*)))  (dot n incoming))))))

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

(defun intersect (s ray)
  (funcall (typecase s (sphere #'sphere-intersect))
	   s ray))

(defun sphere-intersect (s ray)
  (let* ((c (sphere-center s)))
    (multiple-value-bind (first second) 
	(minroot (+ 
		  (sq (ray-direction-x ray)) 
		  (sq (ray-direction-y ray)) 
		  (sq (ray-direction-z ray)))
		     (* 2 (+ (* (- (ray-origin-x ray) (px c)) (ray-direction-x ray))
			     (* (- (ray-origin-y ray) (py c)) (ray-direction-y ray))
			     (* (- (ray-origin-z ray) (pz c)) (ray-direction-z ray))))
		     (+ (sq (- (ray-origin-x ray) (px c)))
			(sq (- (ray-origin-y ray) (py c)))
			(sq (- (ray-origin-z ray) (pz c)))
			(- (sq (sphere-radius s)))))
      (if (and first (> first 0.001))
	  (ray-at ray first)
	  (if (and second (> second 0.001))
	      (ray-at ray second))))))


(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
	   s pt))

(defun sphere-normal (s pt)
    (unit-vector (distance-v (sphere-center s) pt)))


(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (setf *light* (make-point :x 0 :y 500 :z -800))
  (defsphere 0 -300 -1600 150 .6 .5 .8)
  (defsphere -80 -150 -1200 100 .7 .8 .2)
  (defsphere 70 -100 -1200 200 .9 .3 .4)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
	((> z 7))
	(defsphere (* x 200) 300 (* z -400) 40 .75 .56 .87)))
  (tracer (make-pathname :name "spheres.pgm") res))
