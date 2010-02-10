;(require :asdf)
;(load #P"./geometry.lisp")
;(use-package 'geometry)

(defpackage :tracer
  (:use :common-lisp :geometry)
  (:export :ray-test))

(in-package tracer)

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

(defparameter eye (make-point :x 0 :y 0 :z 200))

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
	(let ((ray-to-light (make-ray :origin int :direction (distance-v *light* int))))
	  (multiple-value-bind (light-s light-int) (first-hit ray-to-light)
	    (if (or (not light-s) (behind-light light-int ray-to-light))
		(let ((l (min 1 (+ 0.1 (lambert s int (ray-direction ray))))))
		  (values (* l (surface-color-red s)) (* l (surface-color-green s)) (* l (surface-color-blue s))))
		(values (* 0.1 (surface-color-red s)) (* 0.1 (surface-color-green s)) (* 0.1 (surface-color-blue s))))))
	(values 0 0 0))))

(defun behind-light (intersection ray)
  (> (distance intersection (ray-origin ray)) (distance *light* (ray-origin ray))))

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
	(max 0  (dot n (unit-vector (distance-v *light* intersection))))
	(max 0  (dot n (unit-vector (distance-v intersection *light*)))))))

(defstruct (sphere (:include surface))
  radius center)

(defstruct (triangle (:include surface))
  p1 p2 p3)

(defun deftriangle (px1 py1 pz1 px2 py2 pz2 px3 py3 pz3 cr cg cb)
  (let ((tr (make-triangle
	    :p1 (make-point :x px1 :y py1 :z pz1)
	    :p2 (make-point :x px2 :y py2 :z pz2)
	    :p3 (make-point :x px3 :y py3 :z pz3)
	    :color-red cr
	    :color-green cg
	    :color-blue cb)))
    (push tr *world*)
    tr))

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
  (funcall (typecase s 
	     (sphere #'sphere-intersect)
	     (triangle #'triangle-intersect))
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

(defun triangle-intersect (s ray)
  (let* ((e1 (distance-v (triangle-p2 s) (triangle-p1 s)))
	 (e2 (distance-v (triangle-p3 s) (triangle-p1 s)))
	 (s1 (cross (ray-direction ray) e2))
	 (divisor (dot s1 e1)))
    (if (/= divisor 0)
	(let* ((d (distance-v (ray-origin ray) (triangle-p1 s)))
	       (b1 (/ (dot d s1) divisor)))
	  (if (and (> b1 0) (< b1 1))
	      (let* ((s2 (cross d e1))
		     (b2 (/ (dot (ray-direction ray) s2) divisor)))
		(if (and (> b2 0) (< (+ b1 b2) 1))
		    (let ((thit (/ (dot e2 s2) divisor)))
		      (if (> thit 0.001)
			  (ray-at ray thit))))))))))





(defun normal (s pt)
  (funcall (typecase s 
	     (sphere #'sphere-normal)
	     (triangle #'triangle-normal))
	   s pt))

(defun sphere-normal (s pt)
  (unit-vector (distance-v (sphere-center s) pt)))

(defun triangle-normal (s pt)
  (unit-vector (cross (distance-v (triangle-p1 s) (triangle-p2 s))
		      (distance-v (triangle-p3 s) (triangle-p2 s)))))


(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (setf *light* (make-point :x -80 :y -300 :z -600))
;;  (defsphere 0 -300 -1600 150 .6 .5 .8)
;  (defsphere -80 200 -900 50 .6 .5 .8)
  ;;(defsphere 0 500 -800 10 1 1 1)
  (defsphere -80 -150 -1200 100 .7 .8 .2)
  (defsphere 70 -100 -1200 110 .9 .3 .4)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
	((> z 7))
	(defsphere (* x 200) 300 (* z -400) 40 .79 .66 .45)))
  (deftriangle -800   400 -900  -800 400 -4900 1000 400 -4900 1 1 1)
  (deftriangle -800   400 -900  1000 400 -4900 1000 400 -900 1 1 1)

  (deftriangle -800 -1000 -4900   1000   400 -4900 -800   400 -4900 1 1 1)
  (deftriangle  1000 -1000 -4900  1000   400 -4900 -800 -1000 -4900 1 1 1)

  (deftriangle -800 -1000 -4900 -800   400 -4900  -800 400 -900  1 1 1)
  (deftriangle -800 -1000 -900  -800  -1000 -4900  -800 400 -900 1 1 1)
  (tracer (make-pathname :name "spheres.pgm") res))
