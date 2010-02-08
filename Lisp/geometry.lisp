(defpackage "GEOMETRY" 
  (:use "COMMON-LISP") 
  (:export "RAY-DIRECTION-X" 
	   "RAY-DIRECTION-Y"
	   "RAY-DIRECTION-Z"
	   "RAY-ORIGIN-X"
	   "RAY-ORIGIN-Y"
	   "RAY-ORIGIN-Z"
	   "RAY-ORIGIN"
	   "RAY-DIRECTION"
	   "UNIT-VECTOR"
	   "DISTANCE"
	   "DISTANCE-V"
	   "DOT"
	   "RAY-AT"
	   "SQ"
	   "PX" "PY" "PZ"
	   "VX" "VY" "VZ"
	   "MAKE-POINT"
	   "MAKE-VECT"
	   "MAKE-RAY"
	   "CROSS"))

(in-package geometry)

(declaim (inline ray-direction-x ray-direction-y ray-direction-z ray-origin-x ray-origin-y ray-origin-z))

(defstruct (point (:conc-name p))
  x y z)

(defstruct (vect (:conc-name v))
  x y z)

(defstruct ray origin direction)


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

(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (v)
  (let ((d (mag (vx v) (vy v) (vz v))))
    (make-vect :x (/ (vx v) d) :y (/ (vy v) d) :z (/ (vz v) d))))

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

(defun cross (v1 v2)
  (make-vect :x (- (* (vy v1) (vz v2)) (* (vz v1) (vy v2)))
	       :y (- (* (vz v1) (vx v2)) (* (vx v1) (vz v2)))
	       :z (- (* (vx v1) (vy v2)) (* (vy v1) (vx v2)))))