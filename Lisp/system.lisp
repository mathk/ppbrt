(defpackage ray-tracer-system (:use :common-lisp :asdf))

(in-package :ray-tracer-system)

(defsystem "ray-tracer"
  :version "0.0.1"
  :author "Mathieu Suen <mathk.***@gmail.com>"
  :licence "GPLv3"
  :components ((:file "geometry")
	       (:file "ray-tracer" :depends-on ("geometry"))))