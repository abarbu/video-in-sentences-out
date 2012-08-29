(MODULE
 IDEA7
 (WITH
  QOBISCHEME
  XLIB
  TOOLLIB-MATLAB
  TOOLLIB-MISC
  TOOLLIB-C-BINDINGS
  TOOLLIB-IMAGE-PROCESSING
  IDEALIB-PREGEXP
  IDEALIB-STUFF)
 (MAIN MAIN))

;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "idea7.sch")

(set! *program* "idea7")
(set! *panic?* #f)

(define dummy maximum minimum)

(define (stuff n v box? overlay?)
 (for-each-frame-pair
  (lambda (f) (vector f (read-pnm (ppm-pathname v f))))
  (lambda (prev next)
   (format #t "~a~%" (x prev))
   (let* ((pbm (ppm-absolute-difference (y prev) (y next)))
	  (points
	   (take-if-possible
	    n
	    (sort
	     (remove-if
	      (lambda (points) (< (length points) 20))
	      (map (lambda (g)
		    (pbm->points (graph->pbm g (pnm-height pbm) (pnm-width pbm))))
		   (connected-components
		    (pbm->graph
		     (pgm->pbm pbm 10)
		     5))))
	     >
	     length)))
	  (points (if box? (map points->bounding-box-points points) points)))
    (write-pnm
     (if overlay?
	 (begin
	  (for-each
	   (lambda (p)
	    (set-ppm-pixel!
	     (y prev) (x p) (y p)
	     (if box?
		 '#(255 0 0)
		 (map-vector quantize-coordinate
			     (k*v (/ (z (rgb->hsv (image-ref (y prev) p))) *max-value*)
				  `#(255 0 0))))))
	   (map quantize-point
		(reduce append
			(if box?
			    points
			    (map (lambda (ps)
				  (let ((p (fill-polygon
					    (concave-hull ps 50)
					    `#(,(minimum (map x ps))
					       ,(minimum (map y ps)))
					    `#(,(maximum (map x ps))
					       ,(maximum (map y ps))))))
				   (format #t "~a~%" (length ps))
				   (format #t "~a~%" (length (concave-hull ps 50)))
				   (format #t "~a~%" (length p))
				   p))
				 points))
			'())))
	  (y prev))
	 (pbm->ppm
	  (map-reduce
	   pbm-or
	   (pbm-constant (pnm-width pbm) (pnm-height pbm) #f)
	   (lambda (p) (points->pbm p (pnm-height pbm) (pnm-width pbm)))
	   points)))
     (format #f "/tmp/frame-~a-~a.ppm" (darpa-video->name v) (x prev)))))
  v))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (exactly-one ("outlines" outlines? (outlines "n" integer-argument 1)))
       (at-most-one ("box" box?))
       (at-most-one ("overlay" overlay?)))
 (stuff outlines
	(cond (standard?
	       (standard-corpus-video corpus sequence person location n))
	      (darpa? (string->darpa-video name))
	      (else (fuck-up)))
	box?
	overlay?))
