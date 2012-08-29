(MODULE
  SINGLESPLINEFIT
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
(include "singlesplinefit.sch")

(set! *program* "singlesplinefit")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

(define-command (main (at-most-one ("knots" knots? (knots "nr" integer-argument 30)))
		      (required (sequence "sequence" string-argument))
		      (required (person "person" string-argument))
		      (required (location "location" string-argument))
		      (required (n "n" integer-argument))
		      (required (frame "frame" integer-argument)))
 (let* ((video-name (standard-corpus-video "office" sequence person location n))
	(polygons
	 (read-object-from-file (generic-pathname video-name frame "joined-polygons.sc")))
	(points (if (= (length polygons) 1)
		    (car polygons)
		    (begin
		     (display "Polygons")
		     (pp polygons)
		     (newline)
		     (fuck-up))))
	(knots (if knots? knots 0))
	(spline (fit-spline-to-ordered-points points knots)))
  (format #t "Fit ~s knots to a polygon of size ~s~%"
	  (- (length (spline-coefficients spline))3)
	  (length points))
  (let ((spline spline))
   (write-object-to-file (spline-coefficients spline)
			 (generic-pathname video-name frame "joined-polygons-spline.coefficients"))
   (write-object-to-file (spline-points spline)
			 (generic-pathname video-name frame "joined-polygons-spline.points"))
   (write-pnm (pbm->pgm (points->pbm (spline-points spline) 240 320))
	      (generic-pathname video-name frame "joined-polygons-spline.pgm")))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
