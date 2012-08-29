(MODULE SPLINEFIT (WITH QOBISCHEME XLIB IDEALIB-STUFF) (MAIN MAIN))
;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "splinefit.sch")

(set! *program* "splinefit")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

(define-command (main (at-most-one ("knots" knots? (knots "nr" integer-argument 30)))
		      (required (prefix "prefix" string-argument)))
 (let* ((chains (read-object-from-file (string-append prefix ".chains")))
	(cycle (read-cycle-chains chains prefix 0))
	(knots (if knots? knots (max 3 (/ (length cycle) 2))))
	(spline (fit-spline-to-cycle cycle knots)))
  (format #t "Fit ~s knots to a cycle of size ~s~%"
	  (- (length (spline-coefficients spline))3)
	  (length cycle))
  (let ((spline
	 (if (> (- (length (spline-coefficients spline))3)
		(max 3 (/ (length cycle) 2)))
	     (begin
	      (format #t "Spline was too large, refit with ~s knots to a cycle of size ~s~%"
		      (max 3 (/ (length cycle) 2)) (length cycle))
	      (fit-spline-to-cycle cycle (max 3 (/ (length cycle) 2))))
	     spline)))
   (write-object-to-file (spline-coefficients spline) (string-append prefix "-spline.coefficients"))
   (write-object-to-file (spline-points spline) (string-append prefix "-spline.points"))
   (write-pnm (pbm->pgm (points->pbm (spline-points spline) 240 320))
	      (string-append prefix "-spline.pgm")))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
