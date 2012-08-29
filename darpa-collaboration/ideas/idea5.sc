(MODULE
  IDEA5
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

(include "QobiScheme-AD.sch")
(include "idea5.sch")

(set! *program* "idea5")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

(define (mean-chamfer-distance chain distance-matrix)
 (/ (map-reduce
     +
     0
     (lambda (point) (matrix-ref distance-matrix (y point) (x point)))
     chain)
    (length chain)))

;;; Commands

;;; Top Level

(define-command (main (required (sequence "sequence" string-argument))
		      (required (person "person" string-argument))
		      (required (location "location" string-argument))
		      (required (n "n" integer-argument))
		      (required (length1 "length1" integer-argument))
		      (required (threshold1 "threshold1" real-argument)))
 (let* ((video-name (standard-corpus-video "office" sequence person location n))
	(previous (first
		   (distance-transform
		    (pgm->pbm
		     (read-pnm (canny-pathname video-name 0))
		     128)))))
  (pbms->avi-file
   (map-m-n
    (lambda (frame)
     (write frame) (newline)
     (let* ((this (pgm->pbm
		   (read-pnm (canny-pathname video-name frame))
		   128))
	    (pbm (chains->pbm
		  (remove-if-not
		   (lambda (chain)
		    (and (>= (length chain) length1)
			 (> (mean-chamfer-distance chain previous) threshold1)))
		   (break-chains (pbm->chains this) length1))
		  (pnm-height this)
		  (pnm-width this))))
      (set! previous (first (distance-transform this)))
      pbm))
    1
    (- (video-length video-name) 1))
   15					;hardwired
   (tmp-avi-pathname video-name))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
