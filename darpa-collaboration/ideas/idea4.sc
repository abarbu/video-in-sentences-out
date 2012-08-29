(MODULE
  IDEA4
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
(include "idea4.sch")

(set! *program* "idea4")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

(define-command (main (required (sequence "sequence" string-argument))
		      (required (person "person" string-argument))
		      (required (location "location" string-argument))
		      (required (n "n" integer-argument))
		      (required (threshold1 "threshold1" integer-argument))
		      (required (bloat1 "bloat1" integer-argument))
		      (required (length1 "length1" integer-argument))
		      (required (threshold2 "threshold2" real-argument))
		      (required (bloat2 "bloat2" integer-argument))
		      (required (threshold3 "threshold3" real-argument))
		      (required (threshold4 "threshold4" real-argument)))
 (let* ((video-name (standard-corpus-video "office" sequence person location n))
	(previous-result
	 (let ((pgm (read-pnm (pgm-pathname video-name 0))))
	  (pbm-constant (pnm-width pgm) (pnm-height pgm) #f))))
  (pbms->avi-file
   (map-m-n
    (lambda (frame)
     (write frame) (newline)
     (let* ((previous
	     (read-pnm (pgm-pathname video-name (- frame 1))))
	    (this (read-pnm (pgm-pathname video-name frame)))
	    (next
	     (read-pnm (pgm-pathname video-name (+ frame 1))))
	    (motion-mask
	     (pbm-bloat
	      (pbm-or
	       (pgm->pbm (pgm-absolute-difference previous this) threshold1)
	       (pgm->pbm (pgm-absolute-difference this next) threshold1))
	      bloat1))
	    (pbm (chains->pbm
		  (remove-if-not
		   (lambda (chain)
		    (let ((ratio
			   (/ (count-if
			       (lambda (point)
				(matrix-ref
				 (pbm-bitmap motion-mask) (y point) (x point)))
			       chain)
			      (length chain))))
		     (or (> ratio threshold2)
			 (and (< ratio threshold3)
			      (> (/ (count-if
				     (lambda (point)
				      (matrix-ref (pbm-bitmap previous-result)
						  (y point)
						  (x point)))
				     chain)
				    (length chain))
				 threshold4)))))
		   (break-chains
		    (pbm->chains
		     (pgm->pbm
		      (read-pnm
		       (canny-pathname video-name frame)) 128))
		    length1))
		  (pnm-height this)
		  (pnm-width this))))
      (set! previous-result (pbm-bloat pbm bloat2))
      pbm))
    1
    (- (video-length video-name) 2))
   15					;hardwired
   (tmp-avi-pathname video-name))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
