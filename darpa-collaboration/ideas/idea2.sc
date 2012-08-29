(MODULE
  IDEA2
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
(include "idea2.sch")

(set! *program* "idea2")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

(define-command (main (at-most-one ("subsample" subsample?))
		      (required (sequence "sequence" string-argument))
		      (required (person "person" string-argument))
		      (required (location "location" string-argument))
		      (required (n "n" integer-argument))
		      (required (delta1 "delta1" integer-argument))
		      (required (delta2 "delta2" integer-argument))
		      (required (threshold1 "threshold1" integer-argument))
		      (required (threshold2 "threshold2" integer-argument))
		      (required (bloat1 "bloat1" integer-argument))
		      (required (bloat2 "bloat2" integer-argument))
		      (required (bloat3 "bloat3" integer-argument))
		      (required (persistence "persistence" integer-argument)))
 (let ((previous-masks '())
       (video-name (standard-corpus-video "office" sequence person location n)))
  (for-each-n
   (lambda (frame)
    (unless (zero? frame)
     (write frame) (newline)
     (let* ((canny
	     (pgm->pbm
	      (read-pnm (canny-pathname video-name frame)) 128))
	    (width (pnm-width canny))
	    (height (pnm-height canny))
	    (previous
	     (read-pnm
	      (pgm-pathname video-name (- frame 1))))
	    (this
	     (read-pnm
	      (pgm-pathname video-name frame)))
	    (next
	     (read-pnm
	      (pgm-pathname video-name (+ frame 1))))
	    (motion-mask
	     (pbm-bloat
	      (pbm-or
	       (pgm->pbm (pgm-absolute-difference this next) threshold1)
	       (pgm->pbm (pgm-absolute-difference previous this) threshold1))
	      bloat1))
	    (contour-mask
	     (pbm-bloat
	      (map-reduce
	       pbm-or
	       (pbm-constant width height #f)
	       (lambda (g)
		(let ((points (line-segments->points
			       (points->line-segments
				(concave-hull
				 (pbm->points (graph->pbm g height width))
				 delta2)))))
		 (if (<= (length points) threshold2)
		     (pbm-constant width height #f)
		     (points->pbm points height width))))
	       (connected-components
		(pbm->graph (pbm-and motion-mask canny) delta1)))
	      bloat2)))
      (write-pnm
       (pbm->pgm ((if subsample? subsample-pbm identity)
		  (pbm-and canny
			   (pbm-or contour-mask
				   (reduce pbm-or
					   previous-masks
					   (pbm-constant width height #f))))))

       (foo-pathname video-name frame))
      (when (positive? persistence)
       (set! previous-masks
	     (cons (pbm-bloat (pbm-and canny contour-mask) bloat3)
		   (if (< (length previous-masks) persistence)
		       previous-masks
		       (but-last previous-masks))))))))
   (- (video-length video-name) 1))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
