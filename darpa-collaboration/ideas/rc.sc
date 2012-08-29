(MODULE
  RC
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
(include "rc.sch")

(set! *program* "rc")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

(define *generated-files* '(".graph" ".lines" ".chains" ".w" "-0.cycle"
			    "-cycle-chains.ppm" "-cycle.ppm" "-cycle.png"))

;;; C Externals

;;; Procedures

(define (w1s worst-solid-edge-weight chains)
 (lambda (chain)
  (let ((achain (find-if (lambda (c) (equal? chain (first c))) chains)))
   (if (zero? (second achain))
       worst-solid-edge-weight
       (/ 1 (* (second achain) (length (first achain))))))))

(define (remove-closed-chains-weighted chains)
 (remove-if
  (lambda (c) (equal? (first (first c)) (last (first c))))
  chains))

;;; Commands

;;; Top Level

(define (weighted-chains->rc-graph weighted-chains max-solid-w1 alpha delta)
 (chains-weights->rc-graph (map first weighted-chains)
			   (w1s max-solid-w1 weighted-chains)
			   compute-non-simple-polygon-area
			   (compute-dashed-weight1 alpha)
			   compute-dashed-weight2
			   delta))

(define (write-rc-input-files prefix rc-chains)
 (write-rc-output (string-append prefix ".graph")
		  (rc-chains-vertices rc-chains)
		  (rc-chains-solid-edges rc-chains)
		  (rc-chains-dashed-edges rc-chains))
 (write-solid-lines (string-append prefix ".lines")
		    (rc-chains-solid-edges rc-chains))
 (write-object-to-file rc-chains (string-append prefix ".chains"))
 (write-w-file (convert-graph (rc-chains->graph rc-chains))
	       (string-append prefix ".w")))

(define (cp-numbered-rc-cycle-files i prefix)
 (for-each (lambda (name) (system (format #f "cp ~a~a ~a-~a~a" prefix name prefix i name)))
	   *generated-files*))

(define (rm-temp-rc-cycle-files prefix)
 (for-each (lambda (name) (system (format #f "rm ~a~a" prefix name)))
	   *generated-files*))

(define-command (main (at-most-one ("weighted-chains"
				    weighted-chains?
				    (weighted-chains-filename "filename" string-argument "")))
		      (at-most-one ("alpha" alpha? (alpha "name" real-argument 1.0)))
		      (at-most-one ("contours" contours? (contours "nr" integer-argument 1)))
		      (required (sequence "sequence" string-argument))
		      (required (person "person" string-argument))
		      (required (location "location" string-argument))
		      (required (n "n" integer-argument))
		      (required (frame "frame" integer-argument))
		      (required (pgm-file "pgm-file" string-argument))
		      (required (postfix "postfix" string-argument))
		      (required (delta "delta" integer-argument)))
 (let* ((video-name (standard-corpus-video "office" sequence person location n))
	(pgm (read-pnm (generic-pathname video-name frame
					 (default-extension pgm-file "pgm"))))
	(prefix
	 (generic-pathname video-name frame "rc"))
	(weighted-chains
	 (if weighted-chains?
	     (remove-closed-chains-weighted
	      (remove-if (lambda (ac) (< (length (first ac)) 2))
			 (read-object-from-file
			  (generic-pathname video-name frame
					    weighted-chains-filename))))
	     '()))
	(rc-chains (rc-graph-pgm->rc-chains
		    pgm
		    (if weighted-chains?
			(weighted-chains->rc-graph weighted-chains 1000 alpha delta)
			(begin
			 (format #t "Graph~%")
			 (pgm->rc-graph pgm alpha delta))))))
  (let loop ((rc-chains rc-chains) (iteration 0))
   (format #t "Loop ~a ~a~%" iteration  (length (rc-chains-solid-edges rc-chains)))
   (unless (= (length (rc-chains-vertices rc-chains))
	      (length (rc-chains-solid-edges rc-chains)))
    (fuck-up))
   (write-rc-input-files prefix rc-chains)
   (system (format #f "RatioContour3 ~a" prefix))
   (format #t "Finished RC ~a~%" iteration)
   (let* ((rc-chains (read-object-from-file (string-append prefix ".chains")))
	  (cycle (read-cycle-chains rc-chains prefix 0))
	  (solid-edges (rc-chains-solid-edges rc-chains))
	  (interior-points (points-in-cycle
			    (map
			     point
			     (append (map solid-edge-u solid-edges)
				     (map solid-edge-v solid-edges)))
			    cycle))
	  (solid-edges (remove-if
			(lambda (e)
			 (and (member (point (solid-edge-u e)) interior-points)
			      (member (point (solid-edge-v e)) interior-points)))
			solid-edges)))
    (system (format #f "cp ~a ~a~%"
		    (generic-pathname video-name frame
				      (default-extension pgm-file "pgm"))
		    (string-append prefix ".pgm")))
    (rc-draw-cycle-edges prefix cycle)
    (rc-draw-cycle-chains prefix cycle)
    (rc-draw-cycle prefix cycle)
    (system (format #f "rm ~a~%" (string-append prefix ".pgm")))
    (cp-numbered-rc-cycle-files iteration prefix)
    (rm-temp-rc-cycle-files prefix)
    (format #t "Filtered ~a ~a~%" iteration (length solid-edges))
    (when (< iteration (- contours 1))
     (loop (rc-graph-pgm->rc-chains
	    pgm
	    (chains-weights->rc-graph (map solid-edge-pixels solid-edges)
				      (if weighted-chains?
					  (w1s 1000 weighted-chains)
					  (lambda (_) 0))
				      compute-non-simple-polygon-area
				      (compute-dashed-weight1 alpha)
				      compute-dashed-weight2
				      delta))
	   (+ iteration 1)))))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
