(MODULE
  RENDER-SARNOFF-BOXES
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
(include "render-sarnoff-boxes.sch")

(set! *program* "render-sarnoff-boxes")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;; darpa-wrap ./render-sarnoff-boxes -first-frame 1 -fps 30 -darpa 
;; Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a -boxes 
;; /aux/skhan/Purdue36/SampleResults-11302010/BoundingBoxes/Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a.txt

(define dummy drop take)

(define (line->boxes line)
 (let loop ((l (map string->number (fields line))))
  (if (null? l)
      '()
      (cons (take 4 l) (loop (drop 4 l))))))

(define (box->points box)
 (line-segments->points
  (list (make-line-segment `#(,(second box) ,(first box))
			   `#(,(fourth box) ,(first box)))
	(make-line-segment `#(,(fourth box) ,(first box))
			   `#(,(fourth box) ,(third box)))
	(make-line-segment `#(,(fourth box) ,(third box))
			   `#(,(second box) ,(third box)))
	(make-line-segment `#(,(second box) ,(third box))
			   `#(,(second box) ,(first box))))))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("first-frame" first-frame? (first-frame "n" integer-argument 0)))
       (at-most-one ("last-frame" last-frame? (last-frame "n" integer-argument 0)))
       (at-most-one ("box-name" box-name? (box-name "name" string-argument "voc4-person-1.tracked_box")))
       (at-most-one ("fps" fps? (fps "n" integer-argument 15)))
       (at-most-one ("destination" destination?
		     (destination "filename" string-argument "")))
       (at-most-one ("video" video?)))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up))))
	(destination-name
	 (cond (standard?
		(standard-corpus-video "" sequence person location n))
	       (darpa? (string->darpa-video-name name))
	       (else (fuck-up))))
	(start (if first-frame? first-frame (video-first-frame video-name)))
	(end (if last-frame? last-frame
		 (if darpa?
		     (- (video-length video-name) 1)
		     (video-length video-name))))
	(first-frame (read-pnm (ppm-pathname video-name start)))
	(width (pnm-width first-frame))
	(height (pnm-height first-frame)))
  (map-m-n
   (lambda (frame)
    (format #t "F ~a~%" frame)
    (let ((image (read-pnm (ppm-pathname video-name frame))))
     (for-each (lambda (ps) (for-each
			(lambda (p)
			 (set-ppm-pixel! image
					 (min (max 0 (quantize-coordinate (x p))) (- width 1))
					 (min (max 0 (quantize-coordinate (y p))) (- height 1))
					 '#(255 0 0)))
			ps))
	       (map box->points
		    (map-reduce
		     append
		     '()
		     line->boxes
		     (let ((filename (generic-full-pathname
				      *video-pathname* video-name
				      (format #f "/~a/~a" 
					      (number->padded-string-of-length frame 4)
					      box-name))))
		      (if (file-exists? filename)
			  (read-file filename)
			  (begin (write filename) (newline) '()))))))
     (pnm-movie->video-file (vector image)
			    (generic-full-pathname
			     *video-pathname* video-name
			     (format #f "/~a/frame-box.jpeg"
				     (number->padded-string-of-length frame 4)))
			    "Jpeg")))
   start
   end)
  (when video?
   (ffmpeg fps
	   (generic-full-pathname
	    *video-pathname* video-name
	    "/%04d/frame-box.jpeg")
	   destination)
   )
  ))
