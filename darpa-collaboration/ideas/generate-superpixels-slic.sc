(MODULE
 GENERATE-SUPERPIXELS-SLIC
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
(include "generate-superpixels-slic.sch")

(set! *program* "generate-superpixels-slic")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;;  darpa-wrap ./generate-superpixels-slic  -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a -file frame-full -frame 2 -nr-superpixels 10000 -weight 15
;;  darpa-wrap ./generate-superpixels-slic  -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a -file frame-full -frame 2 -nr-superpixels 10000 -weight 15 -box

;; darpa-wrap ./generate-superpixels-slic -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a -file frame-full.ppm


(define dummy superpixels->map)

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (exactly-one ("file" file? (file "file" string-argument "")))
       (at-most-one ("nr-superpixels" nr-superpixels?
		     (nr-superpixels "integer" integer-argument 10000)))
       (at-most-one ("weight" weight? (weight "integer" integer-argument 15)))
       (at-most-one ("box" box?)))
 (let ((video-name
	(cond (standard?
	       (standard-corpus-video corpus sequence person location n))
	      (darpa? (string->darpa-video name))
	      (else (fuck-up))))
       (box-pathname (lambda (p) (if box? (modify-pathname p "-voc4") p))))
  (for-each-frame-pair
   (lambda (frame)
    (let* ((image (read-pnm (ppm-full-pathname video-name frame))))
     (format #t "Frame ~a~%" frame)
     (when box?
      (set! image
	    (crop-voc4 image
		       (voc4-bloat
			(first (read-voc4-boxes
				(voc4-pathname video-name frame)))
			0.2)))
      (write-pnm image (box-pathname (ppm-full-pathname video-name frame))))
     (let ((width (pnm-width image))
	   (height (pnm-height image)))
      (format #t "~a ~a~%" frame (box-pathname file))
      (run-slic video-name frame (box-pathname file) nr-superpixels weight)
      (let ((superpixels
	     (map
	      slic->superpixel
	      (slic-frame->regions
	       (read-slic-file
		(slic-dat-pathname video-name frame (box-pathname file))
		width height))))
	    (flow (read-object-from-file
		   (optical-flow-pathname video-name frame))))
       (for-each (lambda (s)
		  (set-superpixel-velocity! s (average-optical-flow-superpixel s flow)))
		 superpixels)
       `#(,frame ,superpixels ,(superpixels->map superpixels width height))))))
   (lambda (prev next)
    (format #t "Frame ~a ~a~%" (x prev) (x next))
    (for-each (lambda (s) (set-superpixel-next! s (list (superpixel-track s (z next)))))
	      (y prev))
    (write-object-to-file
     (y prev)
     (slic-pathname video-name (x prev) (box-pathname file))))
   video-name)))
