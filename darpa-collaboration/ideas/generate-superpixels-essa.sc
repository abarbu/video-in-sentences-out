(MODULE
  GENERATE-SUPERPIXELS-ESSA
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
(include "generate-superpixels-essa.sch")

(set! *program* "generate-superpixels-essa")
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

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (exactly-one ("file" file? (file "file" string-argument "")))
       (at-most-one ("hierarchy" hierarchy?)))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up))))
	(essa-first (read-object-from-file
		     (essa-pathname video-name
				    (video-first-frame video-name)))))
  (for-each-frame
   (lambda (frame)
    (format #t "Frame ~a~%" frame)
    (write-object-to-file
     (reduce
      append
      '()
      (if hierarchy?
	  (map-indexed
	   (lambda (s level) (vector->list (map-vector (lambda (s) (format #t "~a~%" s) (essa->superpixel s level)) s)))
	   (cons (D-Rs (F-Ds (read-object-from-file (essa-pathname video-name frame))))
		 (vector->list (map-vector H-CRs (D-Hs (F-Ds essa-first))))))
	  (vector->list (map-vector essa->superpixel-none
				    (D-Rs (F-Ds (read-object-from-file (essa-pathname video-name frame))))))))
     (essa-superpixel-pathname video-name frame "frame")))
   video-name)))
