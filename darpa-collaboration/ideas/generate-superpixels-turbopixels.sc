(MODULE
  GENERATE-SUPERPIXELS-TURBOPIXELS
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
(include "generate-superpixels-turbopixels.sch")

(set! *program* "generate-superpixels-turbopixels")
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

;; darpa-wrap ./generate-superpixels-turbopixels  -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a -prefix frame-full
(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))
		    ("stand-alone" stand-alone? (path "path" string-argument "")))
       (exactly-one ("prefix" prefix? (prefix "file" string-argument ""))))
 (let ((video-name
	(cond (standard?
	       (standard-corpus-video corpus sequence person location n))
	      (darpa? (string->darpa-video name))
	      (stand-alone? (make-stand-alone-video path))
	      (else (fuck-up)))))
  (write video-name) (newline)
  (turbopixels-load video-name)
  (let ((turbopixels-names (turbopixels-names))
	(turbopixels-speed (turbopixels-speed))
	(turbopixels-speed-reliable? (turbopixels-speed-reliable?))
	(turbopixels-centers (turbopixels-centers)))
   (for-each-frame-pair
    (lambda (frame)
     (format #t "Frame ~a~%" frame)
     (let* ((turbopixels-map (fill-matrix-gaps (turbopixels-for-frame frame) 0))
	    (turbopixels (external-turbopixels->turbopixels
			  turbopixels-map
			  frame
			  turbopixels-names turbopixels-centers
			  turbopixels-speed turbopixels-speed-reliable?)))
      (list turbopixels-map turbopixels frame)))
    (lambda (prev next)
     (update-turbopixel-next! (second prev) (first prev) (second next))
     (write-object-to-file
      (map turbopixel->superpixel (second prev))
      (turbopixel-pathname video-name (third prev) prefix)))
    video-name))))
