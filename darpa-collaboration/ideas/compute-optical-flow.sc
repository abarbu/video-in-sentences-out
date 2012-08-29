(MODULE
  COMPUTE-OPTICAL-FLOW
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
(include "compute-optical-flow.sch")

(set! *program* "compute-optical-flow")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;;  darpa-wrap ./compute-optical-flow  -darpa Chase2_A1_C1_Act1_PARK1_MC_AFTN_47ce0b23-c5af-11df-a82b-e80688cb869a

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up)))))
  (start-matlab!)
  (for-each-frame
   (lambda (frame)
    (when (file-exists-tmp-pid? (ppm-full-pathname video-name (+ frame 1)))
     (format #t "Frame ~a~%" frame)
     (write-object-to-file
      (optical-flow
       (ppm-full-pathname video-name frame)
       (ppm-full-pathname video-name (+ frame 1)))
      (scheme-optical-flow-pathname video-name frame))))
   video-name)))
