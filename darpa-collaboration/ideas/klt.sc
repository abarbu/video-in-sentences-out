(MODULE
  KLT
  (WITH
    QOBISCHEME
    XLIB
    TOOLLIB-MATLAB
    TOOLLIB-MISC
    TOOLLIB-C-BINDINGS
    TOOLLIB-IMAGE-PROCESSING
    IDEALIB-HASH-TABLE
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    IDEALIB-MATPLOTLIB
    HMM-WBM
    HMM-TRAIN-CLASSIFY
    CUPEDRO-BINDINGS
    TOOLLIB-CAMERA
    TOOLLIB-HACK-TRACK
    TOOLLIB-HACK-TRACK-DRAWABLE)
  (MAIN MAIN))
;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "klt.sch")

(set! *program* "klt")
(set! *panic?* #t)

;; Run examples:
;; darpa-wrap ./klt -darpa HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2 

(define-command
  (main (exactly-one
	 ("standard" standard?
	  (corpus "corpus" string-argument "")
	  (sequence "sequence" string-argument "")
	  (person "person" string-argument "")
	  (location "location" string-argument "")
	  (n "n" integer-argument 0))
	 ("darpa" darpa? (name "name" string-argument ""))
	 ("stand-alone" stand-alone? (path "path" string-argument ""))
	 ("demo" demo? (demo-name "name" string-argument ""))))
  (let* ((video
	  (cond (standard?
		 (standard-corpus-video corpus sequence person location n))
		(darpa? (string->darpa-video name))
		(stand-alone? (make-stand-alone-video path))
		(demo? (string->demo-video demo-name))
		(else (fuck-up)))))
    (write-klt-movie video (run-cuklt-with-defaults video))))
