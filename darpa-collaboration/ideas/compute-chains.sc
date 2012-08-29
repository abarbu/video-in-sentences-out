(MODULE
  COMPUTE-CHAINS
  (WITH QOBISCHEME XLIB IDEALIB-PREGEXP IDEALIB-STUFF)
  (MAIN MAIN))

;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "compute-chains.sch")

(set! *program* "compute-chains")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;; ./compute-chains -darpa Approach2_A1_C1_Act1_2_Park3_MC_AFTN_47ff4ac2-c5af-11df-a99b-e80688cb869a

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument ""))
		    ("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("threshold" threshold? (threshold "nr" integer-argument 1)))
       (at-most-one ("break-length" break-length? (break-length "nr" integer-argument 20))))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up)))))
  (map-m-n
   (lambda (n)
    (format #t "~a~%" n)
    (write-object-to-file
     (join (map (lambda (c) (break-chain c break-length))
		(pbm->chains (pgm->pbm
			      (read-pnm (berkeley-pathname video-name n))
			      threshold))))
     (berkeley-chains-pathname video-name n)))
   1
   (video-length video-name))))
