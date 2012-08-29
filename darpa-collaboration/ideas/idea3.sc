(MODULE
  IDEA3
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
(include "idea3.sch")

(set! *program* "idea3")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

(define-command (main1 (required (sequence "sequence" string-argument))
		       (required (person "person" string-argument))
		       (required (location "location" string-argument))
		       (required (n "n" integer-argument))
		       (required (threshold1 "threshold1" integer-argument))
		       (required (delta "delta" integer-argument))
		       (required (span "span" integer-argument))
		       (required (threshold2 "threshold2" integer-argument))
		       (required (threshold3 "threshold3" integer-argument)))
 (let ((video-name (standard-corpus-video "office" sequence person location n)))
  (pbms->avi-file
   (conjure
    (map-n
     (lambda (frame)
      (write frame) (newline)
      (pgm->pbm
       (ppm-absolute-difference
	(read-pnm (ppm-pathname video-name frame))
	(read-pnm (ppm-pathname video-name (+ frame 1))))
       threshold1))
     (- (video-length video-name) 1))
    delta
    span
    threshold2
    threshold3)
   15					;hardwired
   (tmp-avi-pathname video-name))))

(define-command (main2 (required (sequence "sequence" string-argument))
		       (required (person "person" string-argument))
		       (required (location "location" string-argument))
		       (required (n "n" integer-argument))
		       (required (threshold1 "threshold1" integer-argument))
		       (required (delta "delta" integer-argument))
		       (required (threshold2 "threshold2" integer-argument)))
 (let ((video-name (standard-corpus-video "office" sequence person location n)))
  (pbms->avi-file
   (map-n (lambda (frame)
	   (write frame) (newline)
	   (connected-component-filter
	    (pgm->pbm
	     (ppm-absolute-difference
	      (read-pnm (ppm-pathname video-name frame))
	      (read-pnm (ppm-pathname video-name (+ frame 1))))
	     threshold1)
	    delta
	    threshold2))
	  (- (video-length video-name) 1))
   15					;hardwired
   (tmp-avi-pathname video-name))))

(define-command (main (required (sequence "sequence" string-argument))
		      (required (person "person" string-argument))
		      (required (location "location" string-argument))
		      (required (n "n" integer-argument))
		      (required (threshold1 "threshold1" integer-argument))
		      (required (delta "delta" integer-argument))
		      (required (threshold2 "threshold2" integer-argument)))
 (let ((video-name (standard-corpus-video "office" sequence person location n)))
  (for-each-n
   (lambda (frame)
    (write frame) (newline)
    (write-pnm
     (pbm->pgm
      (connected-component-filter
       (pgm->pbm
	(ppm-absolute-difference
	 (read-pnm (ppm-pathname video-name frame))
	 (read-pnm (ppm-pathname video-name (+ frame 1))))
	threshold1)
       delta
       threshold2))
     (bar-pathname video-name frame)))
   (- (video-length video-name) 1))
  (pgm-files->avi-file
   (format #f "/aux/qobi/video-datasets/office/~a-~a-~a-~a"
	   sequence person location (number->string n))
   "bar"
   15					;hardwired
   (tmp-avi-pathname video-name))))

(define-command (main4 (required (sequence "sequence" string-argument))
		       (required (person "person" string-argument))
		       (required (location "location" string-argument))
		       (required (n "n" integer-argument))
		       (required (delta "delta" integer-argument))
		       (required (span "span" integer-argument))
		       (required (threshold2 "threshold2" integer-argument))
		       (required (threshold3 "threshold3" integer-argument)))
 (let ((video-name (standard-corpus-video "office" sequence person location n)))
  (pbms->avi-file
   (conjure
    (map-n
     (lambda (frame)
      (pgm->pbm (read-pnm (bar-pathname video-name frame)) 128))
     (- (video-length video-name) 1))
    delta
    span
    threshold2
    threshold3)
   15					;hardwired
   (tmp-avi-pathname video-name))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
