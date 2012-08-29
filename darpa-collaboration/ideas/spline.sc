(MODULE
  SPLINE
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
(include "spline.sch")

(set! *program* "spline")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

(define-command (main (required (sequence "sequence" string-argument))
					  (required (person "person" string-argument))
					  (required (location "location" string-argument))
					  (required (n "n" integer-argument))
					  (required (knots "knots" integer-argument))
					  (required (output "output" string-argument))
					  (optional (max-frames "max-frames" integer-argument infinity)))
 (let ((video-name (standard-corpus-video "office" sequence person location n)))
  (pbms->avi-file
   (map-m-n
    (lambda (frame)
     (write frame) (newline)
     (let* ((prefix (string-append *video-pathname*
								   sequence
								   "-"
								   person
								   "-"
								   location
								   "-"
								   (number->string n)
								   "/"
								   (number->padded-string-of-length 0 4)
								   "/foo"))
			(chains (read-object-from-file (string-append prefix ".chains")))
			(cycle (read-cycle-chains chains prefix 0)))
      (points->pbm (fit-spline-to-cycle cycle knots) 240 320)))
    1
    (min max-frames (- (video-length video-name) 1)))
   15									;hardwired
   (tmp-avi-pathname video-name))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
