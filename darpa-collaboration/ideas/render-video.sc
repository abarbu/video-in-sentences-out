(MODULE
  RENDER-VIDEO
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
(include "render-video.sch")

(set! *program* "render-video")
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
		      (required (filename "pgm-file" string-argument)))
 (let ((video-name (standard-corpus-video "office" sequence person location n)))
  (pbms->avi-file
   (map-m-n
    (lambda (frame)
     (write frame) (newline)
     (if (file-exists? (generic-pathname video-name frame
					 filename))
	 (pgm->pbm
	  (read-pnm
	   (generic-pathname video-name frame
			     filename))
	  1)
	 (begin (format #t "Frame ~a is missing~%" frame)
		(pbm-constant 320 240 #f))))
    1
    (video-length video-name))
   15					;hardwired
   (tmp-avi-pathname video-name))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
