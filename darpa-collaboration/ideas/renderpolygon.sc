(MODULE
  RENDERPOLYGON
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
(include "renderpolygon.sch")

(set! *program* "renderpolygon")
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
		      (required (n "n" integer-argument)))
 (let ((video-name (standard-corpus-video "office" sequence person location n)))
  (pbms->avi-file
   (map-m-n
    (lambda (frame)
     (write frame) (newline)
     (if (file-exists? (generic-pathname video-name frame "joined-polygons.sc"))
	 (let ((polygon (read-object-from-file
			 (generic-pathname video-name frame "joined-polygons.sc"))))
	  (if (not (null? polygon))
	      (let ((image (polygon->pbm (car polygon) 240 320)))
	       (write-pnm
		(pbm->pgm image)
		(generic-pathname video-name frame "joined-polygons.pgm"))
	       image)
	      (begin
	       (format #t "Frame ~a has an empty polygon~%" frame)
	       (pbm-constant 320 240 #f))))
	 (begin (format #t "Frame ~a has an no polygon~%" frame)
		(pbm-constant 320 240 #f))))
    1
    (video-length video-name))
   15					;hardwired
   (tmp-avi-pathname video-name))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
