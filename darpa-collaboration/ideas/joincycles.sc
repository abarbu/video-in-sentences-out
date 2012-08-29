(MODULE
  JOINCYCLES
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
(include "joincycles.sch")

(set! *program* "joincycles")
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
					  (required (frame "frame" integer-argument))
					  (required (upto "upto" integer-argument)))
 (let ((video-name (standard-corpus-video "office" sequence person location n)))
  (write-object-to-file
   (merge-rc-cycles video-name frame upto)
   (generic-pathname video-name frame "joined-polygons.sc"))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
