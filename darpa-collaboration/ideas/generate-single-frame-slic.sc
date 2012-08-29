(MODULE
  GENERATE-SINGLE-FRAME-SLIC
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
(include "generate-single-frame-slic.sch")

(set! *program* "generate-single-frame-slic")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level


(define-command
 (main (exactly-one ("file" file? (file "file" string-argument "")))
       (at-most-one ("n" n? (n "integer" integer-argument 1000)))
       (at-most-one ("weight" weight? (weight "integer" integer-argument 15))))
 (system (format #f "cd /tmp; ~a/darpa-collaboration/bin/slic ~a ~a ~a"
		 (getenv "HOME") file n weight))
 (write-object-to-file
  (let* ((pnm (read-pnm file))
	 (height (pnm-height pnm))
	 (width (pnm-width pnm)))
   (map
    (lambda (r) (make-superpixel (x r) '() '() '() '() (y r)))
    (vector->list
     (frame->regions
      (read-slic-file
       (format #f "/tmp/~a.dat" (strip-directory (strip-extension file)))
       width
       height)))))
  (format #f "~a/~a-slic.sc" (directory file) (strip-directory (strip-extension file)))))
