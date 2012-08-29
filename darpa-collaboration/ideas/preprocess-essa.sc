(MODULE
  PREPROCESS-ESSA
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
(include "preprocess-essa.sch")

(set! *program* "preprocess-essa")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;; ./preprocess-essa -standard "" "a" "foob" "chino" "0"
;; ./preprocess-essa -first-frame 1 -darpa Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("first-frame" first-frame? (first-frame "n" integer-argument 0)))
       (at-most-one ("last-frame" last-frame? (last-frame "n" integer-argument 0)))
       (at-most-one ("level" level? (level "n" integer-argument 0))))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up))))
	(destination-name
	 (cond (standard?
		(standard-corpus-video "" sequence person location n))
	       (darpa? (string->darpa-video-name name))
	       (else (fuck-up))))
	(start (if first-frame? first-frame 0))
	(end (if last-frame? last-frame
		 (if darpa?
		     (- (video-length video-name) 1)
		     (video-length video-name)))))
  (map-m-n
   (lambda (frame)
    (let 
	 ((region-map (essa->region-map (read-object-from-file (essa-pathname video-name frame)) level)))
	 (display (string-append "Frame " (number->string frame)))
	 (newline)
	 (write-object-to-file region-map (regions-pathname video-name frame level)) 
	 (matrix->flatfile region-map (regions-pathname-flat video-name frame level))
     #f))
   start
   end)))
