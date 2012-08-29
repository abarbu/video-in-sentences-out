(MODULE
  ANIMATE-ANNOTATIONS-ESSA
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
(include "animate-annotations-essa.sch")

(set! *program* "animate-annotations-essa")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

(define *essa* '())
(define *essa-level* 0)

;;; Parameters

;;; C Externals

;;; Procedures

; Too lazy to generalize this now and integrate into idealib-stuff.sc
; Do this later
(define (essa-annotate-image! annotations image) 
 (for-each 
  (lambda (r)
   (if (not (equal? (annotation-region->annotation-label annotations (first r) 0) 'none))
   (let ((colour (label->colour (annotation-region->annotation-label annotations (first r) 0))))
    (for-each
     (lambda (p)
	  (set-ppm-pixel! image  (x p) (y p) colour))
     (second r)))))
  (essa->region-pixels *essa* *essa-level*))
; (if (not *blank?*)
; (for-each 
;  (lambda (j)  
;   (for-each 
;    (lambda (i) 
;     (set-ppm-pixel! image (x i) (y i) '#(0 0 0)))
;	   (list-ref j 1)))
;   (essa->region-outline *essa* *essa-level*)))
 image)

;;; Commands

;;; Top Level

;; ./animate-annotations -standard "" "a" "foob" "chino" "0"
;; ./animate-annotations -first-frame 1 -fps 30 -darpa Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a

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
       (at-most-one ("fps" fps? (fps "n" integer-argument 15)))
       (at-most-one ("keep-unselected" keep-unslected?))
       (at-most-one ("destination" destination?
		     (destination "filename" string-argument ""))))
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
		     (video-length video-name))))
;	(first-frame (read-pnm (ppm-pathname video-name start)))
	(first-essa (read-object-from-file (essa-pathname video-name start)))
;	(width (pnm-width first-frame))
;	(height (pnm-height first-frame)))
	(width (essa-width first-essa))
	(height (essa-height first-essa)))
  (map-m-n
   (lambda (frame)
    (let 
	 ((annotation (read-object-from-file (human-annotation-essa-pathname video-name frame *essa-level*))))
	 (set! *essa* (read-object-from-file (essa-pathname video-name frame)))
	 (set! *essa-level* 0)
     (write-pnm
	  (essa-annotate-image! annotation (ppm-constant width height 0 0 0))
       (format #f "/tmp/frame~a-~a.ppm" (getpid) frame))
     #f))
   start
   end)
  (system (format #f "ffmpeg -y -r ~a -i /tmp/frame~a-%d.ppm -vcodec libx264 -vpre hq -vb 900000 ~a &> /dev/null"
		  fps (getpid)
		  (if destination?
		      destination
		      (generic-full-pathname "/tmp" destination-name ".avi"))))
  (map-m-n
   (lambda (frame)
    (rm (format #f "/tmp/frame~a-~a.ppm" (getpid) frame)))
   start
   end)))
