(MODULE
  RENDER-TURBOPIXEL-TRACKING
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
(include "render-turbopixel-tracking.sch")

(set! *program* "render-turbopixel-tracking")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;;  darpa-wrap ./render-turbopixel-tracking -fps 30 -video -darpa Chase2_A1_C1_Act1_PARK1_MC_AFTN_47ce0b23-c5af-11df-a82b-e80688cb869a

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("fps" fps? (fps "n" integer-argument 15)))
       (at-most-one ("destination" destination?
		     (destination "filename" string-argument "")))
       (at-most-one ("video" video?)))
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
	(first-frame (read-pnm (ppm-pathname video-name (video-first-frame video-name))))
	(width (pnm-width first-frame))
	(height (pnm-height first-frame)))
  (turbopixels-load video-name)
  (let* ((t-names (turbopixels-names))
	 (t-speed (turbopixels-speed))
	 (t-speed-reliable? (turbopixels-speed-reliable?))
	 (t-centers (turbopixels-centers))
	 (last-turbopixels '()))
   (rm (format #f "/tmp/frame-~a-*.ppm" (getpid)))
   (for-each-frame
    (lambda (frame)
     (when (file-exists? (pgm-pathname video-name (+ frame 1)))
      (format #t "~a~%" frame)
      (let* ((f (turbopixels-for-frame (+ frame 1)))
	     (ts (external-turbopixels->turbopixels
		  f (+ frame 1) t-names t-centers t-speed t-speed-reliable?))
	     (image (make-ppm #t 255
			      (make-matrix height width 0)
			      (make-matrix height width 0)
			      (make-matrix height width 0))))
       (update-turbopixel-tracks! ts f last-turbopixels)
       (set! last-turbopixels ts)
       (for-each (lambda (t)
		  (let ((color (turbopixel-random-color t)))
		   (for-each (lambda (p) (set-ppm-pixel! image (x p) (y p) color))
			     (turbopixel-pixels t))))
		 ts)
       (write-pnm image (format #f "/tmp/frame-~a-~a.ppm" (getpid) frame)))))
    video-name))
  (when video?
   (ffmpeg fps (format #f "/tmp/frame-~a-%d.ppm" (getpid))
	   (if destination?
	       destination
	       (generic-full-pathname "/tmp" destination-name
				      (format #f "-turbopixel-tracking.avi")))))))
