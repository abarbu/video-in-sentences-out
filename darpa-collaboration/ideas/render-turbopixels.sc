(MODULE
  RENDER-TURBOPIXELS
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
(include "render-turbopixels.sch")

(set! *program* "render-turbopixels")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;;  darpa-wrap ./render-turbopixels -frame-difference -fps 30 -video  -darpa Chase2_A1_C1_Act1_PARK1_MC_AFTN_47ce0b23-c5af-11df-a82b-e80688cb869a
;;  darpa-wrap ./render-turbopixels -superpixel-velocity -fps 30 -video  -darpa Chase2_A1_C1_Act1_PARK1_MC_AFTN_47ce0b23-c5af-11df-a82b-e80688cb869a



(define (ppm-raw p)
 (make-ppm #t (ppm-maxval p) (ppm-red p) (ppm-green p) (ppm-blue p)))

(define-command
 (main (exactly-one ("standard" standard?
					 (corpus "corpus" string-argument "")
					 (sequence "sequence" string-argument "")
					 (person "person" string-argument "")
					 (location "location" string-argument "")
					 (n "n" integer-argument 0))
					("darpa" darpa? (name "name" string-argument "")))
       (exactly-one ("frame-difference" frame-difference?)
					("superpixel-velocity" superpixel-velocity?)
					("superpixel-speed" superpixel-speed?))
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
  (let ((t-names (turbopixel-names))
		(t-speed (turbopixel-speed))
		(t-speed-reliable? (turbopixel-speed-reliable?))
		(t-area (turbopixel-area)))
   (rm (format #f "/tmp/frame-~a-*.ppm" (getpid)))
   (for-each-frame
    (lambda (f)
     (when (file-exists? (pgm-pathname video-name (+ f 1)))
      (format #t "~a~%" f)
      (write-pnm
       (ppm-raw (pgm->ppm
				 (pbm->pgm
				  (turbopixel-regions->pbm
				   (cond (frame-difference?
						  (moving-turbopixels (turbopixels->regions (turbopixels-for-frame f))
											  (frame-absolute-difference video-name f)
											  2.6))
						 (superpixel-velocity?
						  (moving-turbopixels (turbopixels->regions (turbopixels-for-frame f))
											  (turbopixels-motion 0.55 f)
											  0.7))
						 (superpixel-speed?
						  (remove-if-not (lambda (r)
										  (let ((i (turbopixel-index t-names f (x r))))
										   (and (vector-ref t-speed-reliable? i)
												(> (+ (abs (x (vector-ref t-speed i)))
													  (abs (y (vector-ref t-speed i))))
												   1.5))))
										 (turbopixels->regions (turbopixels-for-frame f))))
						 (else (fuck-up)))
				   width
				   height))))
       (format #f "/tmp/frame-~a-~a.ppm" (getpid) f))))
    video-name)
   (when video?
    ;; (system (format #f "ffmpeg -y -r ~a -i /tmp/frame-~a-%d.ppm -vcodec libx264 -vpre hq -vb 2000000 ~a &> /dev/null"
    ;; 		    fps (getpid)
    ;; 		    (if destination?
    ;; 			destination
    ;; 			(generic-full-pathname "/tmp" destination-name (format #f "-~a.avi" (cond (frame-difference? "fd")
    ;; 												  (superpixel-velocity? "sv")
    ;; 												  (superpixel-speed? "ss")
    ;; 												  (else (fuck-up))))))))
    (format #t "ffmpeg -y -r ~a -i /tmp/frame-~a-%d.ppm -vcodec libx264 -vb 2000000 ~a &> /dev/null"
			fps (getpid)
			(if destination?
				destination
				(generic-full-pathname "/tmp" destination-name (format #f "-~a.avi" (cond (frame-difference? "fd")
																						  (superpixel-velocity? "sv")
																						  (superpixel-speed? "ss")
																						  (else (fuck-up)))))))
    (system (format #f "ffmpeg -y -r ~a -i /tmp/frame-~a-%d.ppm -vcodec libx264 -vb 2000000 ~a &> /dev/null"
					fps (getpid)
					(if destination?
						destination
						(generic-full-pathname "/tmp" destination-name (format #f "-~a.avi" (cond (frame-difference? "fd")
																								  (superpixel-velocity? "sv")
																								  (superpixel-speed? "ss")
																								  (else (fuck-up))))))))
    (rm (format #f "/tmp/frame-~a-*.ppm" (getpid)))))))
