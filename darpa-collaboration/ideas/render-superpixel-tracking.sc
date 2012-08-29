(MODULE
 RENDER-SUPERPIXEL-TRACKING
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
(include "render-superpixel-tracking.sch")

(set! *program* "render-superpixel-tracking")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;;  darpa-wrap ./render-superpixel-tracking -fps 30 -video -darpa Chase2_A1_C1_Act1_PARK1_MC_AFTN_47ce0b23-c5af-11df-a82b-e80688cb869a
;; darpa-wrap ./render-superpixel-tracking -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a -fps 10 -slic

(define (random-rgb)
 (vector
  (random-integer *max-red*)
  (random-integer *max-green*)
  (random-integer *max-blue*)))

(define-command
 (main (exactly-one ("standard" standard?
					 (corpus "corpus" string-argument "")
					 (sequence "sequence" string-argument "")
					 (person "person" string-argument "")
					 (location "location" string-argument "")
					 (n "n" integer-argument 0))
					("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("fps" fps? (fps "n" integer-argument 15)))
       (at-most-one ("motion" motion? (motion "n" integer-argument 2)))
       (at-most-one ("destination" destination?
					 (destination "filename" string-argument "")))
       (at-most-one ("video" video?))
       (exactly-one ("slic" slic?) ("essa" essa?) ("turbopixels" turbopixels?)))
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
		(first-frame (read-pnm (ppm-full-pathname video-name (video-first-frame video-name))))
		(width (pnm-width first-frame))
		(height (pnm-height first-frame))
		(superpixel-type (cond (slic? 'slic)
							   (essa? 'essa)
							   (turbopixels? 'turbopixels)
							   (else (fuck-up)))))
  (rm (format #f "/tmp/frame-~a-*.ppm" (getpid)))
  (for-each-frame-pair
   (lambda (frame)
    (format #t "Frame ~a~%" frame)
    (vector frame
			(map
			 (lambda (s) (vector s (random-rgb) '#(0 0)))
			 (remove-if-not
			  (lambda (s) (or (not (superpixel-children s)) (null? (superpixel-children s))))
			  (read-object-from-file
			   (superpixel-pathname video-name frame "frame-full" superpixel-type))))))
   (lambda (prev next)
    (for-each
     (lambda (s)
      (when (superpixel-next (x s))
       (let ((n (find-if (lambda (n-s) (member (superpixel-name (x n-s)) (superpixel-next (x s))))
						 (y next))))
		(when (and n (>= (magnitude (safe-superpixel-velocity (x s))) (magnitude (z n))))
		 (vector-set! n 1 (y s))
		 (vector-set! n 2 (safe-superpixel-velocity (x s)))))))
     (y prev))
    (let ((image (make-ppm #t 255
						   (make-matrix height width 0)
						   (make-matrix height width 0)
						   (make-matrix height width 0))))
     (for-each (lambda (s)
				(let ((c (superpixel-center (x s))))
				 (when (or (not motion?)
						   (>= (magnitude (safe-superpixel-velocity (x s)))
							   motion))
				  (for-each (lambda (p) (set-ppm-pixel! image (x p) (y p) (y s)))
							(superpixel-pixels (x s)))
				  (for-each (lambda (p) (set-ppm-pixel! image (x p) (y p) '#(255  255 255)))
							(quantize-points
							 (line-segment->points (make-line-segment c (v+ c (safe-superpixel-velocity (x s)))))))
				  (set-ppm-pixel! image (quantize-coordinate (x c)) (quantize-coordinate (y c)) '#(0 0 0)))))
			   (y prev))
     (write-pnm image (format #f "/tmp/frame-~a-~a.ppm" (getpid) (x prev)))))
   video-name)
  (when video?
   (ffmpeg fps (format #f "/tmp/frame-~a-%d.ppm" (getpid))
		   (if destination?
			   destination
			   (generic-full-pathname "/tmp" destination-name
									  (format #f "-~a-tracking.avi"
											  (superpixel-type->string superpixel-type))))))))
