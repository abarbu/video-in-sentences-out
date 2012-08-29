(MODULE
  ANIMATE-ANNOTATIONS
  (WITH QOBISCHEME XLIB IDEALIB-PREGEXP IDEALIB-STUFF)
  (MAIN MAIN))

;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "animate-annotations.sch")

(set! *program* "animate-annotations")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

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
	(first-frame (read-pnm (berkeley-pathname video-name start)))
	(width (pnm-width first-frame))
	(height (pnm-height first-frame)))
  (map-m-n
   (lambda (frame)
    (let ((annotation (read-object-from-file
		       (human-annotation-pathname video-name frame))))
     (write-pnm
      (annotation->ppm (if keep-unslected?
			   annotation
			   (remove-if
			    (lambda (a) (equal? (annotation-label a) 'none))
			    annotation))
		       width height)
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
