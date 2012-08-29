(MODULE
  RENDER-DARPA-STUFF
  (WITH
    QOBISCHEME
    XLIB
    CUPEDRO-BINDINGS
    HMM-TRAIN-CLASSIFY
    HMM-WBM
    IDEALIB-HASH-TABLE
    IDEALIB-MATPLOTLIB
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    TOOLLIB-C-BINDINGS
    TOOLLIB-CAMERA
    TOOLLIB-HACK-TRACK-DRAWABLE
    TOOLLIB-HACK-TRACK
    TOOLLIB-IMAGE-PROCESSING
    TOOLLIB-MATLAB
    TOOLLIB-MISC)
  (MAIN MAIN))
;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "render-darpa-stuff.sch")

(set! *program* "render-darpa-stuff")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Commands

;;; Top Level

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))
		    ("stand-alone" stand-alone? (path "path" string-argument "")))
       (any-number ("detector-boxes" detector-boxes?
		    (detector-boxes-type "type" string-argument)
       		    (detector-boxes-label "label" string-argument)))
       (any-number ("predicted-boxes" predicted-boxes?
       		    (predicted-boxes-type "type" string-argument)
		    (predicted-boxes-label "label" string-argument)))
       (any-number ("tracked-box" tracked-box?
       		    (tracked-box-type "type" string-argument)
		    (tracked-box-label "label" string-argument)
       		    (tracked-box-number "number" integer-argument)))
       (any-number ("smooth-tracked-box" smooth-tracked-box?
       		    (smooth-tracked-box-type "type" string-argument)
		    (smooth-tracked-box-label "label" string-argument)
       		    (smooth-tracked-box-number "number" integer-argument)))
       (at-most-one ("tiled" tiled?)
		    ;; If these are still needed we can add them back in
       		    ("double" double?
       		     (path2a "path" string-argument "")
       		     (path2b "path" string-argument ""))
       		    ("triple" triple?
       		     (path3a "path" string-argument "")
       		     (path3b "path" string-argument "")
       		     (path3c "path" string-argument ""))
       		    ("quad" quad?
       		     (path4a "path" string-argument "")
       		     (path4b "path" string-argument "")
       		     (path4c "path" string-argument "")
       		     (path4d "path" string-argument "")))
       (at-most-one ("legend" legend?))
       (at-most-one ("black-background" black-background?))
       (at-most-one ("parts" parts?))
       (at-most-one ("top" top? (top "n" integer-argument infinity)))
       (at-most-one ("cutoff" cutoff? (cutoff "n" integer-argument infinity)))
       (at-most-one ("thickness" thickness? (thickness "n" integer-argument 3)))
       (at-most-one ("video" video?
       		     (video-name "filename" string-argument "/tmp/foo.avi")))
       (at-most-one ("name" name?))
       (at-most-one ("frame-name" frame-name?
		     (frame-name "name" string-argument "frame")))
       (at-most-one ("video-datasets" video-datasets?
		     (video-datasets "pathname" string-argument "")))
       (at-most-one ("fps" fps? (fps "n" integer-argument 30)))
       (at-most-one ("video-caption" video-caption?
                     (video-caption "video-caption" string-argument "")))
       (at-most-one ("rank-box-colors" rank-box-colors?)))
 (when video-datasets? (set! *video-pathname* video-datasets))
 (define (topn boxes-movie)
  (if top?
      (map (lambda (boxes)
	    (let ((boxes (take-if-possible top (sort boxes > voc4-detection-strength))))
	     (if cutoff?
		 (remove-if (lambda (b) (< (voc4-detection-strength b) cutoff)) boxes)
		 boxes)))
	   boxes-movie)
      boxes-movie))
 (let*
   ((video-names
     (cond (double? (list (make-stand-alone-video path2a)
			  (make-stand-alone-video path2b)))
	   (triple? (list (make-stand-alone-video path3a)
			  (make-stand-alone-video path3b)
			  (make-stand-alone-video path3c)))
	   (quad? (list (make-stand-alone-video path4a)
			(make-stand-alone-video path4b)
			(make-stand-alone-video path4c)
			(make-stand-alone-video path4d)))
	   (standard? (list (standard-corpus-video corpus sequence person location n)))
	   (darpa? (list (string->darpa-video name)))
	   (stand-alone? (list (make-stand-alone-video path)))
	   (else (fuck-up))))
    (sample-scale (/ (x (video-dimensions (first video-names))) 1280))
    (detector-boxes
     (begin
      (display "Reading detector boxes")(newline)
      (map
       (lambda (video-name)
	(cond
	 (detector-boxes?
	  (map (lambda (type label)
		(list (list type label 0 "boxes")
		      (topn (read-detector-boxes video-name type label))))
	       detector-boxes-type detector-boxes-label))
	 ((or tracked-box? smooth-tracked-box?) '())
	 (else
	  (map (lambda (ab) (list ab
				  (topn
				   (read-detector-boxes
				    video-name (first ab) (second ab)))))
	       (video-voc4-detector-boxes-available video-name)))))
       video-names)))
    (predicted-boxes
     (begin
      (display "Reading predicted boxes")(newline)
      (map
       (lambda (video-name)
	(cond (predicted-boxes?
	       (map (lambda (type label)
		     (list (list type label "predicted_boxes")
			   (topn
			    (read-predicted-boxes video-name type label))))
		    predicted-boxes-type predicted-boxes-label))
	      ((or detector-boxes? tracked-box? smooth-tracked-box?) '())
	      (else
	       (map (lambda (ab) (list ab
                                       (topn (read-predicted-boxes video-name
								   (first ab)
								   (second ab)))))
		    (video-voc4-predicted-boxes-available video-name)))))
       video-names)))
    (tracked-boxes
     (begin
      (display "Reading tracked boxes")(newline)
      (map (lambda (video-name)
	    (cond (tracked-box?
		   (map (lambda (type label number)
			 (list (list type label number "tracked_box")
			        (read-tracked-boxes video-name type label number)))
			tracked-box-type tracked-box-label tracked-box-number))
		  ((or predicted-boxes? detector-boxes? smooth-tracked-box?) '())
		  (else
		   (map
		    (lambda (ab)
		     (list ab (read-tracked-boxes
				video-name (first ab) (second ab) (third ab))))
		    (video-voc4-tracked-boxes-available video-name)))))
	   video-names)))
    (smooth-tracked-boxes
     (begin
      (display "Reading smooth boxes")(newline)
      (map (lambda (video-name)
	    (cond
	     (smooth-tracked-box?
	      (map (lambda (type label number)
		    (list (list type label number "smooth_tracked_box")
			   (read-smooth-tracked-boxes video-name type label number)))
		   smooth-tracked-box-type smooth-tracked-box-label
		   smooth-tracked-box-number))
	     ((or predicted-boxes? detector-boxes? tracked-box?) '())
	     (else
	      (map
	       (lambda (ab)
	        (list ab (read-smooth-tracked-boxes
				video-name (first ab) (second ab) (third ab))))
	       (video-voc4-smooth-tracked-boxes-available video-name)))))
	   video-names)))
    (nr-boxes
     (+ (or (maximum (map (lambda (a) (length (second a))) (join detector-boxes))) 0)
	(or (maximum (map (lambda (a) (length (second a))) (join tracked-boxes))) 0)
	(or (maximum (map (lambda (a) (length (second a))) (join predicted-boxes))) 0)
	(or (maximum (map (lambda (a) (length (second a))) (join smooth-tracked-boxes))) 0)))
    (colours '(#(255 0 0) #(0 128 255) #(128 255 0) #(0 255 128) 
	       #(0 255 0) #(0 0 255) #(255 128 0) #(255 0 128) 
	       #(128 0 255))))
  (when (> nr-boxes (length colours))
   (set! colours (append colours (generate-colours-n (- nr-boxes 4)))))
  (display "Done reading")(newline)
  (unless (or detector-boxes? tracked-box? smooth-tracked-box?)
   (format #t "Automatically found boxes: ~%")
   (pp (map first (first smooth-tracked-boxes)))
   (newline))
  (let* ((dimensions (video-dimensions (first video-names))))
   (for-each-imlib-frame-from-video-indexed
    (lambda (frame index image)
     (format #t "Frame ~a~%" frame)
     (imlib-context-set-image! image)
     (let ((scaled-image (imlib-create-cropped-scaled-image 0 0
							    (x dimensions) (y dimensions)
							    1280
							    (/ (* 1280 (y dimensions))
							       (x dimensions)))))
      (imlib-free-image-and-decache)
      (set! image scaled-image)
      (imlib-context-set-image! image)
      (when black-background? (panic "Buggy"))
      (let ((image (if black-background?
		       (imlib-create-image
			(imlib-get-image-width)
			(imlib-get-image-height))
		       image)))
       (when black-background?
	(imlib-context-set-image! image)
	(imlib-context-set-color! 0 0 0 255)
	(imlib-image-fill-rectangle
	 0 0 (imlib-get-image-width) (imlib-get-image-height)))
       (cond
	(tiled?
	 (draw-tiled
	  (first detector-boxes) (first predicted-boxes)
	  (first tracked-boxes) (first smooth-tracked-boxes)
	  (first video-names) image frame index thickness name? legend?
	  colours frame-name video-caption  rank-box-colors?))
	;; If these are still needed we can add them back in
	(double? (fuck-up))
	(triple? (fuck-up))
	(quad? (fuck-up))
	(else
	 (draw-single-f
	       (if (and (not detector-boxes?) (not predicted-boxes?)
			(not tracked-box?) (not smooth-tracked-box?))
		   (join detector-boxes)
		   (join
		    (map (lambda (selected? boxes) (if selected? (first boxes) '()))
			 (list detector-boxes? predicted-boxes? tracked-box? smooth-tracked-box?)
			 (list detector-boxes predicted-boxes tracked-boxes smooth-tracked-boxes))))
	       (first video-names) image frame index thickness name? colours parts?
	       frame-name (lambda () #f)  rank-box-colors?)))
       (when black-background?
	(imlib-context-set-image! image)
	(imlib-free-image-and-decache))
       (imlib-context-set-image! image)
       (imlib-free-image-and-decache))))
    (first video-names)))
  (when video?
   (ffmpeg fps
	   (generic-full-pathname
	    *video-pathname*
	    (first video-names)
	    (format #f "/%06d/~a.png" frame-name))
	   video-name))
  (format #t "done~%")))

