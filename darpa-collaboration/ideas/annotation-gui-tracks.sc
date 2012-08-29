(MODULE
  ANNOTATION-GUI-TRACKS
  (WITH
    QOBISCHEME
    XLIB
    TOOLLIB-MATLAB
    TOOLLIB-MISC
    TOOLLIB-C-BINDINGS
    TOOLLIB-IMAGE-PROCESSING
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    IDEALIB-MATPLOTLIB)
  (MAIN MAIN))

;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "annotation-gui-tracks.sch")

(set! *program* "annotation-gui-tracks")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; (run (list (string->darpa-video "BOUNCE10_A1_C1_Act8_URBAN_MC_AFTN_DARK_43797fda-1dc6-11e0-ad1b-e80688ca39a2")))
;;; (run (list (string->darpa-video "CHASE3_A1_C1_Act2_6_URBAN_MC_AFTN_b436e7cc-07b6-11e0-a6d4-e80688cb869a")))
;;; (run (list
;;;        (string->darpa-video "BOUNCE10_A1_C1_Act8_URBAN_MC_AFTN_DARK_43797fda-1dc6-11e0-ad1b-e80688ca39a2")
;;;        (string->darpa-video "CHASE3_A1_C1_Act2_6_URBAN_MC_AFTN_b436e7cc-07b6-11e0-a6d4-e80688cb869a")))

(define *imlib-images* #f)
(define *videos* #f)
(define *first-video* #f)
(define (*video-name*) (first *videos*))
(define (*frame-number*) 1)
(define *tracks* #f)
(define *track-annotation* #f)
(define *shown-frames* #f)
(define *models-available* #f)
(define *imlib-image* #f)
(define *width* #f)
(define *height* #f)
(define *annotated-models* #f)

(define *delete-buttons* '())

(define (redraw)
 (redraw-buttons)
 (redraw-display-pane))

(define (*blend*)
 (imlib-context-set-image! *imlib-image*)
 (imlib-blend-image-onto-image (first *imlib-images*) 150 0 0
			       *width* *height* 0 0
			       (/ *width* 2) (/ *height* 2))
 (imlib-blend-image-onto-image (second *imlib-images*) 150 0 0
			       *width* *height* (/ *width* 2) 0
			       (/ *width* 2) (/ *height* 2))
 (imlib-blend-image-onto-image (third *imlib-images*) 150 0 0
			       *width* *height* 0 (/ *height* 2)
			       (/ *width* 2) (/ *height* 2))
 (imlib-blend-image-onto-image (fourth *imlib-images*) 150 0 0
			       *width* *height* (/ *width* 2) (/ *height* 2)
			       (/ *width* 2) (/ *height* 2)))

(define (load!)
 (display (*video-name*)) (newline)
 (let ((len (video-length (*video-name*))))
  (set! *shown-frames*
	(map exact-round (list (* 2 (/ len 6)) (* 3 (/ len 6)) (* 4 (/ len 6)) (* 5 (/ len 6))))))
 (display (*video-name*)) (newline)	;debugging
 (let ((video (ffmpeg-open-video (mov-pathname (*video-name*)))))
  (set!
   *imlib-images*
   (let loop ()
    (if (ffmpeg-video-finished? video)
	'()
	(if (find (ffmpeg-video-frame-index video) *shown-frames*)
	    (let ((frame (ffmpeg-video-frame-data-as-imlib video)))
	     (cons frame (begin (ffmpeg-next-frame! video) (loop))))
	    (begin (ffmpeg-next-frame! video) (loop))))))
  (ffmpeg-close-video video))
 (imlib-context-set-image! (first *imlib-images*))
 (set! *width* (imlib-get-image-width))
 (set! *height* (imlib-get-image-height))
 (set! *imlib-image* (imlib-create-image *width* *height*))
 (*blend*)
 (set! *models-available*
       (video-tracked-boxes-available (*video-name*) "voc4_overgenerated"))
 (set! *tracks*
       (map
	(lambda (track)
	 (list (second track) (third track)
	       (list->vector
		(read-voc4-overgenerated-tracked-boxes
		 (*video-name*) (second track) (third track)))))
	*models-available*))
 (if (file-exists?
      (human-track-annotation-pathname (*video-name*)))
     (set!
      *track-annotation*
      (read-object-from-file
       (human-track-annotation-pathname (*video-name*))))
     (reset-tracks!))
 (recreate-buttons!))

(define (save!)
 (write-object-to-file
  *track-annotation*
  (human-track-annotation-pathname (*video-name*)))
 (rm (generic-root-pathname (*video-name*) "voc4_human-*.tracked-box"))
 (for-each
   (lambda (annotations)
    (for-each-indexed
     (lambda (annotation i)
      (when (track-annotation-good? annotation)
       (write-box-movie (vector->list (third (get-track annotation *tracks*)))
			(tracked-box-pathname (*video-name*) "voc4_human"
						     (track-annotation-name annotation)
						     (+ i 1)))))
     (remove-if-not track-annotation-good? annotations)))
  (transitive-equivalence-classesp
   (lambda (a b) (equal? (track-annotation-name a) (track-annotation-name b)))
   *track-annotation*)))

(define (recreate-buttons!) #f)

(define (next-unannotated-button!)
 (set! *videos* (ring-backward *videos*))
 (cond ((equal? *first-video* (*video-name*))
	(load!)
	(redraw)
	(message "Returned to the first video in the list"))
       ((not (file-exists? (human-track-annotation-pathname (*video-name*))))
	(load!)
	(redraw)
	(message (string-append "Not annotated: " (darpa-video->string (*video-name*)))))
       (else (next-unannotated-button!))))

;;;

(define (save-button!)
 (save!)
 (statistics-collect-and-write! (*video-name*) 'tracks))

(define (load-button!)
 (load!)
 (statistics-reset!)
 (redraw))

(define (next!)
 (for-each
  (lambda (image)
   (imlib-context-set-image! image)
   (imlib-free-image-and-decache))
  *imlib-images*)
 (set! *videos* (ring-backward *videos*))
 (load!))

(define (previous!)
 (set! *videos* (ring-forward *videos*))
 (load!))

(define (new-number->string-of-length-and-precision number length precision)
 (let* ((negative? (negative? number))
	(integer-part (inexact->exact (floor (abs number))))
	(fraction-part
	 (inexact->exact
	  (floor (* (expt 10 precision) (- (abs number) integer-part)))))
	(integer-part-string (number->string integer-part))
	(fraction-part-string (number->string fraction-part)))
  (if negative?
      (string-append
       (make-string
	(- length (string-length integer-part-string) 2 precision) #\space)
       "-"
       integer-part-string
       "."
       (make-string (- precision (string-length fraction-part-string)) #\0)
       fraction-part-string)
      (string-append
       (make-string
	(- length (string-length integer-part-string) 1 precision) #\space)
       integer-part-string
       "."
       (make-string (- precision (string-length fraction-part-string)) #\0)
       fraction-part-string))))

(define (format-statistics-position-and-time)
 (format #f "~a@~a ~a% ~as"
	 (darpa-video->string (*video-name*))
	 (*frame-number*)
	 (new-number->string-of-length-and-precision
	  (* (/ (position *first-video* *videos*)
		(length *videos*))
	     100)
	  6 2)
	 (new-number->string-of-length-and-precision (statistics-time-elapsed) 8 4)))

(define (next-button!)
 (next!)
 (redraw)
 (message (string-append "Next: " (format-statistics-position-and-time)))
 (statistics-reset!))

(define (previous-button!)
 (previous!)
 (redraw)
 (message (string-append "Previous: " (format-statistics-position-and-time)))
 (statistics-reset!))

(define (save-and-next-button!)
 (save!)
 (let ((msg (format-statistics-position-and-time)))
  (statistics-collect-and-write! (*video-name*) 'tracks)
  (next!)
  (redraw)
  (message (string-append "Save&Next: " msg))))

(define (run videos)
 (set! *delete-buttons* '())
 (imlib-context-disconnect-display)
 (set! *videos* videos)
 (set! *first-video* (first videos))
 (statistics-reset!)
 (load!)
 (viewer '()))

(define (reset-tracks!)
 (set!
  *track-annotation*
  (map (lambda (track)
	(make-track-annotation #f (first track) (second track)))
       *tracks*)))

;;;

(define (reset!)
 (reset-tracks!) (recreate-buttons!))

(define (get-track annotation tracks)
 (find-if (lambda (a)
	   (equal? (list (first a) (second a))
		   (list (track-annotation-name annotation)
			 (track-annotation-number annotation))))
	  tracks))

(define (nth-frame-from-track annotation tracks nth)
 (vector-ref (third (get-track annotation tracks)) nth))

(define (draw-track-annotation annotations nth x-offset y-offset scale)
 (for-each
   (lambda (annotation)
    (unless (track-annotation-good? annotation)
     (let* ((box (nth-frame-from-track annotation *tracks* (nth *shown-frames*))))
      (draw-box *red-gc*
		(exact-round (+ x-offset (/ (voc4-detection-x1 box) scale)))
		(exact-round (+ y-offset (/ (voc4-detection-y1 box) scale)))
		(exact-round (+ x-offset (/ (voc4-detection-x2 box) scale)))
		(exact-round (+ y-offset (/ (voc4-detection-y2 box) scale)))))))
  annotations)
 (for-each
   (lambda (annotation)
    (when (track-annotation-good? annotation)
     (let* ((box (nth-frame-from-track annotation *tracks* (nth *shown-frames*))))
      (draw-box *green-gc*
		(exact-round (+ x-offset (/ (voc4-detection-x1 box) scale)))
		(exact-round (+ y-offset (/ (voc4-detection-y1 box) scale)))
		(exact-round (+ x-offset (/ (voc4-detection-x2 box) scale)))
		(exact-round (+ y-offset (/ (voc4-detection-y2 box) scale)))))))
  annotations))

(define (quadrant x1 y1 width height)
 (cond ((and (< x1 (/ width 2)) (< y1 (/ height 2))) 0)
       ((and (> x1 (/ width 2)) (< y1 (/ height 2))) 1)
       ((and (< x1 (/ width 2)) (> y1 (/ height 2))) 2)
       ((and (> x1 (/ width 2)) (> y1 (/ height 2))) 3)))

(define (point->quadrant-point x1 y1 width height)
 (vector (* (if (< x1 (/ width 2))  x1 (- x1 (/ width  2))) 2)
	 (* (if (< y1 (/ height 2)) y1 (- y1 (/ height 2))) 2)))

(define (define-sized-button x y size offset text-procedure bold?-procedure method)
 (let* ((width (exact-round (* size *button-width*)))
	(button (xcreatesimplewindow
		 *display* *window* (+ (* offset width) (* x (+ *button-width* 4)) 2)
		 (+ (* y (+ *button-height* 4)) 2)
		 width *button-height* 1
		 (xcolor-pixel (second *foreground*))
		 (xcolor-pixel (second *background*)))))
  (when (eq? method abort-command) (set! *abort-button* button))
  (xselectinput
   *display* button (bit-or exposuremask buttonpressmask keypressmask))
  (set-window-method!
   button
   'expose
   (lambda ()
    (let* ((text (if (procedure? text-procedure)
		     (text-procedure)
		     text-procedure))
	   (bold? (if (procedure? bold?-procedure)
		      (bold?-procedure)
		      bold?-procedure))
	   (text-width
	    (xtextwidth
	     (if bold? *bold-font* *roman-font*) text (string-length text)))
	   (text-x (quotient (- width text-width) 2))
	   (text-y (- *button-height* (+ *text-baseline* 2))))
     (xclearwindow *display* button)
     (xdrawstring *display* button (if bold? *bold-gc* *roman-gc*)
		  text-x text-y text (string-length text)))))
  (set-window-method! button 'buttonpress (lambda (x y button state) (method)))
  (set! *buttons* (cons button *buttons*))
  (xmapsubwindows *display* *window*)
  (xmapraised *display* *window*)
  (lambda ()
   (set! *buttons* (remove button *buttons*))
   (set! *window-methods* (remove-if (lambda (m) (equal? (first m) button)) *window-methods*))
   (xdestroywindow *display* button))))

(define (watch-button!)
 (system (string-append "mplayer " (rendered-tracker-video-pathname (*video-name*)))))

(define-application viewer 1300 700 #f 2 8
 (lambda ()
  (setup-extra-x-gcs)
  (define-button 0 0 "Help" #f help-command)
  (define-button 1 0 "Watch" #f watch-button!)
  (define-button 7 0 "Quit" #f quit)
  (define-button 3 0 "Go unannotated" #f next-unannotated-button!)
  (define-button 4 0 "Reset" #f (lambda () (reset!) (redraw)))
  (define-button 0 1 "Load" #f load-button!)
  (define-button 1 1 "Save" #f save-button!)
  (define-button 2 1 "Save & Next" #f save-and-next-button!)
  (define-button 3 1 "Previous" #f previous-button!)
  (define-button 4 1 "Next" #f next-button!)
  ;;
  (define-key (list (control #\x) (control #\c)) "Quit" quit)
  (define-key #\n "Next" next-button!)
  (define-key #\r "Reset" (lambda () (reset!) (redraw)))
  (define-key #\s "Save" save-button!)
  (define-key #\f "Save & Next" save-and-next-button!)
  (define-key #\p "Prev" previous-button!)
  (define-key #\w "Watch" watch-button!)
  (define-key (control #\h) "Help" help-command))
 (lambda () #f)
 (lambda () (free-extra-x-gcs))
 (lambda ()
  (xremove-expose-events)

  (imlib-context-set-image! *imlib-image*)
  (draw-imlib-pixmap *imlib-image* 0 0)
  (draw-track-annotation *track-annotation* first 0 0 2)
  (draw-track-annotation *track-annotation* second (/ *width* 2) 0 2)
  (draw-track-annotation *track-annotation* third 0 (/ *height* 2) 2)
  (draw-track-annotation *track-annotation* fourth (/ *width* 2) (/ *height* 2) 2)

  (define-region 0 0 *display-pane-width* *display-pane-height*
   (lambda (x1 y1)
    (statistics-click!)
    (let* ((quadrant (quadrant x1 y1 *width* *height*))
	   (quadrant-point (point->quadrant-point x1 y1 *width* *height*))
	   (frame (list-ref *shown-frames* quadrant))
	   (annotation
	    (minimump
	     *track-annotation*
	     (lambda (annotation)
	      (let ((detection (nth-frame-from-track annotation *tracks* frame)))
	       (minimum
  		(map (lambda (p) (distance p quadrant-point))
  		     (list
  		      (vector (voc4-detection-x1 detection) (voc4-detection-y1 detection))
  		      (vector (voc4-detection-x1 detection) (voc4-detection-y2 detection))
  		      (vector (voc4-detection-x2 detection) (voc4-detection-y2 detection))
  		      (vector (voc4-detection-x2 detection) (voc4-detection-y1 detection))))))))))
     (let ((update (not (track-annotation-good? annotation))))
      (set-track-annotation-good?! annotation
				   (not (track-annotation-good? annotation)))
      (redraw)))))))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa?
		     (name "name" string-argument ""))
		    ("list" list-filename? (list-filename "filename" string-argument ""))))
 (if list-filename?
     (run (map string->darpa-video (read-file list-filename)))
     (let* ((video-name
	     (cond (standard?
		    (standard-corpus-video corpus sequence person location n))
		   (darpa? (string->darpa-video name))
		   (else (fuck-up)))))
      (run (list video-name)))))
