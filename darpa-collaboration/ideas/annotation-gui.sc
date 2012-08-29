(MODULE
  ANNOTATION-GUI
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
(include "annotation-gui.sch")

(set! *program* "annotation-gui")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

(define *video-name* #f)
(define *frame-number* #f)
(define *image* #f)
(define *image-pixmap* #f)
(define *image-pgm* #f)
(define *image-berkeley* #f)
(define *annotation* '())
(define *use-grid?* #t)
(define *grid* #f)
(define *annotation-image* #f)
(define *annotation-image-scaled* #f)
(define *annotation-pixmap* #f)
(define *number-of-frames* #f)
(define *brush?* #f)
(define *brush-radius* 1)
(define *mode* 'person1)
(define *predict?* #f)
(define *video-length* #f)
(define *time-stamp* #f)
(define *resize?* #t)
(define *auto-flip?* #t)
(define *blank?* #f)
(define *blank-image* #f)

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;; (run (standard-corpus-video "" "a" "foob" "chino" "0") 4)
;; ./annotation-gui -standard "" "a" "foob" "chino" "0"
;; ./annotation-gui -darpa Approach2_A1_C1_Act1_2_Park3_MC_AFTN_47ff4ac2-c5af-11df-a99b-e80688cb869a -frame 1

(define (setup-grid!)
 (set! *grid*
       (annotations->uniform-grid *annotation*
				  (pnm-height *image-berkeley*)
				  (pnm-width *image-berkeley*)
				  10)))

(define (load-video! video-name frame)
 (set! *video-name* video-name)
 (set! *frame-number* frame)
 (set! *number-of-frames* (video-length video-name))
 (set! *video-length* (video-length video-name))
 (load-frame!)
 (load-annotation!))

(define (load-frame!)
 (set! *image* (read-pnm (ppm-pathname *video-name* *frame-number*)))
 (set! *image-berkeley* (read-pnm (berkeley-pathname *video-name* *frame-number*)))
 (set! *blank-image* (ppm-constant (pnm-width *image*) (pnm-height *image*) 255 255 255)))

(define (save!)
 (write-object-to-file
  *annotation*
  (human-annotation-pathname *video-name* *frame-number*)))

(define (load-annotation!)
 (if (file-exists? (human-annotation-pathname *video-name* *frame-number*))
     (set! *annotation*
	   (read-object-from-file
	    (human-annotation-pathname *video-name* *frame-number*)))
     (set! *annotation* (frame->empty-annotations *video-name* *frame-number*)))
 (when *use-grid?* (setup-grid!)))

(define (reset!)
 (set! *annotation* (frame->empty-annotations *video-name* *frame-number*))
 (when *use-grid?* (setup-grid!)))

(define (frame->empty-annotations video-name n)
 (map (lambda (p) (make-annotation p 'none))
      (if (file-exists? (berkeley-chains-pathname video-name n))
	  (read-object-from-file (berkeley-chains-pathname video-name n))
	  (join (map (lambda (c) (break-chain c 20))
		     (pbm->chains (pgm->pbm *image-berkeley* 1)))))))

;; TODO ANDREI skip over bad frames
;; todo last frame
(define (next-frame!)
 (when (= (+ *frame-number* 1) *number-of-frames*) (message "Last frame") (abort))
 (set! *frame-number* (+ 1 *frame-number*))
 (load-frame!)
 (if (and *predict?*
	  (not (file-exists? (human-annotation-pathname *video-name* *frame-number*)))
	  (> (length *annotation*) 0))
     (begin
      (set! *annotation*
	    (frame-closest-annotation *video-name* *frame-number* *annotation*))
      (when *use-grid?* (setup-grid!)))
     (load-annotation!)))

;; TODO ANDREI skip over bad frames
(define (previous-frame!)
 (when (= *frame-number* 0) (message "First frame") (abort))
 (set! *frame-number* (- *frame-number* 1))
 (load-frame!)
 (load-annotation!))

(define (annotate-blank-ppm annotations blank-ppm)
 (for-each
  (lambda (c)
   (unless (equal? (annotation-label c) 'none)
    (let ((colour (annotation->colour c)))
     (for-each (lambda (p) (set-ppm-pixel! blank-ppm (x p) (y p) colour))
	       (annotation-pixels c)))))
  annotations)
 blank-ppm)

(define (update-pixmaps!)
 (set! *image-pixmap* (pnm->pixmap *image*))
 (set! *annotation-image*
       (if *blank?*
	   (annotate-blank-ppm *annotation* *blank-image*)
	   (annotation-on-ppm *annotation* *image*)))
 (when *resize?*
  (set! *annotation-image-scaled* (scale-ppm *annotation-image* 2)))
 (when *annotation-pixmap* (free-pixmap *annotation-pixmap*))
 (set! *annotation-pixmap* (pnm->pixmap (if *resize?*
					    *annotation-image-scaled*
					    *annotation-image*))))

(define (recache-strings!) #f)

(define (next-button!)
 (message "")
 (next-frame!)
 (when *predict?* (set! *mode* 'smart))
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons)
 (message (format #f "Next frame, ~a ~a% ~as" *frame-number*
		  (number->string-of-length-and-precision
		   (* (/ *frame-number* *video-length*) 100)
		   6
		   2)
		  (let* ((t (current-time))
			 (diff (- t *time-stamp*)))
		   (set! *time-stamp* t)
		   (number->string-of-length-and-precision diff 8 4)))))

(define (next-and-save-button!)
 (message "")
 (save!)
 (next-frame!)
 (when *predict?* (set! *mode* 'smart))
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons)
 (message (format #f "Next frame ~a, ~a% ~as" *frame-number*
		  (number->string-of-length-and-precision
		   (* (/ *frame-number* *video-length*) 100)
		   6
		   2)
		  (let* ((t (current-time))
			 (diff (- t *time-stamp*)))
		   (set! *time-stamp* t)
		   (number->string-of-length-and-precision diff 8 4)))))

(define (previous-button!)
 (message "")
 (previous-frame!)
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons)
 (message (format #f "Previous frame, ~a ~a%" *frame-number*
		  (number->string-of-length-and-precision
		   (* (/ *frame-number* *video-length*) 100)
		   6
		   2))))

(define (save-button!)
 (message "")
 (save!)
 (update-pixmaps!)
 (message (format #f "Saved")))

(define (load-button!)
 (message "")
 (load-annotation!)
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons)
 (message (format #f "Loaded")))

(define (reset-button!)
 (message "")
 (reset!)
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons))

(define-application viewer (if *resize?* 1300 735) (if *resize?* 700 390) 0 1 8
 (lambda ()
  ;; FIXME Andrei This crashed on Jarell's machine
  ;; (setup-extra-x-gcs)
  (recache-strings!)
  (update-pixmaps!)
  (standard-buttons 8 (lambda () #f))
  (define-button 1 0 "Reset" #f reset-button!)
  (define-button 2 0 (lambda () (format #f "~a" *frame-number*)) #f (lambda () #f))
  (define-button 3 0 (lambda () (format #f "~a" *video-length*)) #f (lambda () #f))
  (define-button 7 12 "Save" #f save-button!)
  (define-button 7 15 "Load" #f load-button!)
  (define-toggle-button 7 3 "Brush?" *brush?* (lambda () (message "")))
  (define-spinner-buttons 4 0 "Radius"
   (lambda () (set! *brush-radius* (* *brush-radius* 2)))
   (lambda () (set! *brush-radius* (/ *brush-radius* 2)))
   (lambda () (number->string *brush-radius*)))
  (define-toggle-button 7 4 "Auto flip?" *auto-flip?* (lambda () (message "")))
  (define-toggle-button 7 5 "Predict?" *predict?* (lambda () (message "")))
  (define-toggle-button 7 6 "Blank?" *blank?*
   (lambda ()
    (message "")
    (update-pixmaps!)
    (redraw-display-pane)
    (redraw-buttons)))
  (define-button 7 8 "Previous" #f previous-button!)
  (define-button 7 9 "Next" #f next-button!)
  (define-button 7 11 "Next&Save" #f next-and-save-button!)
  (define-radio-buttons *mode* (lambda () #f)
   (0 (if *resize?* 30 16) object1 "Object1")
   (1 (if *resize?* 30 16) object2 "Object2")
   (2 (if *resize?* 30 16) object3 "Object3")
   (3 (if *resize?* 30 16) object4 "Object4")
   (4 (if *resize?* 30 16) object5 "Object5")
   (5 (if *resize?* 30 16) object6 "Object6")
   (6 (if *resize?* 30 16) object7 "Object7")
   (0 (if *resize?* 31 17) person1 "Person1")
   (1 (if *resize?* 31 17) arm1 "Arm1")
   (2 (if *resize?* 31 17) person2 "Person2")
   (3 (if *resize?* 31 17) arm2 "Arm2")
   (4 (if *resize?* 31 17) smart "Smart")
   (5 (if *resize?* 31 17) none "None"))
  (define-key (control #\s) "Next & Save" next-and-save-button!)
  (define-key (meta #\s) "Save" save-button!)
  (define-key (control #\p) "Previous" previous-button!)
  (define-key (control #\n) "Next" next-button!)

  (define-key #\1 "None" (lambda () (set! *mode* 'none) (redraw-buttons)))
  (define-key #\2 "Smart" (lambda () (set! *mode* 'smart) (redraw-buttons)))
  (define-key #\3 "Person1"  (lambda () (set! *mode* 'person1) (redraw-buttons)))
  (define-key #\4 "Arm1" (lambda () (set! *mode* 'arm1) (redraw-buttons)))
  (define-key #\5 "Arm2" (lambda () (set! *mode* 'arm2) (redraw-buttons)))
  (define-key #\6 "Person2" (lambda () (set! *mode* 'person2) (redraw-buttons)))
  (define-key #\q "Object1"  (lambda () (set! *mode* 'object1) (redraw-buttons)))
  (define-key #\w "Object2"  (lambda () (set! *mode* 'object2) (redraw-buttons)))
  (define-key #\e "Object3"  (lambda () (set! *mode* 'object3) (redraw-buttons)))
  (define-key #\r "Object4"  (lambda () (set! *mode* 'object4) (redraw-buttons)))
  (define-key #\t "Object5"  (lambda () (set! *mode* 'object5) (redraw-buttons)))
  (define-key #\y "Object6"  (lambda () (set! *mode* 'object6) (redraw-buttons)))
  (define-key #\u "Object7"  (lambda () (set! *mode* 'object7) (redraw-buttons)))
  )
 (lambda () #f)							;post
 (lambda () #f
    ;; FIXME This is tied to (setup-extra-x-gcs)
    ;; (free-extra-x-gcs)
    )								;fin
 (lambda ()
  (xremove-expose-events)
  (when #f (draw-pixmap *image-pixmap* (pnm-width *image*) 0))
  (draw-clickable-pixmap-from-pnm
   *annotation-pixmap* *annotation-image*
   0 0 (if *resize?* 2 1)
   (lambda (x1 y1)
    (if *brush?*
	(for-each
	 (lambda (a) (update-annotation-within-distance a `#(,x1 ,y1) *mode* *brush-radius*))
	 *annotation*)
	(let ((closest (closest-annotation
			(if *use-grid?*
			    (lookup-9-closest-cells *grid* `#(,x1 ,y1))
			    *annotation*)
			`#(,x1 ,y1))))
	 (if (equal? *mode* 'smart)
	     (let ((next-closest (closest-annotation
				  (remove-if
				   (lambda (a) (or (equal? (annotation-pixels a) closest)
					      (equal? (annotation-label a) 'none)))
				   (if *use-grid?*
				       (lookup-9-closest-cells *grid* `#(,x1 ,y1))
				       *annotation*))
				  `#(,x1 ,y1))))
	      (update-annotation-within-distance closest `#(,x1 ,y1)
						 (annotation-label next-closest) 5))
	     (update-annotation-within-distance closest `#(,x1 ,y1) *mode* 5))))
    (redraw-display-pane)))
  #f)									;re
 (lambda () (redraw-display-pane) (redraw-buttons) #f))

(define (update-annotation-within-distance annotation point mode distance)
 (when (< (annotation-point-distance annotation point) distance)
  (set-annotation-label! annotation
			 (if (and *auto-flip?* (equal? (annotation-label annotation) mode))
			     'none mode))
  (annotate-image! (list annotation) *annotation-image*)
  (when *resize?*
   (annotate-image-scaled! (list annotation) *annotation-image-scaled*))
  (when *annotation-pixmap* (free-pixmap *annotation-pixmap*))
  (if *resize?*
      (set! *annotation-pixmap* (pnm->pixmap *annotation-image-scaled*))
      (set! *annotation-pixmap* (pnm->pixmap *annotation-image*)))))

(define (run video-name frame)
 (load-video! video-name frame)
 (set! *time-stamp* (current-time))
 (viewer '()))

(define (frame-closest-annotation video-name n annotations)
 (let* ((new-annotations (frame->empty-annotations video-name n))
	(matrix (make-matrix (pnm-height *image-berkeley*)
			     (pnm-width *image-berkeley*)
			     #f))
	(columns (matrix-columns matrix))
	(rows (matrix-rows matrix)))
  (for-each
   (lambda (a)
    (unless (equal? (annotation-label a) 'none)
     (let ((entry (list (length (annotation-pixels a)) (annotation-label a))))
      (for-each (lambda (p)
		 (matrix-set! matrix (y p) (x p) entry))
		(annotation-pixels a)))))
   annotations)
  (for-each-indexed
   (lambda (a i)
    (let ((p (quantize-point (centroid (annotation-pixels a))))
	  (l (length (annotation-pixels a))))
     (unless (= l 1)
      (for-each-pixel-in-rectangle
       (x p) (y p) 4 4
       (lambda (x y)
	(when (and (>= x 0) (< x columns)
		   (>= y 0) (< y rows))
	 (let ((entry (matrix-ref matrix y x)))
	  (when (and entry
		     (between? l (- (first entry) 2) (+ (first entry) 10)))
	   (set-annotation-label! a (second entry))))))))))
   new-annotations)
  new-annotations))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("frame" frame? (frame "n" integer-argument 0)))
       (at-most-one ("resize" resize?)))
 (set! *resize?* resize?) 
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up)))))
  (run video-name frame)))
