(MODULE
  ANNOTATION-GUI-ESSA
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
(include "annotation-gui-essa.sch")

(set! *program* "annotation-gui-essa")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

(define *video-name* #f)
(define *frame-number* #f)
(define *image* #f)
(define *image-annotated* #f)
(define *image-pixmap* #f)
(define *image-pgm* #f)
(define *region-map* '())
(define *image-essa* #f)
(define *annotation* '())
(define *essa-annotation* '())
(define *essa* '())
(define *essa-first* '())
(define *essa-level* 1)
(define *use-grid?* #t)
(define *grid* #f)
(define *annotation-image* #f)
(define *annotation-image-scaled* #f)
(define *annotation-pixmap* #f)
(define *number-of-frames* #f)
(define *mode* 'person1)
(define *predict?* #f)
(define *video-length* #f)
(define *time-stamp* #f)
(define *num-clicks* #f)
(define *resize?* #t)
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


; File I/O

(define (load-video! video-name frame)
 (set! *video-name* video-name)
 (set! *frame-number* frame)
 (set! *number-of-frames* (video-length video-name))
 (set! *video-length* (video-length video-name))
 (load-frame!)
 (load-annotation!))

(define (load-frame!)
 (set! *image* (read-pnm (ppm-pathname *video-name* *frame-number*)))
  (if (file-exists? (essa-pathname *video-name* *frame-number*)) 
	(set! *essa* (read-object-from-file (essa-pathname *video-name* *frame-number*)))
	(panic "Essa segmentation not found!"))
  ; Graft the hierarchy onto the new essa file
  (if (eqv? (D-Hs (F-Ds *essa*)) '#() ) 
	(set-D-Hs! (F-Ds *essa*) (D-Hs (F-Ds (read-object-from-file (essa-pathname *video-name* 1))))))
   (set! *region-map* (essa->region-map *essa* *essa-level*))
   (set! *blank-image* (ppm-constant (essa-width *essa*) (essa-height *essa*) 255 255 255))
   (set! *num-clicks* 0)
  )

(define (save!)
 (write-object-to-file
  *essa-annotation*
  (human-annotation-essa-pathname *video-name* *frame-number* *essa-level*)))

(define (load-annotation!)
  (if (file-exists? (human-annotation-essa-pathname *video-name* *frame-number* *essa-level*))
	(set! *essa-annotation*
	  (read-object-from-file
		(human-annotation-essa-pathname *video-name* *frame-number* *essa-level*)))
	(set! *essa-annotation* (frame->empty-annotations *video-name* *frame-number*))))

(define (reset!)
 (set! *essa-annotation* (frame->empty-annotations *video-name* *frame-number*)))

; Stolen from essa->region-pixels
; Modify so it doesn't stupidly construct redundant pixel lists
(define (essa->unique-regions f level)
(map
  (lambda (c) (first (first c)) )
  (equivalence-classesp
   (lambda (a b) (= (first a) (first b)))
   (vector->list
    (map-vector (lambda (v)
		 (list
		  (x v)
		  (scalines->pixels (r-top-y (y v))
				    (r-scanline (y v)))))
		(essa-frame-hierarchy->slice *essa* *essa-level*))))))

; Init a new, empty annotation
(define (frame->empty-annotations video-name n)
  (map (lambda (a) (make-essa-annotation 'none *essa-level* a))
   (essa->unique-regions *essa* *essa-level*)))

; Frame navigation

(define (next-frame!)
 (when (= (+ *frame-number* 1) *number-of-frames*) (message "Last frame") (abort))
 (set! *frame-number* (+ 1 *frame-number*))
 (load-frame!)
 (if (and *predict?*
	  (not (file-exists? (human-annotation-essa-pathname *video-name* *frame-number* *essa-level*)))
	  (> (length *essa-annotation*) 0))
     (begin
	  (let* ((essa-annotation-old *essa-annotation*))
	   (set! *essa-annotation* (frame->empty-annotations *video-name* *frame-number*))  
	   (for-each
	    (lambda (r) 
		 (if (not (equal? (annotation-region->annotation-label essa-annotation-old r *essa-level*) #f))
		  (update-annotation *essa-annotation* r (annotation-region->annotation-label essa-annotation-old r *essa-level*))))
		(essa->unique-regions *essa* *essa-level*))
       ))
     (load-annotation!))
; (load-annotation!)
 )
 ;Load and/or predict annotations

(define (previous-frame!)
 (when (= *frame-number* 0) (message "First frame") (abort))
 (set! *frame-number* (- *frame-number* 1))
 (load-frame!)
 (load-annotation!))

(define (update-pixmaps!)
; (set! *image-pixmap* (pnm->pixmap *image*))
 (set! *image-essa* (essa-rasterized->pgm *essa* *essa-level*))
 (set! *image-annotated* *image*)
 (essa-annotate-image! *essa-annotation* *image-annotated*)
 ; Display annotation image
 (set! *image-pixmap* (pnm->pixmap *image-annotated*))
)

(define (recache-strings!) #f)

(define (human-annotation-time-pathname video-name frame level)
 (generic-pathname video-name frame "human-annotation.time" ))
(define (human-annotation-clicks-pathname video-name frame level)
 (generic-pathname video-name frame "human-annotation.clicks" ))
; Callbacks

(define (next-button!)
 (let* 
  ((t (current-time))
  (diff (- t *time-stamp*)))
 (set! *time-stamp* t)
; (call-with-output-file (human-annotation-time-pathname *video-name* *frame-number* *essa-level*) 
;  (lambda (port)
;   (format port (number->string diff))))
;(call-with-output-file (human-annotation-clicks-pathname *video-name* *frame-number* *essa-level*) 
;  (lambda (port)
;   (format port (number->string *num-clicks*))))
 (message "")
 (next-frame!)
 (when *predict?* (set! *mode* 'none))
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons)
 (message (format #f "Next frame, ~a ~a% ~as" *frame-number*
		  (number->string-of-length-and-precision
		   (* (/ *frame-number* *video-length*) 100)
		   6
		   2)
		  (number->string-of-length-and-precision diff 8 4)))))

(define (next-and-save-button!)
 (let* 
  ((t (current-time))
  (diff (- t *time-stamp*)))
 (set! *time-stamp* t)
 (call-with-output-file (human-annotation-time-pathname *video-name* *frame-number* *essa-level*) 
  (lambda (port)
   (format port (number->string diff))))
(call-with-output-file (human-annotation-clicks-pathname *video-name* *frame-number* *essa-level*) 
  (lambda (port)
   (format port (number->string *num-clicks*))))
 (message "")
 (save!)
 (next-frame!)
 (when *predict?* (set! *mode* 'none))
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons)
 (message (format #f "Next frame ~a, ~a% ~as" *frame-number*
		  (number->string-of-length-and-precision
		   (* (/ *frame-number* *video-length*) 100)
		   6
		   2)
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

; Main window function
(define-application viewer (+ (essa-width *essa*) 77) (+ (essa-height *essa*) 80)   0 1 8
 (lambda () ; Pre-initialize procedure
  (recache-strings!)
  (update-pixmaps!)
  (standard-buttons 8 (lambda () #f))
  (define-button 1 0 "Reset" #f reset-button!)
  (define-button 2 0 (lambda () (format #f "~a" *frame-number*)) #f (lambda () #f))
  (define-button 3 0 (lambda () (format #f "~a" *video-length*)) #f (lambda () #f))
  (define-button 7 9 "Save" #f save-button!)
  (define-button 7 10 "Load" #f load-button!)
  (define-toggle-button 7 2 "Predict?" *predict?* (lambda () (message "")))
  (define-toggle-button 7 3 "Blank?" *blank?*
   (lambda ()
    (message "")
    (update-pixmaps!)
    (redraw-display-pane)
    (redraw-buttons)))
  (define-button 7 5 "Previous" #f previous-button!)
  (define-button 7 6 "Next" #f next-button!)
  (define-button 7 7 "Next&Save" #f next-and-save-button!)
  (define-radio-buttons *mode* (lambda () #f)
   (0 14  none "None")
   (1 14  ball "Ball")
   (2 14  chair "Chair")
   (3 14  gun "Gun")
   (4 14  vehicle "Vehicle")
   (5 14  shovel "Shovel")
;   (6 (if *resize?* 30 14) object6 "Object6")
;   (7 (if *resize?* 30 14) object7 "Object7")
   (0 15 person1 "Person1")
   (1 15 p1arm1 "P1Arm1")
   (2 15 p1arm2 "P1Arm2")
   (3 15 p1leg1 "P1Leg1")
   (4 15 p1leg2 "P1Leg2")
   (0 16 person2 "Person2")
   (1 16 p2arm1 "P2Arm1")
   (2 16 p2arm2 "P2Arm2")
   (3 16 p2leg1 "P2Leg1")
   (4 16 p2leg2 "P2Leg2"))
  (define-key (control #\s) "Next & Save" next-and-save-button!)
  (define-key (meta #\s) "Save" save-button!)
  (define-key (control #\p) "Previous" previous-button!)
  (define-key (control #\n) "Next" next-button!)
;  (define-key #\1 "None" (lambda () (set! *mode* 'none) (redraw-buttons)))
;  (define-key #\2 "Person1"  (lambda () (set! *mode* 'person1) (redraw-buttons)))
;  (define-key #\3 "Arm1" (lambda () (set! *mode* 'arm1) (redraw-buttons)))
;  (define-key #\4 "Arm2" (lambda () (set! *mode* 'arm2) (redraw-buttons)))
;  (define-key #\5 "Person2" (lambda () (set! *mode* 'person2) (redraw-buttons)))
;  (define-key #\q "Object1"  (lambda () (set! *mode* 'object1) (redraw-buttons)))
;  (define-key #\w "Object2"  (lambda () (set! *mode* 'object2) (redraw-buttons)))
;  (define-key #\e "Object3"  (lambda () (set! *mode* 'object3) (redraw-buttons)))
;  (define-key #\r "Object4"  (lambda () (set! *mode* 'object4) (redraw-buttons)))
;  (define-key #\t "Object5"  (lambda () (set! *mode* 'object5) (redraw-buttons)))
;  (define-key #\y "Object6"  (lambda () (set! *mode* 'object6) (redraw-buttons)))
;  (define-key #\u "Object7"  (lambda () (set! *mode* 'object7) (redraw-buttons)))
  )
 (lambda () #f) ; Post-initialize procedure
 (lambda () #f) ; Finalize procedure
 (lambda () ; Redraw procedure
  (xremove-expose-events)
  (when #f (draw-pixmap *image-pixmap* (pnm-width *image*) 0))
  (draw-clickable-pixmap-from-pnm *image-pixmap* *image-annotated* 0 0 1
   (lambda (x1 y1)
	 (message (format #f "Clicked ~ax~a: ~a" x1 y1 (matrix-ref *region-map* y1 x1) ))
	 (set! *essa-annotation* (update-annotation *essa-annotation* (matrix-ref *region-map* y1 x1) *mode*))
	 (set! *image-annotated* *image*)
	 (essa-annotate-image! *essa-annotation* *image-annotated*)
	 (set! *image-pixmap* (pnm->pixmap *image-annotated*))
	 (set! *num-clicks* (+ *num-clicks* 1))
	 (redraw-display-pane))) #f)
 ; Listener procedure
 (lambda () (redraw-display-pane) (redraw-buttons) #f)) ; viewer end

;; Annotation related functions

(define (update-annotation-within-distance annotation point mode distance) (panic "Unimplemented") )
; (when (< (annotation-point-distance annotation point) distance)
;  (set-annotation-label! annotation
;			 (if (and *auto-flip?* (equal? (annotation-label annotation) mode))
;			     'none mode))
;  (annotate-image! (list annotation) *annotation-image*)
;  (when *resize?*
;   (annotate-image-scaled! (list annotation) *annotation-image-scaled*))
;  (when *annotation-pixmap* (free-pixmap *annotation-pixmap*))
;  (if *resize?*
;      (set! *annotation-pixmap* (pnm->pixmap *annotation-image-scaled*))
;      (set! *annotation-pixmap* (pnm->pixmap *annotation-image*)))))

; Change annotated label
(define (update-annotation annotation region label) 
 (map 
  (lambda(a)
   (if (= (essa-annotation-region a) region)
    (set-essa-annotation-label! a label)) a)
  annotation))

(define (annotate-blank-ppm annotations blank-ppm)
 (for-each
  (lambda (c)
   (unless (equal? (annotation-label c) 'none)
    (let ((colour (annotation->colour c)))
     (for-each (lambda (p) (set-ppm-pixel! blank-ppm (x p) (y p) colour))
	       (annotation-pixels c)))))
  annotations)
 blank-ppm)

(define (essa-annotate-image! annotations image) 
(if (not *blank?*)
 (for-each 
  (lambda (j)  
   (for-each 
    (lambda (i) 
     (set-ppm-pixel! image (x i) (y i) '#(0 0 0)))
	   (list-ref j 1)))
   (essa->region-outline *essa* *essa-level*)))
(for-each 
  (lambda (r)
   (if (not (equal? (annotation-region->annotation-label annotations (first r) *essa-level*) 'none))
   (let ((colour (label->colour (annotation-region->annotation-label annotations (first r) *essa-level* )))) 
    (for-each
     (lambda (p)
	  (set-ppm-pixel! image  (x p) (y p) colour))
     (second r)))))
  (essa->region-outline *essa* *essa-level*))
 image)

; Launcher for the viewing frame
(define (run video-name frame)
 (load-video! video-name frame)
 (set! *time-stamp* (current-time))
 (viewer '()))

; Main function
(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("frame" frame? (frame "n" integer-argument 0)))
       (at-most-one ("level" level? (level "n" integer-argument 0)))
       (at-most-one ("resize" resize?)))
 (set! *resize?* resize?) 
 (set! *essa-level* level) 
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up)))))
  (run video-name frame)))
