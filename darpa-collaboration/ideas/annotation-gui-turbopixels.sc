(MODULE
  ANNOTATION-GUI-TURBOPIXELS
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
(include "annotation-gui-turbopixels.sch")

(set! *program* "annotation-gui-turbopixels")
(set! *panic?* #f)

;;; Macros

;;; Structures

(define-structure turbopixel-annotation label turbopixel)

;;; Variables

;;(define (c-getpid) 3)
;;(define (c-srand n) #f)
;; (main '(annotation-gui-turbopixels "-darpa" "Approach2_A1_C1_Act1_2_Park3_MC_AFTN_47ff4ac2-c5af-11df-a99b-e80688cb869a" "-frame" "1"))

(define *video-name* #f)
(define *frame-number* #f)
(define *image* #f)
(define *image-pixmap* #f)
(define *annotation* '())
(define *annotation-mapping* #f)
(define *annotation-image* #f)
(define *annotation-image-scaled* #f)
(define *annotation-pixmap* #f)
(define *number-of-frames* #f)
(define *mode* 'person1)
(define *predict?* #f)
(define *video-length* #f)
(define *time-stamp* #f)
(define *resize?* #t)
(define *auto-flip?* #t)
(define *blank?* #f)
(define *blank-image* #f)
(define *turbopixels-names* #f)
(define *turbopixels-speed* #f)
(define *turbopixels-speed-reliable?* #f)
(define *turbopixels-centers* #f)
(define *turbopixels-map* #f)
(define *turbopixels* #f)

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;; (run (standard-corpus-video "" "a" "foob" "chino" "0") 4)
;; ./annotation-gui-turbopixels -standard "" "a" "foob" "chino" "0"
;; ./annotation-gui-turbopixels -darpa Approach2_A1_C1_Act1_2_Park3_MC_AFTN_47ff4ac2-c5af-11df-a99b-e80688cb869a -frame 1

;;; FIXME ANDREI This hardcodes the first frame of the video to 1!

(define (resize-image w h i)
 (let ((name (format #f "/tmp/resize-~a.ppm" (getpid))))
  (write-pnm i name)
  (system (format #f "mogrify -resize ~ax~a! ~a &> /dev/null" w h name))
  (let ((r (read-pnm name)))
   (rm name)
   r)))

(define (load-video! video-name frame)
 (set! *video-name* video-name)
 (set! *frame-number* frame)
 (set! *number-of-frames* (video-length video-name))
 (set! *video-length* (video-length video-name))
 (turbopixels-load *video-name*)
 (set! *turbopixels-names* (turbopixels-names))
 (set! *turbopixels-speed* (turbopixels-speed))
 (set! *turbopixels-speed-reliable?* (turbopixels-speed-reliable?))
 (set! *turbopixels-centers* (turbopixels-centers))
 (load-frame!)
 (load-annotation!))

(define (load-frame!)
 (set! *turbopixels-map* (turbopixels-for-frame *frame-number*))
 (set! *turbopixels* (external-turbopixels->turbopixels
		      *turbopixels-map*
		      *frame-number*
		      *turbopixels-names* *turbopixels-centers*
		      *turbopixels-speed* *turbopixels-speed-reliable?*))
 (set! *image* (read-pnm (ppm-pathname *video-name* *frame-number*)))
 (set! *blank-image* (ppm-constant (pnm-width *image*) (pnm-height *image*) 255 255 255)))

(define (save!)
 (write-object-to-file
  *annotation*
  (human-annotation-turbopixel-pathname *video-name* *frame-number*)))

(define (load-annotation!)
 (if (file-exists? (human-annotation-turbopixel-pathname *video-name* *frame-number*))
     (set! *annotation*
	   (read-object-from-file
	    (human-annotation-turbopixel-pathname *video-name* *frame-number*)))
     (set! *annotation* (frame->empty-annotations *video-name* *frame-number*))))

(define (reset!)
 (set! *annotation* (frame->empty-annotations *video-name* *frame-number*)))

(define (frame->empty-annotations video-name n)
 (map (lambda (t) (make-turbopixel-annotation 'none t))
      *turbopixels*))

;; TODO ANDREI skip over bad frames
(define (next-frame!)
 (when (= (+ *frame-number* 1) *number-of-frames*) (message "Last frame") (abort))
 (set! *frame-number* (+ 1 *frame-number*))
 (load-frame!)
 (if (and *predict?*
	  (not (file-exists? (human-annotation-turbopixel-pathname *video-name* *frame-number*)))
	  (> (length *annotation*) 0))
     (begin
      (set! *annotation*
	    (frame-closest-annotation *video-name* *frame-number* *annotation*)))
     (load-annotation!)))

;; TODO ANDREI skip over bad frames
(define (previous-frame!)
 (when (= *frame-number* 0) (message "First frame") (abort))
 (set! *frame-number* (- *frame-number* 1))
 (load-frame!)
 (load-annotation!))

(define (turbopixel-annotate-blank-ppm annotations blank-ppm)
 (for-each
  (lambda (c)
   (unless (equal? (turbopixel-annotation-label c) 'none)
    (let ((colour (turbopixel-anotation->colour c)))
     (for-each (lambda (p) (set-ppm-pixel! blank-ppm (x p) (y p) colour))
	       (turbopixel-annotation->pixels c)))))
  annotations)
 blank-ppm)

(define (turbopixel-annotation-on-ppm annotations image)
 ;; FIXME ANDREI Intentionally discarding the input image
 (for-each
  (lambda (c)
   (let ((colour (turbopixel-anotation->colour c)))
    (for-each (lambda (p) (turbopixel-merge-colour p c colour image))
	      (turbopixel-annotation->pixels c))))
  annotations)
 image)

(define (update-pixmaps!)
 (set! *image-pixmap* (pnm->pixmap *image*))
 (set! *annotation-image*
       (if *blank?*
	   (turbopixel-annotate-blank-ppm *annotation* *blank-image*)
	   (turbopixel-annotation-on-ppm *annotation* *image*)))
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
   (0 (if *resize?* 31 17) person1 "Person1")
   (1 (if *resize?* 31 17) arm1 "Arm1")
   (2 (if *resize?* 31 17) arm2 "Arm2")
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
    (when (turbopixel-point->annotation `#(,x1 ,y1))
     (turbopixel-update-annotation (turbopixel-point->annotation `#(,x1 ,y1)) *mode*))
    (redraw-display-pane)))
  #f)									;re
 (lambda () (redraw-display-pane) (redraw-buttons) #f))

(define (turbopixel-point->annotation p)
 (let ((t (image-ref *turbopixels-map* p)))
  (find-if (lambda (a) (= t (turbopixel-name (turbopixel-annotation-turbopixel a))))
	   *annotation*)))

(define (turbopixel-annotation->pixels a)
 (turbopixel-pixels (turbopixel-annotation-turbopixel a)))

(define (turbopixel-anotation->colour a)
 (case (turbopixel-annotation-label a)
  ((none) '#(255 0 0))
  ((person1) '#(0 255 0))
  ((person2) '#(0 0 255))
  (else (fuck-up))))

(define (turbopixel-merge-colour p c colour image)
 (set-ppm-pixel!
  image (x p) (y p)
  (if (equal? (turbopixel-annotation-label c) 'none)
      (image-ref *image* p)
      (map-vector
       (lambda (a b) (min (* a (max (/ b 60) 1)) 255))
       (image-ref *image* p)
       colour))))

(define (turbopixel-annotate-image! annotations image)
 (for-each
  (lambda (c)
   (let ((colour (turbopixel-anotation->colour c)))
    (for-each (lambda (p) (turbopixel-merge-colour p c colour image))
	      (turbopixel-annotation->pixels c))))
  annotations)
 image)

(define (turbopixel-annotate-image-scaled! annotations image)
 (for-each
  (lambda (c)
   (let ((colour (turbopixel-anotation->colour c)))
    (for-each (lambda (p)
	       (let ((x (* (x p) 2)) (y (* (y p) 2)))
		(set-ppm-pixel! image x y colour)
		(set-ppm-pixel! image (+ x 1) y colour)
		(set-ppm-pixel! image x (+ y 1) colour)
		(set-ppm-pixel! image (+ x 1) (+ y 1) colour)))
	      (turbopixel-annotation->pixels c))))
  annotations)
 image)

(define (turbopixel-update-annotation annotation mode)
 (set-turbopixel-annotation-label!
  annotation
  (if (and *auto-flip?*
	   (equal? (turbopixel-annotation-label annotation) mode))
      'none mode))
 (turbopixel-annotate-image! (list annotation) *annotation-image*)
 (when *resize?*
  (turbopixel-annotate-image-scaled! (list annotation) *annotation-image-scaled*))
 (when *annotation-pixmap* (free-pixmap *annotation-pixmap*))
 (if *resize?*
     (set! *annotation-pixmap* (pnm->pixmap *annotation-image-scaled*))
     (set! *annotation-pixmap* (pnm->pixmap *annotation-image*))))

(define (run video-name frame)
 (load-video! video-name frame)
 (set! *time-stamp* (current-time))
 (viewer '()))

(define (frame-closest-annotation video-name n annotations)
 (let ((new-annotations (frame->empty-annotations video-name n)))
  (update-turbopixel-tracks! (map turbopixel-annotation-turbopixel new-annotations)
			     *turbopixels-map*
			     (map turbopixel-annotation-turbopixel annotations))
  (update-turbopixel-tracks! *turbopixels*
			     *turbopixels-map*
			     (map turbopixel-annotation-turbopixel annotations))
  (for-each (lambda (n-a)
	     (let* ((track (turbopixel-track (turbopixel-annotation-turbopixel n-a)))
		    (old-annotation
		     (find-if (lambda (a)
			       (= track (turbopixel-track (turbopixel-annotation-turbopixel a))))
			      annotations)))
	      (when old-annotation
	       (set-turbopixel-annotation-label! n-a (turbopixel-annotation-label old-annotation)))))
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
