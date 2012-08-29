(MODULE
 ANNOTATION-GUI-SUPERPIXELS
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
(include "annotation-gui-superpixels.sch")

(set! *program* "annotation-gui-superpixels")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;; (define (c-getpid) 3)
;; (define (c-srand n) #f)
;; (main '(annotation-gui-superpixels "-darpa" "Approach2_A1_C1_Act1_2_Park3_MC_AFTN_47ff4ac2-c5af-11df-a99b-e80688cb869a" "-frame" "1"))
;; darpa-wrap  ./annotation-gui-superpixels -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a -frame 2 -slic
;; darpa-wrap  ./annotation-gui-superpixels -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a -frame 2 -turbopixels

(define *imlib-image* #f)
(define *video-name* #f)
(define *frame-number* #f)
(define *annotation* '())
(define *annotation-mapping* #f)
(define *annotation-image* #f)
(define *annotation-image-scaled* #f)
(define *essa* #f)
(define *essa-next* #f)
(define *number-of-frames* #f)
(define *mode* 'person1)
(define *predict?* #t)
(define *video-length* #f)
(define *time-stamp* #f)
(define *resize?* #t)
(define *auto-flip?* #t)
(define *blank?* #f)
(define *blank-image* #f)
(define *superpixel-map* #f)
(define *superpixels* #f)
(define *superpixel-type* 'slic)
(define *superpixel-adjacency* #f)
(define *number-of-clicks* 0)
(define *width* #f)
(define *height* #f)
(define *cached* #t) ;; TODO #f is not supported at the moment
(define *foreground-segmentation* #f)

(define (*large-gui?*) (or *resize?* (> *width* 650)))

(define *time-depth* 0)

(define (time-thunk format-string thunk)
 (let* ((start (current-time))
	(result (thunk))
	(end (current-time)))
  (format #t format-string
	  (number->string-of-length-and-precision (- end start) 8 2))
  result))

(define-macro time
 (lambda (form expander)
  (expander `(time-thunk ,(second form) (lambda () ,(third form))) expander)))

(define-macro time-code
 (lambda (form expander)
  (expander `(time-thunk (format #f "~a~~a : ~a~%"
				 (make-string *time-depth* #\+)
				 ,(format #f "~a" (second form)))
			 (lambda ()
			  (set! *time-depth* (+ *time-depth* 1))
			  ,(second form)
			  (set! *time-depth* (- *time-depth* 1))))
	    expander)))

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;;; FIXME ANDREI This hardcodes the first frame of the video to 1!

(define (load-video! video-name frame)
 (set! *video-name* video-name)
 (set! *frame-number* frame)
 (set! *number-of-frames* (video-length video-name))
 (set! *video-length* (video-length video-name))
 (let ((i (read-pnm (ppm-full-pathname *video-name* *frame-number*))))
  (set! *width* (pnm-width i))
  (set! *height* (pnm-height i)))
 (load-frame!)
 (load-annotation!))

(define (read-raw-ppm pathname)
 (define (for-each-n f n)
  (let loop ((i 0)) (when (< i n) (f i) (loop (+ i 1)))))
 (define (read-pnm port)
  (define (read-ppm raw?)
   (let* ((width (read port))
	  (height (read port))
	  (maxval (read port))
	  (size (* width height))
	  (red (make-matrix height width 0))
	  (green (make-matrix height width 0))
	  (blue (make-matrix height width 0)))
    (read-char port)
    (time-code (for-each-n
    		(lambda (y)
    		 (for-each-n
    		  (lambda (x)
    		   (matrix-set! red y x (char->integer (read-char port)))
    		   (matrix-set! green y x (char->integer (read-char port)))
    		   (matrix-set! blue y x (char->integer (read-char port))))
    		  width))
    		height))
    (make-ppm raw? maxval red green blue)))
  (let ((format (read port)))
   (read-char port)
   (while (char=? (peek-char port) #\#) (read-line port))
   (case format
    ((p6) (read-ppm #t))
    (else (panic "Incorrect format for a pnm image")))))
 (if (string=? pathname "-")
     (read-pnm (current-input-port))
     (call-with-input-file pathname read-pnm)))

(define (load-frame!)
 (time-code (set! *imlib-image* (imlib-load-image! (ppm-full-pathname *video-name* *frame-number*))))
 (time-code (set! *blank-image* (ppm-constant *width* *height* 255 255 255)))
 (time-code (set! *superpixels*
		  (if (file-exists?
		       (cached-superpixels-pathname *video-name* *frame-number* "frame" *superpixel-type*))
		      (read-binary-superpixels
		       (cached-superpixels-pathname *video-name* *frame-number* "frame" *superpixel-type*))
		      (read-object-from-file
		       (superpixel-pathname *video-name* *frame-number* "frame" *superpixel-type*)))))
 (let ((background (format #f "foreground-~a.pgm" (number->padded-string-of-length *frame-number* 4))))
  (time-code (set! *foreground-segmentation*
		   (pgm->pbm (read-pnm background) 128))))
 (when #f
  (time-code (set! *essa* (read-object-from-file
			   (superpixel-pathname *video-name* *frame-number* "frame" 'essa)
			   ;; (essa-pathname *video-name* *frame-number*)
			   )))
  (time-code (set! *essa-next*
		   (read-object-from-file
		    (superpixel-pathname *video-name* (+ *frame-number* 1) "frame" 'essa)
		    ;; (essa-pathname *video-name* (+ *frame-number* 1))
		    ))))
 (time-code
  (set! *superpixel-map*
	(if (file-exists?
	     (cached-superpixel-map-pathname *video-name* *frame-number* "frame" *superpixel-type*))
	    (c-read-binary-superpixel-map
	     (cached-superpixel-map-pathname *video-name* *frame-number* "frame" *superpixel-type*))
	    (superpixels->map
	     (map (lambda (s)
		   (make-superpixel (superpixel-name s)
				    #f
				    #f
				    #f
				    #f
				    (boundary->region (superpixel-pixels s))))
		  *superpixels*)
	     *width*
	     *height*))))
 (time-code
  (set! *superpixel-adjacency*
	(if (file-exists?
	     (cached-superpixel-map-pathname *video-name* *frame-number* "frame" *superpixel-type*))
	    (read-object-from-file
	     (cached-superpixel-adjacency *video-name* *frame-number* "frame" *superpixel-type*))
	    (panic "Pregenerated file not found")))))

(define (save!)
 (write-superpixel-annotations-to-file
  *annotation*
  (human-annotation-superpixel-pathname *video-name* *frame-number* *superpixel-type*)))

(define (write-superpixel-annotations-to-file annotations file)
 (write-object-to-file
  (map (lambda (a) (make-superpixel-annotation (superpixel-annotation-label a)
					  (superpixel-name (superpixel-annotation-superpixel a))))
       annotations)
  file))

(define (read-superpixel-annotations-from-file superpixels file)
 (map (lambda (a) (make-superpixel-annotation (superpixel-annotation-label a)
					 (find-if
					  (lambda (s)
					   (equal? (superpixel-name s)
						   (superpixel-annotation-superpixel a)))
					  superpixels)))
      (read-object-from-file file)))


(define (load-annotation!)
 (if (file-exists? (human-annotation-superpixel-pathname *video-name* *frame-number* *superpixel-type*))
     (set! *annotation*
	   (read-superpixel-annotations-from-file
	    *superpixels*
	    (human-annotation-superpixel-pathname *video-name* *frame-number* *superpixel-type*)))
     (set! *annotation* (frame->empty-annotations *superpixels*))))

(define (reset!)
 (set! *annotation* (frame->empty-annotations *superpixels*))
 (reset-statistics!))

;; TODO ANDREI skip over bad frames
(define (next-frame!)
 (format #t "Next frame start~%")
 (time-code
  (begin
   (time-code (when (= (+ *frame-number* 1) *number-of-frames*) (message "Last frame") (abort)))
   (set! *frame-number* (+ 1 *frame-number*))
   (time-code (load-frame!))
   (time-code (if (and *predict?*
		       (not (file-exists?
			     (human-annotation-superpixel-pathname
			      *video-name* *frame-number* *superpixel-type*)))
		       (> (length *annotation*) 0))
		  (if *essa*
		      (begin
		       (let ((new-annotations (frame->empty-annotations *superpixels*)))
			(superpixel-propagate-essa-annotations! *annotation* new-annotations *essa* *essa-next*)
			(set! *annotation* new-annotations)))
		      (set! *annotation*
			    (frame-closest-annotation *video-name* *frame-number* *annotation*)))
		  (load-annotation!)))))
 (format #t "Next frame end~%"))

;; TODO ANDREI skip over bad frames
(define (previous-frame!)
 (when (= *frame-number* 0) (message "First frame") (abort))
 (set! *frame-number* (- *frame-number* 1))
 (load-frame!)
 (load-annotation!)
 (reset-statistics!))

(define (superpixel-annotation-on-imlib annotations image)
 (imlib-context-set-image! image)
 (imlib-image-set-has-alpha! 0)
 (for-each
  (lambda (c)
   (let ((colour (superpixel-anotation->colour c)))
    (imlib-context-set-color! (r colour) (g colour) (b colour) 255)
    (for-each (lambda (p) (imlib-image-draw-pixel (x p) (y p) 0))
	      (superpixel-annotation->pixels c))))
  annotations)
 image)

(define (update-pixmaps!)
 (time-code (set! *annotation-image*
		  (if *blank?*
		      (panic "Unimplemented")
		      ;; (superpixel-annotate-blank-ppm *annotation* *blank-image*)
		      (superpixel-annotation-on-imlib *annotation* *imlib-image*))))
 (time-code (when *resize?*
	     (set! *annotation-image-scaled* (scale-ppm *annotation-image* 2)))))

(define (recache-strings!) #f)

(define (next-button!)
 (message "")
 (time-code (next-frame!))
 (time-code (update-pixmaps!))
 (time-code (redraw-display-pane))
 (time-code (redraw-buttons))
 (message (format #f "Next frame, ~a ~a% ~as" *frame-number*
				  (number->string-of-length-and-precision
				   (* (/ *frame-number* *video-length*) 100)
				   6
				   2)
				  (let* ((t (current-time))
						 (diff (- t *time-stamp*)))
				   (set! *time-stamp* t)
				   (number->string-of-length-and-precision diff 8 4))))
 (reset-statistics!))

(define (collect-and-write-statistics!)
 (let* ((t (current-time))
		(diff (- t *time-stamp*))
		(stats
		 (if (file-exists?
			  (human-annotation-statistics-pathname *video-name* *frame-number*))
			 (read-object-from-file
			  (human-annotation-statistics-pathname *video-name* *frame-number*))
			 '())))
  (write-object-to-file
   (cons (make-human-annotation-statistics
		  (current-time) *number-of-clicks* diff *superpixel-type*)
		 stats)
   (human-annotation-statistics-pathname *video-name* *frame-number*))
  (reset-statistics!)
  diff))

(define (reset-statistics!)
 (set! *time-stamp* (current-time))
 (set! *number-of-clicks* 0))

(define (save-and-next-button!)
 (message "")
 (let ((stats (collect-and-write-statistics!)))
  (time-code (save!))
  (time-code (next-frame!))
  (time-code (update-pixmaps!))
  (time-code (redraw-display-pane))
  (time-code (redraw-buttons))
  (message (format #f "Next frame ~a, ~a% ~as" *frame-number*
				   (number->string-of-length-and-precision
					(* (/ *frame-number* *video-length*) 100)
					6
					2)
				   (number->string-of-length-and-precision
					stats
					8 4)))
  (time-code (reset-statistics!))))

(define (previous-button!)
 (message "")
 (previous-frame!)
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons)
 (reset-statistics!)
 (message (format #f "Previous frame, ~a ~a%" *frame-number*
				  (number->string-of-length-and-precision
				   (* (/ *frame-number* *video-length*) 100)
				   6
				   2)))
 (reset-statistics!))

(define (save-button!)
 (message "")
 (let ((stats (collect-and-write-statistics!)))
  (time-code (save!))
  (time-code (update-pixmaps!))
  (message (format #f "Saved"))))

(define (load-button!)
 (message "")
 (load-annotation!)
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons)
 (reset-statistics!)
 (message (format #f "Loaded")))

(define (reset-button!)
 (message "")
 (reset!)
 (update-pixmaps!)
 (redraw-display-pane)
 (redraw-buttons)
 (reset-statistics!))

(define (define-dynamic-radio-buttons get-thunk do-thunk elements)
 (for-each
  (lambda (e)
   (define-button (first e) (second e) (fourth e)
    (lambda () (equal? (get-thunk) (third e)))
    (lambda () (do-thunk (third e)) (redraw-buttons))))
  elements))

(define-application viewer (if (*large-gui?*) 1300 735) (if (*large-gui?*) 700 390) 0 4 8
 (lambda ()
  (setup-extra-x-gcs)
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
  (define-button 7 11 "Save&Next" #f save-and-next-button!)
  (define-dynamic-radio-buttons
   (lambda () *mode*) (lambda (a) (set! *mode* a))
   (append
    '((0 1 person1 "Person1")
	  (1 1 person1-arm1 "Arm1")
	  (2 1 person1-arm2 "Arm2")
	  (3 1 person1-leg1 "Leg1")
	  (4 1 person1-leg2 "Leg2")
	  (0 2 person2 "Person2")
	  (1 2 person2-arm1 "Arm1")
	  (2 2 person2-arm2 "Arm2")
	  (3 2 person2-leg1 "Leg1")
	  (4 2 person2-leg2 "Leg2"))
    (map-indexed
     (lambda (e i)
      (list i 3 e (string-downcase (symbol->string e))))
     (cdr
      (find-if
       (lambda (a) (equal? (first a) (darpa-video-verb *video-name*)))
       (read-object-from-file (verb-objects-pathname)))))))
  (define-key (control #\s) "Save&Next" save-and-next-button!)
  (define-key (meta #\s) "Save" save-button!)
  (define-key (control #\p) "Previous" previous-button!)
  (define-key (control #\n) "Next" next-button!)

  (define-key #\q "None" (lambda () (set! *mode* 'none) (redraw-buttons)))
  (define-key #\w "Person1" (lambda () (set! *mode* 'person1) (redraw-buttons)))
  (define-key #\e "Arm1" (lambda () (set! *mode* 'person1-arm1) (redraw-buttons)))
  (define-key #\r "Arm2" (lambda () (set! *mode* 'person1-arm2) (redraw-buttons)))
  (define-key #\t "Leg1" (lambda () (set! *mode* 'person1-leg1) (redraw-buttons)))
  (define-key #\y "Leg2" (lambda () (set! *mode* 'person1-leg2) (redraw-buttons)))

  (define-key #\s "Person2" (lambda () (set! *mode* 'person2) (redraw-buttons)))
  (define-key #\d "Arm1" (lambda () (set! *mode* 'person2-arm1) (redraw-buttons)))
  (define-key #\f "Arm2" (lambda () (set! *mode* 'person2-arm2) (redraw-buttons)))
  (define-key #\g "Leg1" (lambda () (set! *mode* 'person2-leg1) (redraw-buttons)))
  (define-key #\h "Leg2" (lambda () (set! *mode* 'person2-leg2) (redraw-buttons)))

  ;; TODO ANDREI keyboard
  ;; (for-each-indexed
  ;;  (lambda (e i)
  ;;   (define-key (integer->char (+ (char->integer #\1) i)) (string-downcase (symbol->string e))
  ;;    (lambda () (set! *mode* e) (redraw-buttons))))
  ;;  (second
  ;;   (find-if
  ;;    (lambda (a) (equal? (first a) (darpa-video-verb *video-name*)))
  ;;    (read-object-from-file
  ;;     (string-append
  ;;      (getenv "HOME")
  ;;      "/darpa-collaboration/documentation/verb-objects.sc")))))
  )
 (lambda () #f)				;post
 (lambda () (free-extra-x-gcs))		;fin
 (lambda ()
  (xremove-expose-events)
  (draw-clickable-pixmap-imlib
   *imlib-image*
   0 0 (if *resize?* 2 1)
   (lambda (x1 y1)
	(let ((annotation
		   (superpixel-point->annotation `#(,x1 ,y1) *superpixel-map* *annotation*)))
	 (when annotation
	  (set! *number-of-clicks* (+ 1 *number-of-clicks*))
	  (superpixel-update-annotation annotation *mode*))
	 (redraw-display-pane))))
  #f)					;re
 (lambda () (redraw-display-pane) (redraw-buttons) #f))

(define (draw-clickable-pixmap-imlib image x y scale handler)
 (imlib-context-set-display! (cdr *display*))
 (imlib-context-set-visual! (cdr (xdefaultvisual *display* *screen*)))
 (imlib-context-set-colormap! (xdefaultcolormap *display* *screen*))
 (imlib-context-set-drawable! *display-pane*)
 (imlib-context-set-image! image)
 (imlib-render-image-on-drawable 0 0)
 (define-region x y (* *width* scale) (* *height* scale)
  (lambda (x1 y1)
   (let ((x1 (quantize-coordinate (/ x1 scale)))
	 (y1 (quantize-coordinate (/ y1 scale))))
    (when (and (>= x 0) (< x *width*) (>= y 0) (< y *height*))
     (handler (- x1 x) (- y1 y)))))))

(define (superpixel-point->annotation p map annotations)
 (define (binary-map-ref b p)
  (if (vector? b)
      (image-ref b p)
      (c-superpixel-map-ref b (x p) (y p) *width*)))
 (let ((t (binary-map-ref map p)))
  (find-if (lambda (a) (= t (superpixel-name (superpixel-annotation-superpixel a))))
	   annotations)))

(define (superpixel-annotate-image! annotations image)
 (for-each
  (lambda (c)
   (let ((colour (superpixel-anotation->colour c)))
    (for-each (lambda (p) (set-ppm-pixel! image (x p) (y p) colour))
	      (superpixel->outline
	       (superpixel-annotation-superpixel c)))))
  annotations)
 image)

(define (superpixel-annotate-image-scaled! annotations image)
 (for-each
  (lambda (c)
   (let ((colour (superpixel-anotation->colour c)))
    (for-each (lambda (p)
	       (let ((x (* (x p) 2)) (y (* (y p) 2)))
		(set-ppm-pixel! image x y colour)
		(set-ppm-pixel! image (+ x 1) y colour)
		(set-ppm-pixel! image x (+ y 1) colour)
		(set-ppm-pixel! image (+ x 1) (+ y 1) colour)))
	      (superpixel-annotation->pixels (superpixel-pixels
					      (superpixel-annotation-superpixel c))))))
  annotations)
 image)

(define (superpixel-update-annotation annotation mode)
 (set-superpixel-annotation-label!
  annotation
  (if (and *auto-flip?*
	   (equal? (superpixel-annotation-label annotation) mode))
      'none mode))
 (if (vector? *annotation-image*)
     (superpixel-annotate-image! (list annotation) *annotation-image*)
     (superpixel-annotation-on-imlib (list annotation) *imlib-image*))
 (when *resize?*
  (superpixel-annotate-image-scaled! (list annotation) *annotation-image-scaled*)))

(define (run video-name frame)
 (load-video! video-name frame)
 (reset-statistics!)
 (viewer '()))

(define (most-commonp p l)
 ;; TODO ANDREI Inefficient
 (car (maximump (transitive-equivalence-classesp (lambda (a b) (equal? (p a) (p b))) l) length)))

(define (most-common l)
 (display "A")
 (pp l)(newline)
 ;; TODO ANDREI Inefficient
 (car (maximump (equivalence-classes l) length)))

(define (adjacent-superpixels superpixel superpixel-adjacency)
 (remove
  (superpixel-name superpixel)
  (remove-duplicates
   (reduce
    append
    (remove-if-not
     (lambda (a)
      (or (equal? (first a) (superpixel-name superpixel))
	  (equal? (second a) (superpixel-name superpixel))))
     superpixel-adjacency)
    '()))))

(define (adjacent-annotations annotation annotations superpixel-adjacency)
 (let ((adjacent-superpixels
	(adjacent-superpixels (superpixel-annotation-superpixel annotation) superpixel-adjacency)))
  (remove-if-not
   (lambda (a)
    (member (superpixel-name (superpixel-annotation-superpixel a)) adjacent-superpixels))
   annotations)))

(define (frame-closest-annotation video-name n annotations)
 (let ((new-annotations (map (lambda (a) (vector a '#(0 0)))
			     (frame->empty-annotations *superpixels*))))
  (for-each (lambda (a)
	     (when (superpixel-next (superpixel-annotation-superpixel a))
	      (map
	       (lambda (next)
		(let ((s (find-if
			  (lambda (n-a)
			   (equal? next (superpixel-name (superpixel-annotation-superpixel (x n-a)))))
			  new-annotations)))
		 (when (>= (magnitude (safe-superpixel-velocity (superpixel-annotation-superpixel a)))
			   (magnitude (y s)))
		  (set-superpixel-annotation-label! (x s) (superpixel-annotation-label a))
		  (vector-set! s 1 (safe-superpixel-velocity (superpixel-annotation-superpixel a))))))
	       (superpixel-next (superpixel-annotation-superpixel a)))))
	    annotations)
  (let* ((split (split-by (lambda (a)
			   (some (lambda (p) (image-ref *foreground-segmentation* p))
				 (superpixel-annotation->pixels (x a))))
			  new-annotations))
	 (in (car split))
	 (out (cadr split)))
   (for-each (lambda (a) (set-superpixel-annotation-label! (x a) 'none)) out)
   (for-each
    (lambda (e)
     (set-superpixel-annotation-label! (first e) (second e)))
    (removeq
     #f
     (map (lambda (a)
	   (let ((annotation (x a)))
	    (when (equal? (superpixel-annotation-label annotation) 'none)
	     (let ((adjacent-annotations
		    (remove
		     'none
		     (map
		      superpixel-annotation-label
		      (adjacent-annotations annotation (map x in) *superpixel-adjacency*)))))
	      (unless (null? adjacent-annotations)
	       (list annotation (most-common adjacent-annotations)))))))
	  in)))
   (map x new-annotations))))

(define (superpixel-propagate-essa-annotations! a a-next e e-next)
 (for-each
  (lambda (a)
   (time-code
    (for-each
     (lambda (o)
      (time-code
       (for-each (lambda (on)
		  (let ((on-superpixel (find-if (lambda (e-n) (equal? (superpixel-name e-n) on)) e-next)))
		   (unless on-superpixel
		    (for-each
		     (lambda (a-n) (set-superpixel-annotation-label! a-n (superpixel-annotation-label a)))
		     (remove-if (lambda (a-n)
				 (null? (intersection
					 (superpixel-pixels (superpixel-annotation-superpixel a-next))
					 (superpixel-pixels on-superpixel))))
				a-next)))))
		 (removeq #f (superpixel-next o)))))
     (remove-if (lambda (e)
		 (null? (intersection
			 (superpixel-pixels (superpixel-annotation-superpixel a))
			 (superpixel-pixels e))))
		e))))
  (remove-if (lambda (a) (equal? (superpixel-annotation-label a) 'none)) a))
 a-next)

(define-command
 (main (exactly-one ("standard" standard?
					 (corpus "corpus" string-argument "")
					 (sequence "sequence" string-argument "")
					 (person "person" string-argument "")
					 (location "location" string-argument "")
					 (n "n" integer-argument 0))
					("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("frame" frame? (frame "n" integer-argument 0)))
       (at-most-one ("resize" resize?))
       (exactly-one ("slic" slic?) ("essa" essa?) ("turbopixels" turbopixels?)))
 (set! *resize?* resize?)
 (set! *superpixel-type*
       (cond (slic? 'slic)
			 (essa? 'essa)
			 (turbopixels? 'turbopixels)
			 (else (fuck-up))))
 (let* ((video-name
		 (cond (standard?
				(standard-corpus-video corpus sequence person location n))
			   (darpa? (string->darpa-video name))
			   (else (fuck-up)))))
  (run video-name frame)))
