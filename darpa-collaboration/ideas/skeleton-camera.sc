(MODULE
  SKELETON-CAMERA
  (WITH
    QOBISCHEME
    XLIB
    TOOLLIB-MATLAB
    TOOLLIB-MISC
    TOOLLIB-C-BINDINGS
    TOOLLIB-IMAGE-PROCESSING
    IDEALIB-HASH-TABLE
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    IDEALIB-MATPLOTLIB
    HMM-WBM
    HMM-TRAIN-CLASSIFY
    CUPEDRO-BINDINGS
    TOOLLIB-CAMERA
    TOOLLIB-HACK-TRACK
    TOOLLIB-HACK-TRACK-DRAWABLE)
  (MAIN MAIN))

(include "QobiScheme.sch")
(include "toollib-c-macros.sch")
(include "toollib-c-externals.sch")
(include "toollib-controls.sch")
(include "skeleton-camera.sch")

;; TODO This is a quick cleanup job from imitate, lots of dead code still around

(define *window-xs* '#(0 801))
(define *window-ys* '#(10 10))

(define *image* #f)
(define *image-filename* #f)
(define *image-pixmap* #f)
(define *ellipse-threshold* #f)
(define *view-mode* #f)
(define *matlab-engine* #f)
(define *ellipses* #f)
(define *circle-threshold* #f)
(define *lines* #f)
(define *selecting-ellipses?* #f)
(define *selected-ellipses* '())
(define *grid* #f)
(define *showing-grid?* #f)
(define *show-grid-points* '())
(define *show-grid-lines* '())
(define *n-seconds-to-capture* 4)
(define *frame-rate* 20)

(define *incremental-mode* #f)

;;; Ellipse finder

(define (width v) (vector-ref v 2))
(define (height v) (vector-ref v 3))

;;; Main

(set! *program* "skeleton")
(set! *panic?* #f)

(define (buffer-size)
 ;; Total number of frames that can be captured at once
 1024)

(define (niffty-bell delay)
 (system (format #f
		"mplayer ~a/darpa-collaboration/ideas/KDE_Beep_ClassicBeep.wav >/dev/null 2>&1"
		(getenv "HOME")))
 (interruptible-usleep delay))

(define (play-command camera)
 (let ((n-frames (+ (hack-track-get-current-frame camera) 1)))
  (format #t "Playing back ~a frames at ~a fps~%" n-frames *frame-rate*)
  (for-each
   (lambda (idx)
    (hack-track-show-frame
     0
     idx)
    (xflush *display*)
    (usleep (inexact->exact (round (/ 1000000.0 *frame-rate*)))))
   (enumerate n-frames))
  (redraw-display-pane)))

(define (dump-command camera)
 (for-each
  (lambda (idx)
   (let ((save-filename (format #f "/tmp/hack-track-~a.png" idx))
	 (imlib-image (hack-track-frame->imlib (first *cameras*) idx)))
    (format #t "Dumping frame ~a to file ~a~%" idx save-filename)
    (imlib-context-set-image! imlib-image)
    (imlib-save-image save-filename)
    (imlib-free-image)))
  (enumerate (+ (hack-track-get-current-frame camera) 1))))

(define (capture! camera)
 (let ((n-frames (inexact->exact (round (* *n-seconds-to-capture* *frame-rate*)))))
  ;; Check n-frames is in bounds
  (when (<= n-frames 0) (panic "Cannot capture 0 or less frames"))
  (when (> n-frames (buffer-size))
   (panic "Cannot capture ~a or more frames" (buffer-size)))
  (format #t "Capturing ~as at ~a fps~%" *n-seconds-to-capture* *frame-rate*)
  (hack-track-turn-on-viewfinder! camera 1)
  ;; 3-2-1!
  (niffty-bell 1000000)
  (niffty-bell 1000000)
  (niffty-bell 1000000)
  (niffty-bell 125000)
  (hack-track-capture-frames
   camera
   (inexact->exact 0)
   (inexact->exact (round (/ *hack-track-raw-frame-rate* *frame-rate*)))
   n-frames)
  (hack-track-turn-on-viewfinder! camera 1)
  (niffty-bell 1000)))

(define (capture-command camera)
 (capture! camera)
 (hack-track-turn-off! camera)
 (set! *mode* 'images)
 (redraw-buttons)
 (redraw-display-pane))

(define (redraw)
 (unless *image-pixmap*
  (when *image*
   (set! *image-pixmap* (pnm->pixmap *image*))
   (draw-pixmap *image-pixmap* 0 0)))
 (for-each
  (lambda (e)
   (xdrawarc *display* *display-pane* *red-gc*
	     (+ (- (x e) 3) 0)
	     (+ (- (y e) 3) 0)
	     (width e) (height e) 0 (* 360 64)))
  *selected-ellipses*)
 (when *showing-grid?*
  (for-each
   (lambda (l)
    (xdrawline *display* *display-pane* *dark-red-gc*
	       (+ (- (x (p l)) 3) 0)
	       (+ (- (y (p l)) 3) 0)
	       (+ (- (x (q l)) 3) 0)
	       (+ (- (y (q l)) 3) 0)))
   *show-grid-lines*)
  (for-each
   (lambda (e)
    (xdrawarc *display* *display-pane* *dark-red-gc*
	      (+ (- (x e) 3) 0)
	      (+ (- (y e) 3) 0)
	      (width e) (height e) 0 (* 360 64)))
   *show-grid-points*))
 (when (and (eq? *view-mode* 'opencv) *ellipses*)
  (for-each
   (lambda (e)
    (xdrawarc *display* *display-pane* *red-gc*
	      (+ (- (x e) 3) 0)
	      (+ (- (y e) 3) 0)
	      (width e) (height e) 0 (* 360 64)))
   *ellipses*))
 (when (and (eq? *view-mode* 'lines) *lines*)
  (for-each
   (lambda (l)
    (xdrawline *display* *display-pane* *green-gc*
	       (+ (- (x (p l)) 3) 0)
	       (+ (- (y (p l)) 3) 0)
	       (+ (- (x (q l)) 3) 0)
	       (+ (- (y (q l)) 3) 0)))
   *lines*)))

;;; ------------------------------------------------------------- Viewer

(define-application viewer *width* *height* #f 3 9
 (lambda ()
  (standard-buttons
   9
   (lambda ()
    (for-each (lambda (camera) (hack-track-turn-off! camera))
	      *cameras*)))
  (camera-buttons 1)
  (define-button 1 0 "Snapshot" #f
   (lambda ()
    (for-each (lambda (camera) (hack-track-turn-off! camera)) *cameras*)
    (set! *incremental-mode* #f)
    (for-each (lambda (camera) (hack-track-turn-on-capturing! camera 0 1 1))
	      *cameras*)
    (wait-for-next-frame 0)
    (write-pnm (hack-track-frame->ppm *camera* 0) "/tmp/a.ppm")
    (for-each (lambda (camera) (hack-track-turn-off! camera)) *cameras*)
    (redraw-display-pane)))
  (define-button 2 0 "Viewfinder" #f
   (lambda ()
    (for-each (lambda (camera) (hack-track-turn-off! camera))
	      *cameras*)
    (set! *incremental-mode* #f)
    (for-each (lambda (camera) (hack-track-turn-on-viewfinder! camera 1))
	      *cameras*)))
  (define-button 3 0 "Incremental" #f
   (lambda ()
    (set! *incremental-mode* #t)
    (for-each (lambda (camera) (hack-track-turn-off! camera))
	      *cameras*)
    (for-each (lambda (camera) (hack-track-turn-on-capturing! camera 0 1 1))
	      *cameras*)
    (wait-for-next-frame 0)
    (redraw-display-pane)))
  (define-button 4 0 "Capture" #f
   ;; (dump-command (first *cameras*)) shows how to access via imlib
   (lambda() (capture-command (first *cameras*))))
  (define-button 5 0
   (lambda () (format #f "fps (~a) +" *n-seconds-to-capture*))
   #f
   (lambda ()
    (set! *n-seconds-to-capture* (+ *n-seconds-to-capture* 1))
    (redraw-buttons)))
  (define-button 6 0
   (lambda () (format #f "fps (~a) -" *n-seconds-to-capture*))
   #f
   (lambda ()
    (set! *n-seconds-to-capture* (- *n-seconds-to-capture* 1))
    (redraw-buttons)))  
  (define-button 7 0 "Play" #f
   (lambda () (play-command (first *cameras*))))
  (set-background-task-enabler!
   (lambda ()
    (hack-track-unblock)
    (hack-track-set-interrupts?! #t)))
  (set-background-task-disabler!
   (lambda ()
    (hack-track-block)
    (hack-track-set-interrupts?! #f)))
  (map-indexed
   (lambda (n i)
    (hack-track-startup-with-viewfinder-size n
					     (vector-ref *window-xs* i) ; x
					     (vector-ref *window-ys* i) ; y
					     800 ; width
					     600 ; height
					     (buffer-size) ; n-frames
					     1 ; subsample
					     800 ; view-width
					     600)) ; view-size
   *cameras*)
  (map (lambda (n) (hack-track-turn-on-viewfinder! n 1))
       *cameras*)
  (hack-track-update-drawables!
   `(,(make-drawable-arc
       *display-pane* *blue-gc* 100 100 20 20 0 (* 64 360))
     ,(make-drawable-line
       *display-pane* *red-gc* 20 20 200 200)
     ,(make-drawable-rectangle
       *display-pane* *green-gc* 20 20 200 200)))
  )
 (lambda () #f) ;post
 (lambda () (map hack-track-shutdown *cameras*)) ;fin
 (lambda ()
  (when *image-pixmap* (draw-pixmap *image-pixmap* 0 0))
  (define-region
   0 0 *width* *height*
   (lambda (x y) (display `#(,x ,y)) (newline)))
  (let ((frame (hack-track-get-current-frame 0)))
   (unless *incremental-mode*
    (for-each (lambda (camera)
	       (hack-track-show-frame camera frame))
	      *cameras*))
   (when *incremental-mode*
    #f
    ;; (let* ((segments (pnm->line-segments
    ;; 		      (hack-track-frame->ppm 0 frame) 1.0 0.3 0.3 12 15 10 5)))
    ;;  (for-each (lambda (camera)
    ;; 		(hack-track-show-frame camera frame))
    ;; 	       *cameras*)
    ;;  (for-each
    ;;   (lambda (l)
    ;;    (xdrawline *display* *display-pane* *green-gc*
    ;; 		  (+ (- (x (p l)) 3) 0)
    ;; 		  (+ (- (y (p l)) 3) 0)
    ;; 		  (+ (- (x (q l)) 3) 0)
    ;; 		  (+ (- (y (q l)) 3) 0)))
    ;;   segments))
    ))
  (redraw)
  (when *incremental-mode*
   (for-each (lambda (camera) (hack-track-turn-on-capturing! camera 0 1 1))
	     *cameras*)
   (wait-for-next-frame 0))
  (hack-track-exposed)
  #f) ;re
 (lambda () #f) ;listen
 )

(define (update-image . args)
 (when *image*
  (when (and *ellipse-threshold* (member 'opencv args))
   (set! *ellipses* (pnm->ellipses *image* *ellipse-threshold*)))
  (when (member 'lines args)
   #f
   ;; (set! *lines* (pnm->line-segments *image* 1.0 0.3 0.3 12 15 10 5))
   )))

(define-command (main (exactly-one ("camera" camera?
				    (camera "id" integer-argument 0))))
 (run camera))

(define (run camera)
 (v4l2-initialize)
 (set! clear-display-pane? #f)
 (hack-track-set-raw-frame-rate! 20)
 (set! *cameras* (list camera))
 (when (null? *cameras*) (panic "No cameras initialized"))
 (set! *camera* (first *cameras*))
 (set! *view-mode* 'regular)
 (update-image 'lines)
 (set! *width* 1700)
 (set! *height* 700)
 (viewer '()))




