(MODULE TOOLLIB-HACK-TRACK)
;;; LaHaShem HaAretz U'Mloah

;;; TTMTTD
;;;  1. way to startup just frame buffer, just frame buffer and viewfinder
;;;  2. (HACK-TRACK-SET-FRAME-FROM-PNM! i pnm)
;;;  3. (HACK-TRACK-SET-FRAMES-FROM-PNM-MOVIE! i j pnm-movie)
;;;  4. (HACK-TRACK-SET-FRAMES-FROM-VIDEO-FILE! i j pathname video-type)
;;;  5. make interchangeable between X and XShm
;;;  6. 24->32 bit so can use memcpy
;;;  7. double buffer, check for overrun
;;;  8. uncompressed single-file MBM, MGM, MPM file format
;;;       VIDEO-TYPE-ENUM
;;;       GET-VIDEO-WIDTH
;;;       GET-VIDEO-HEIGHT
;;;       READ-PPM-FROM-VIDEO-INPUT-PORT
;;;       READ-PIXMAP-FROM-VIDEO-INPUT-PORT
;;;       CLOSE-VIDEO-INPUT-PORT
;;;       OPEN-VIDEO-OUTPUT-FILE
;;;       WRITE-PNM-TO-VIDEO-OUTPUT-PORT
;;;       CLOSE-VIDEO-OUTPUT-PORT

(include "QobiScheme.sch")
(include "toollib-c-macros.sch")
(include "toollib-c-externals.sch")
(include "toollib-controls.sch")
(include "toollib-hack-track.sch")

(c-include "time.h")
(c-include "toollib-v4l2-c.h")
(c-include "toollib-hack-track-c.h")
 
;;; Structures

;;; Constants

(define CLOCKS_PER_SEC (c-value int CLOCKS_PER_SEC))
(define wall-fixed #x100000000)
(define wall-fix 0.0)
(define wall-bad #t)

(define number-of-cameras (c-value int NCAMERAS))

;;; Variables

(define *hack-track-size* '())
(define *hack-track-start* (vector #f #f #f #f))
(define *hack-track-finish* (vector #f #f #f #f))
(define *hack-track-raw-frame-rate* #f)

(define (*hack-track-width* camera) (second (assoc camera *hack-track-size*)))
(define (*hack-track-height* camera) (third (assoc camera *hack-track-size*)))

;;; Procedures

(define hack-track-get-max-frames
 (c-function int ("hack_track_get_max_frames")))
(define hack-track-get-current-frame
 (c-function int ("hack_track_get_current_frame" int)))
(define hack-track-get-hack-track-i
 (c-function int ("hack_track_get_hack_track_i" int)))
(define hack-track-interrupts-internal
 (c-function int ("hack_track_interrupts")))
(define hack-track-set-interrupts-internal!
 (c-function void ("hack_track_set_interrupts" int))) 
(define hack-track-startup-internal
 (c-function void ("hack_track_startup" int int int int int int)))
(define hack-track-startup-offline-internal
 (c-function void ("hack_track_startup_offline" int int int int int)))
(define hack-track-startup-with-viewfinder-internal
 (c-function void ("hack_track_startup_with_viewfinder" int int int 
		   int int pointer int unsigned int int int int int)))
(define hack-track-restartup-internal
 (c-function void ("hack_track_restartup" int int int int int int)))
(define hack-track-block (c-function void ("hack_track_block")))
(define hack-track-unblock (c-function void ("hack_track_unblock")))
(define hack-track-turn-on-viewfinder-internal!
 (c-function void ("hack_track_turn_on_viewfinder" int int)))
(define hack-track-turn-on-capturing-internal!
 (c-function void ("hack_track_turn_on_capturing" int int int int)))
(define hack-track-turn-off-internal!
 (c-function void ("hack_track_turn_off" int)))
(define hack-track-capture-frames-internal
 (c-function void ("hack_track_capture_frames" int int int int)))
(define hack-track-malloc (c-function void ("hack_track_malloc" int int)))
(define hack-track-page-in-frames
 (c-function void ("hack_track_page_in_frames" int int int)))
(define hack-track-get-red
 (c-function int ("hack_track_get_red" int int int int)))
(define hack-track-get-green
 (c-function int ("hack_track_get_green" int int int int)))
(define hack-track-get-blue
 (c-function int ("hack_track_get_blue" int int int int)))
(define hack-track-get-pixel
 (c-function int ("hack_track_get_pixel" int int int int)))
(define hack-track-set-red!
 (c-function void ("hack_track_set_red" int int int int int)))
(define hack-track-set-green!
 (c-function void ("hack_track_set_green" int int int int int)))
(define hack-track-set-blue!
 (c-function void ("hack_track_set_blue" int int int int int)))
(define hack-track-set-pixel!
 (c-function void ("hack_track_set_pixel" int int int int int)))
(define hack-track-copy-frame-to-ximage!
 (c-function void ("hack_track_copy_frame_to_ximage" int int pointer)))
(define hack-track-show-frame-internal
 (c-function void ("hack_track_show_frame" int int)))
(define hack-track-start! (c-function void ("hack_track_start" int int)))
(define hack-track-finish! (c-function void ("hack_track_finish" int)))
(define hack-track-free (c-function void ("hack_track_free" int)))
(define hack-track-shutdown (c-function void ("hack_track_shutdown" int)))
(define hack-track-shutdown-offline
 (c-function void ("hack_track_shutdown_offline" int)))
(define hack-track-send-frame
 (c-function void ("hack_track_send_frame" int pointer int)))
(define hack-track-receive-frame
 (c-function void ("hack_track_receive_frame" int pointer int)))
(define hack-track-exposed (c-function void ("hack_track_exposed")))
(define hack-track-frame->imlib ;; camera frame-no
 (c-function pointer ("hack_track_frame_to_imlib" int int)))

(define (read-wall-clock)
 (let ((time (wall-clock)))
  (if (< time 0)
      (when wall-bad
       (set! wall-fix (+ wall-fixed wall-fix))
       (set! wall-bad #f))
      (set! wall-bad #t))
  (+ time wall-fix)))

(define (interruptible-usleep n)
 (hack-track-set-interrupts?! #t)
 (let ((end (+ (read-wall-clock) (* (/ n 1000000.0) 100))))
  (let loop ()
   (usleep 1000)
   (unless (> (read-wall-clock) end) (loop))))
 (hack-track-set-interrupts?! #f))

;;; Line Segments

(define (hack-track-set-raw-frame-rate! raw-frame-rate)
 (set! *hack-track-raw-frame-rate*
       (if raw-frame-rate (exact->inexact raw-frame-rate) 15.0)))

(define (hack-track-interrupts?) (= (hack-track-interrupts-internal) 1))

(define (hack-track-set-interrupts?! on?)
 (hack-track-set-interrupts-internal! (if on? 1 0))
 #f)

(define (hack-track-startup camera width height nframes subsample)
 (set! *hack-track-size* (cons `(,camera ,width ,height) *hack-track-size*))
 (hack-track-startup-internal
  camera
  width
  height
  nframes
  subsample
  (inexact->exact (round *hack-track-raw-frame-rate*)))
 #f)

(define (hack-track-startup-offline camera width height nframes subsample)
 (set! *hack-track-size* (cons `(,camera ,width ,height) *hack-track-size*))
 (hack-track-startup-offline-internal camera width height nframes subsample)
 #f)

(define (hack-track-startup-with-viewfinder-size
	 camera x y width height nframes subsample view-width view-height)
 (unless (and
	  (= (visual-class (xdefaultvisual *display* *screen*)) truecolor)
	  (= (visual-red_mask (xdefaultvisual *display* *screen*)) 16711680)
	  (= (visual-green_mask (xdefaultvisual *display* *screen*)) 65280)
	  (= (visual-blue_mask (xdefaultvisual *display* *screen*)) 255)
	  (= (xdefaultdepth *display* *screen*) 24))
  (panic "Hack-Track can only handle 24-bit TRUECOLOR visuals"))
 (set! *hack-track-size* (cons `(,camera ,width ,height) *hack-track-size*))
 (hack-track-startup-with-viewfinder-internal
  camera
  width
  height
  nframes
  subsample
  (cdr *display*)
  *screen*
  *display-pane*
  x
  y
  (min view-width *display-pane-width*)
  (min view-height *display-pane-height*)
  (inexact->exact (round *hack-track-raw-frame-rate*)))
 #f)

(define (hack-track-startup-with-viewfinder
	 camera x y width height nframes subsample)
 (hack-track-startup-with-viewfinder-size
  camera x y width height nframes subsample
  *display-pane-width*
  *display-pane-height*))

(define (hack-track-restartup camera width height nframes subsample)
 (set! *hack-track-size* (cons `(,camera ,width ,height) *hack-track-size*))
 (hack-track-restartup-internal
  camera
  width
  height
  nframes
  subsample
  (inexact->exact (round *hack-track-raw-frame-rate*)))
 #f)

(define (hack-track-turn-on-viewfinder! camera m)
 (hack-track-turn-on-viewfinder-internal! camera m)
 #f)

(define (hack-track-turn-on-capturing! camera i l n)
 (hack-track-turn-on-capturing-internal! camera i l n)
 #f)

(define (hack-track-get-fresh-image! camera)
 (hack-track-turn-on-capturing-internal! camera 0 1 4)
 (wait-for-next-frame 3))

(define (hack-track-turn-off! camera)
 (hack-track-turn-off-internal! camera)
 #f)

(define (hack-track-frame->ppm camera i)
 (let ((red (make-matrix (*hack-track-height* camera) (*hack-track-width* camera)))
       (green (make-matrix (*hack-track-height* camera) (*hack-track-width* camera)))
       (blue (make-matrix (*hack-track-height* camera) (*hack-track-width* camera))))
  (do ((y 0 (+ y 1))) ((>= y (*hack-track-height* camera)))
   (do ((x 0 (+ x 1))) ((>= x (*hack-track-width* camera)))
    (matrix-set! red y x (hack-track-get-red camera i y x))
    (matrix-set! green y x (hack-track-get-green camera i y x))
    (matrix-set! blue y x (hack-track-get-blue camera i y x))))
  (make-ppm #t 255 red green blue)))

(define (hack-track-frames->ppm-movie camera m n)
 (map-n-vector (lambda (i) (hack-track-frame->ppm camera (+ m i))) (- n m)))

(define (ximage->ppm ximage width height)
 ;; This is hardwired to raw and 8x8x8 RGB.
 (make-ppm
  #t
  255
  (map-n-vector
   (lambda (y)
    (map-n-vector (lambda (x) (quotient (xgetpixel ximage x y) 65536)) width))
   height)
  (map-n-vector
   (lambda (y)
    (map-n-vector
     (lambda (x) (quotient (remainder (xgetpixel ximage x y) 65536) 256))
     width))
   height)
  (map-n-vector
   (lambda (y)
    (map-n-vector (lambda (x) (remainder (xgetpixel ximage x y) 256)) width))
   height)))

(define c-xcreateimage 
 (c-function pointer ("XCreateImage" pointer pointer int int 
		      int pointer int int int int)))

(define (hack-track-frame->pixmap camera i)
 (let* ((default-visual (xdefaultvisual *display* *screen*))
	(default-depth (xdefaultdepth *display* *screen*))
	(data (malloc (* 4 (*hack-track-width* camera) (*hack-track-height* camera))))
	(ximage (cons 'ximagep
		      (c-xcreateimage
		       (cdr *display*) (cdr default-visual) default-depth
		       zpixmap 0 data (*hack-track-width* camera) (*hack-track-height* camera)
		       32 0)))
	(pixmap (xcreatepixmap
		 *display* *display-pane*
		 (*hack-track-width* camera) (*hack-track-height* camera) default-depth)))
  (hack-track-copy-frame-to-ximage! camera i (cdr ximage))
  (xputimage *display* pixmap *color-gc* ximage 0 0 0 0
	     (*hack-track-width* camera) (*hack-track-height* camera))
  (xdestroyimage ximage)
  pixmap))

(define (hack-track-frames->pixmaps camera m n)
 (let* ((default-visual (xdefaultvisual *display* *screen*))
	(default-depth (xdefaultdepth *display* *screen*))
	(data (malloc (* 4 (*hack-track-width* camera) (*hack-track-height* camera))))
	(ximage (cons 'ximagep
		      (c-xcreateimage
		       (cdr *display*) (cdr default-visual) default-depth
		       zpixmap 0 data (*hack-track-width* camera) (*hack-track-height* camera)
		       32 0)))
	(pixmaps (map-n-vector
		  (lambda (i)
		   (xcreatepixmap
		    *display* *display-pane*
		    (*hack-track-width* camera) (*hack-track-height* camera) default-depth))
		  (- n m))))
  (do ((i 0 (+ i 1))) ((>= i (- n m)))
   (hack-track-copy-frame-to-ximage! camera (+ m i) (cdr ximage))
   (xputimage *display* (vector-ref pixmaps i) *color-gc* ximage 0 0 0 0
	      (*hack-track-width* camera) (*hack-track-height* camera)))
  (xdestroyimage ximage)
  pixmaps))

(define (hack-track-show-frame camera i)
 (hack-track-show-frame-internal camera i)
 #f)

(define (hack-track-show-frames camera m n)
 (do ((i m (+ i 1))) ((>= i n))
  (hack-track-show-frame camera i)
  (usleep (inexact->exact (round (/ 1000000.0 *frame-rate*))))))

(define (hack-track-capture-frames camera i l n)
 (hack-track-set-interrupts?! #t)
 (hack-track-capture-frames-internal camera i l n)
 (hack-track-set-interrupts?! #f))

(define (hack-track-frames->video-file camera m n pathname video-type)
 (call-with-video-output-file
  pathname video-type (- n m)
  (lambda (video-output-port)
   (do ((i m (+ i 1))) ((>= i n))
    (write-pnm-to-video-output-port
     (hack-track-frame->ppm camera i) video-output-port)))))

(define (hack-track->pnm-movie
	 camera before after height width nframes subsample)
 (let ((downsample-factor (/ *hack-track-raw-frame-rate* *frame-rate*)))
  (unless (< (abs (- downsample-factor (round downsample-factor))) 0.001)
   (panic "Frame rate does not yield an integral downsample factor"))
  (hack-track-startup camera width height nframes subsample)
  (before)
  (hack-track-capture-frames
   camera 0 (inexact->exact (round downsample-factor)) nframes)
  (after)
  (let ((pnm-movie (hack-track-frames->ppm-movie camera 0 nframes)))
   (hack-track-shutdown camera)
   pnm-movie)))

(define (hack-track->video-file camera
				before
				after
				width
				height
				nframes
				subsample
				pathname
				video-type)
 (let ((downsample-factor (/ *hack-track-raw-frame-rate* *frame-rate*)))
  (unless (< (abs (- downsample-factor (round downsample-factor))) 0.001)
   (panic "Frame rate does not yield an integral downsample factor"))
  (hack-track-startup camera width height nframes subsample)
  (before)
  (hack-track-capture-frames
   camera 0 (inexact->exact (round downsample-factor)) nframes)
  (after)
  (hack-track-frames->video-file camera 0 nframes pathname video-type)
  (hack-track-shutdown camera)
  #f))

(define (hack-track-ppm->frame! camera i ppm)
 (unless (and (= (*hack-track-height* camera) (pnm-height ppm))
	      (= (*hack-track-width* camera) (pnm-width ppm)))
  (panic "Wrong height or width"))
 (let ((red (ppm-red ppm))
       (green (ppm-green ppm))
       (blue (ppm-blue ppm)))
  (do ((y 0 (+ y 1))) ((>= y (*hack-track-height* camera)))
   (do ((x 0 (+ x 1))) ((>= x (*hack-track-width* camera)))
    (hack-track-set-red! camera i y x (matrix-ref red y x))
    (hack-track-set-green! camera i y x (matrix-ref green y x))
    (hack-track-set-blue! camera i y x (matrix-ref blue y x))))))

(define (with-hack-track-paused f)
 (hack-track-block)
 (hack-track-set-interrupts?! #f)
 (let ((result (f)))
  (hack-track-unblock)
  (hack-track-set-interrupts?! #t)
  result))

;;; Tam V'Nishlam Shevah L'El Borei Olam
