(MODULE TOOLLIB-CAMERA)

(include "QobiScheme.sch")
(include "toollib-c-macros.sch")
(include "toollib-camera.sch")
(include "toollib-controls.sch")
(include "toollib-c-externals.sch")

(c-include "linux/videodev2.h")
(c-include "toollib-v4l2-c.h")
(c-include "toollib-logitech-controls.h")

(c-ffi:rename-id "v4l2_frmsizeenum" "v4l2-frame-size")
(c-ffi:rename-id "v4l2_frmivalenum" "v4l2-frame-interval")
(c-ffi:rename-id "v4l2_streamparm" "v4l2-stream-param")
(c-ffi:rename-id "v4l2_queryctrl" "v4l2-query-control")

;;; Parameters

(define *maximum-number-of-cameras* 10)
(define *cameras* '())
(define *camera* 0)
(define *auto?* #t)

(define v4l2-ioctl-request-code-to-string (c-function string ("v4l2_ioctl_request_code_to_string" int)))
(define v4l2-cid-to-string (c-function string ("v4l2_cid_to_string" int)))
(define v4l2-print-available-controls (c-function void ("v4l2_print_available_controls" int)))

(define v4l2-initialize (c-function void ("v4l2_initialize")))
(define v4l2-deinitialize (c-function void ("v4l2_deinitialize")))

;;; create camera-get-<control> and camera-set-<control>
(define-macro define-control
 (lambda (f e)
  (e (let* ((control (second f))
	    (v4l2-cid (third f))
	    (scheme-cid (string->pretty-symbol
			 (schemeify-name (symbol->string v4l2-cid)))))
      `(begin
	(define (,(append-symbols 'camera-set- control '!) camera value)
	 (with-hack-track-paused
	  (lambda () (camera-set-control! camera ,scheme-cid value))))
	(define ,scheme-cid (c-value int ,v4l2-cid))
	(define (,(append-symbols 'camera-get- control) camera)
	 (with-hack-track-paused
	  (lambda () (camera-get-control camera ,scheme-cid)))))) e)))

(define (camera-set-framerate! c n d)
 (with-alloc 
  (c-sizeof "struct v4l2_streamparm")
  (lambda (s)
   (v4l2-stream-param-type-set! s BUF-TYPE-VIDEO-CAPTURE)
   (v4l2-stream-param-timeperframe-numerator-set! s n)
   (v4l2-stream-param-timeperframe-denominator-set! s d)
   (= (ioctl (camera-fd c) VIDIOC-SET-PARAMETERS s) 0))))

(define BUF-TYPE-VIDEO-CAPTURE (c-value int V4L2_BUF_TYPE_VIDEO_CAPTURE))

(c-define-field v4l2-format index int 0)
(c-define-field v4l2-format type int 4)
(c-define-field v4l2-format pixel int 44)

(c-define-struct-field "v4l2_frmsizeenum" "index" int)
(c-define-struct-field "v4l2_frmsizeenum" "pixel_format" int)
(c-define-struct-field "v4l2_frmsizeenum" "type" int)
(c-define-struct-field "v4l2_frmsizeenum" "discrete" int)

(c-define-field v4l2-discrete width int 12)
(c-define-field v4l2-discrete height int 16)

(c-define-struct-field "v4l2_frmivalenum" "index" int)
(c-define-struct-field "v4l2_frmivalenum" "pixel_format" int)
(c-define-struct-field "v4l2_frmivalenum" "width" int)
(c-define-struct-field "v4l2_frmivalenum" "height" int)
(c-define-struct-field "v4l2_frmivalenum" "type" int)
(c-define-struct-field "v4l2_frmivalenum" "discrete" int)

(define TYPE-DISCRETE (c-value int V4L2_FRMSIZE_TYPE_DISCRETE))
(define TYPE-CONTINUOUS (c-value int V4L2_FRMSIZE_TYPE_CONTINUOUS))
(define TYPE-STEPWISE (c-value int V4L2_FRMSIZE_TYPE_STEPWISE))

(c-define-field v4l2-frame-interval numerator int 20)
(c-define-field v4l2-frame-interval denominator int 24)

(c-define-struct-field "v4l2_fract" "numerator" int)
(c-define-struct-field "v4l2_fract" "denominator" int)

(c-define-struct-field "v4l2_streamparm" "type" int)
(c-define-field v4l2-stream-param capability int
		(+ (struct-offset "v4l2_streamparm" "parm")
		   (struct-offset "v4l2_captureparm" "capability")))
(c-define-field v4l2-stream-param capturemode int
		(+ (struct-offset "v4l2_streamparm" "parm")
		   (struct-offset "v4l2_captureparm" "capturemode")))
(c-define-field v4l2-stream-param timeperframe-numerator int
		(+ (struct-offset "v4l2_streamparm" "parm")
		   (struct-offset "v4l2_captureparm" "timeperframe")
		   (struct-offset "v4l2_fract" "numerator")))
(c-define-field v4l2-stream-param timeperframe-denominator int
		(+ (struct-offset "v4l2_streamparm" "parm")
		   (struct-offset "v4l2_captureparm" "timeperframe")
		   (struct-offset "v4l2_fract" "denominator")))
(c-define-field v4l2-stream-param extendedmode int 
		(+ (struct-offset "v4l2_streamparm" "parm")
		   (struct-offset "v4l2_captureparm" "extendedmode")))

(c-define-field v4l2-stream-param readbuffers int
		(+ (struct-offset "v4l2_streamparm" "parm")
		   (struct-offset "v4l2_captureparm" "readbuffers")))

(c-define-struct-field "v4l2_control" "id" int)
(c-define-struct-field "v4l2_control" "value" int)

(c-define-struct-field "v4l2_querymenu" "id" int)
(c-define-struct-field "v4l2_querymenu" "index" int)
(c-define-struct-field "v4l2_querymenu" "name" char)

(c-define-struct-field "v4l2_queryctrl" "id" int)
(c-define-struct-field "v4l2_queryctrl" "type" int)
;; TODO Andrei: Add support for strings
(c-define-struct-field "v4l2_queryctrl" "name" char)
(c-define-struct-field "v4l2_queryctrl" "minimum" int)
(c-define-struct-field "v4l2_queryctrl" "maximum" int)
(c-define-struct-field "v4l2_queryctrl" "step" int)
(c-define-struct-field "v4l2_queryctrl" "default_value" int)
(c-define-struct-field "v4l2_queryctrl" "flags" int)

(c-define-struct-field "v4l2_capability" "driver" char)
(c-define-struct-field "v4l2_capability" "card" char)
(c-define-struct-field "v4l2_capability" "bus_info" char)
(c-define-struct-field "v4l2_capability" "version" int)
(c-define-struct-field "v4l2_capability" "capabilities" int)

(define VIDIOC-ENUM-FMT (c-value int VIDIOC_ENUM_FMT))
(define VIDIOC-ENUM-FRAME-SIZES (c-value int VIDIOC_ENUM_FRAMESIZES))
(define VIDIOC-ENUM-FRAME-INTERVALS (c-value int VIDIOC_ENUM_FRAMEINTERVALS))
(define VIDIOC-GET-PARAMETERS (c-value int VIDIOC_G_PARM))
(define VIDIOC-SET-PARAMETERS (c-value int VIDIOC_S_PARM))
(define VIDIOC-GET-CONTROL (c-value int VIDIOC_G_CTRL))
(define VIDIOC-SET-CONTROL (c-value int VIDIOC_S_CTRL))
(define VIDIOC-QUERY-CONTROL (c-value int VIDIOC_QUERYCTRL))
(define VIDIOC-QUERY-MENU (c-value int VIDIOC_QUERYMENU))
(define VIDIOC-QUERY-CAPABILITIES (c-value int VIDIOC_QUERYCAP))

(define v4l2-fd (c-value pointer "v4l2_fd"))

(define CTRL-FLAG-NEXT-CTRL (c-value int V4L2_CTRL_FLAG_NEXT_CTRL))

(define EINVAL (c-value int EINVAL))

(include "toollib-control-data.sch")

(define (get-formats fd)
 (with-alloc
  (c-sizeof "struct v4l2_fmtdesc")
  (lambda (fmt)
   (v4l2-format-index-set! fmt 0)
   (v4l2-format-type-set! fmt BUF-TYPE-VIDEO-CAPTURE)
   (let loop ((l '()))
    (if (= (ioctl fd VIDIOC-ENUM-FMT fmt) 0)
	(begin
	 (v4l2-format-index-update fmt (lambda (x) (+ x 1)))
	 (loop (cons (v4l2-format-pixel fmt) l)))
	l)))))

(define (get-frame-sizes fd pixfmt)
 (with-alloc
  (c-sizeof "struct v4l2_frmsizeenum")
  (lambda (f)
   (v4l2-frame-size-index-set! f 0)
   (v4l2-frame-size-pixel-format-set! f pixfmt)
   (let loop ((l '()))
    (if (= (ioctl fd VIDIOC-ENUM-FRAME-SIZES f) 0)
	(begin
	 (v4l2-frame-size-index-update f (lambda (x) (+ x 1)))
	 (if (= (v4l2-frame-size-type f) TYPE-DISCRETE)
	     (loop (cons (list
			  (v4l2-discrete-width f)
			  (v4l2-discrete-height f))
			 l))
	     (begin
	      (display (format
			"get-format-info can't handle type: ~a~%"
			(v4l2-frame-size-type f)))
	      (loop l))))
	l)))))

(define (get-frame-intervals fd pixfmt width height)
 (with-alloc
  (c-sizeof "struct v4l2_frmivalenum")
  (lambda (f)
   (v4l2-frame-interval-index-set! f 0)
   (v4l2-frame-interval-pixel-format-set! f pixfmt)
   (v4l2-frame-interval-width-set! f width)
   (v4l2-frame-interval-height-set! f height)
   (let loop ((l '()))
    (if (= (ioctl fd VIDIOC-ENUM-FRAME-INTERVALS f) 0)
	(begin
	 (v4l2-frame-interval-index-update f (lambda (x) (+ x 1)))
	 (if (= (v4l2-frame-interval-type f) TYPE-DISCRETE)
	     (loop (cons (list
			  (v4l2-frame-interval-numerator f)
			  (v4l2-frame-interval-denominator f))
			 l))
	     (begin
	      (display (format
			"get-format-info can't handle type: ~a~%"
			(v4l2-frame-interval-type f)))
	      (loop l))))
	l)))))

(define (get-format-info fd pixfmt)
 (map (lambda (s) (list `(,(car s) ,(cadr s))
			(get-frame-intervals fd pixfmt (car s) (cadr s))))
      (get-frame-sizes fd pixfmt)))

(define (camera-automatic-format fd width height)
 (call-with-current-continuation
  (lambda (c)
   (for-each
    (lambda (pixfmt)
     (when (some (lambda (s) (and (= (car s) width) (= (cadr s) height)))
		 (get-frame-sizes fd pixfmt))
      (c pixfmt)))
    (get-formats fd))
   #f)))

(define (list-cameras)
 (map
  ((curry2 string-append) "/dev/v4l/")
  (remove-if-not
   ((curry2 prefix?) "video")
   (directory-list "/dev/v4l"))))

(define (camera-get-control camera id)
 (with-alloc
  (c-sizeof "struct v4l2_control")
  (lambda (control)
   (v4l2-control-id-set! control id)
   (v4l2-control-value-set! control 0)
   (when (= (ioctl (camera-fd camera) VIDIOC-GET-CONTROL control) 0)
    (v4l2-control-value control)))))

(define (camera-set-control! camera id value)
 (with-alloc
  (c-sizeof "struct v4l2_control")
  (lambda (control)
   (v4l2-control-id-set! control id)
   (v4l2-control-value-set! control value)
   (= (ioctl (camera-fd camera) VIDIOC-SET-CONTROL control) 0))))

(define (camera-fd camera)
 ((c-sized-int-ptr-ref (c-sizeof "int") #f) v4l2-fd (* camera 4)))

(define-structure camera-control id type name minimum maximum step default_value flags)

(define (camera-control-information camera id)
 (with-alloc
  (c-sizeof "struct v4l2_queryctrl")
  (lambda (query)
   (v4l2-query-control-id-set! query id)
   (when (= (ioctl (camera-fd camera) VIDIOC-QUERY-CONTROL query) 0)
    (make-camera-control
     (v4l2-query-control-id query)
     (v4l2-query-control-type query)
     (v4l2-query-control-name query)
     (v4l2-query-control-minimum query)
     (v4l2-query-control-maximum query)
     (v4l2-query-control-step query)
     (v4l2-query-control-default-value query)
     (v4l2-query-control-flags query))))))

(define (control-step camera id)
 (camera-control-step (camera-control-information camera id)))

(define ioctl (c-function int ("xioctl" int int pointer)))

;;; Waiting

(define (wait-for-pan-tilt camera)
 ;; debugging
 (when #f
  (let loop ((i 0))
   (let ((status (v4l-pan-tilt-status camera)))
    (when (= i 20) (message "Time out waiting for pan/tilt") (abort))
    (when (zero? status)
     ;; Wait for pan/tilt.
     (interruptible-usleep 500000)
     (loop (+ i 1)))))))

(define (wait-for-next-frame frame)
 (let loop ((i 0))
  ;; hardwired
  ;; 200ms which is 5fps
  (when (= i 200) (message "Time out waiting for next frame") (abort))
  (when (some
	 (lambda (camera) (< (hack-track-get-current-frame camera) frame))
	 *cameras*)
   (interruptible-usleep 1000)
   (loop (+ i 1))))
 (unless (every
	  (lambda (camera) (= (hack-track-get-current-frame camera) frame))
	  *cameras*)
  (message
   (format #f "Skipped a frame ~s ~s"
	   (map (lambda (camera) (hack-track-get-current-frame camera))
		*cameras*)
	   frame))
  (abort)))

(define (wait-for-camera-next-frame camera frame)
 (let loop ((i 0))
  ;; hardwired
  ;; 200ms which is 5fps
  (when (= i 200) (message "Time out waiting for next frame") (abort))
  (when (< (hack-track-get-current-frame camera) frame)
   (interruptible-usleep 1000)
   (loop (+ i 1))))
 (unless (= (hack-track-get-current-frame camera) frame)
  (message
   (format #f "Skipped a frame ~s ~s"
	   (hack-track-get-current-frame camera)
	   frame))
  (abort)))

;;; Pan and tilt get special treatment, logitech decided that
;;; separate/persistent pan/tilt counters were a good idea, and so
;;; they have to be reset to 0; also have to wait for the camera
;;; motion to finish. Frames don't seem to get delivered in the
;;; meantime

(define *pan/tilt-timeout* 500000)

(define (camera-pan camera n)
 (camera-set-pan-relative! camera n)
 (interruptible-usleep *pan/tilt-timeout*)
 (camera-set-pan-relative! camera 0))

(define (camera-tilt camera n)
 (camera-set-tilt-relative! camera n)
 (interruptible-usleep *pan/tilt-timeout*)
 (camera-set-tilt-relative! camera 0))

(define (camera-pan-reset camera)
 (camera-set-pan-reset! camera 1)
 (interruptible-usleep (* 7 *pan/tilt-timeout*))
 (camera-set-pan-reset! camera 0))

(define (camera-tilt-reset camera)
 (camera-set-tilt-reset! camera 1)
 (interruptible-usleep (* 3 *pan/tilt-timeout*))
 (camera-set-tilt-reset! camera 0))

(define (camera-buttons r)
 (define-button 1 r "Brightness +" #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-brightness! *camera*
			     (+ (camera-get-brightness *camera*)
				(* 10 (control-step *camera*
						    v4l2-cid-brightness))))))
   (redraw-buttons)))
 (define-button 1 (+ 1 r)
  (lambda () (format #f "~a -" (camera-get-brightness *camera*)))
  #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-brightness! *camera*
			     (- (camera-get-brightness *camera*)
				(* 10 (control-step *camera*
						    v4l2-cid-brightness))))))
   (redraw-buttons)))
 (define-button 2 r "Contrast +" #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-contrast! *camera* (+ (camera-get-contrast *camera*)
				       (* 10 (control-step
					      *camera*
					      v4l2-cid-contrast))))))
   (redraw-buttons)))
 (define-button 2 (+ 1 r)
  (lambda () (format #f "~a -" (camera-get-contrast *camera*)))
  #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-contrast! *camera* (- (camera-get-contrast *camera*)
				       (* 10 (control-step
					      *camera*
					      v4l2-cid-contrast))))))
   (redraw-buttons)))
 (define-button 3 r "Saturation +" #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-saturation! *camera* (+ (camera-get-saturation *camera*)
					 (* 10 (control-step
						*camera*
						v4l2-cid-saturation))))))
   (redraw-buttons)))
 (define-button 3 (+ 1 r)
  (lambda () (format #f "~a -" (camera-get-saturation *camera*)))
  #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-saturation! *camera* (- (camera-get-saturation *camera*)
					 (* 10 (control-step
						*camera*
						v4l2-cid-saturation))))))
   (redraw-buttons)))

 ;; FIXME v4l2 does not work
 
 ;; (define-button 3 r "Focus +" #f
 ;;  (lambda ()
 ;;   (with-hack-track-paused
 ;;    (lambda ()
 ;;     (camera-set-focus! *camera* (+ (camera-get-focus *camera*)
 ;; 				    (* 3 (control-step *camera*
 ;; 						       v4l2-cid-focus))))))
 ;;   (redraw-buttons)))
 
 ;; (define-button 3 (+ 1 r)
 ;;  (lambda () (format #f "~a -" (camera-get-focus *camera*)))
 ;;  #f
 ;;  (lambda ()
 ;;   (with-hack-track-paused
 ;;    (lambda ()
 ;;     (camera-set-focus! *camera* (- (camera-get-focus *camera*)
 ;; 				    (* 3 (control-step *camera*
 ;; 						       v4l2-cid-focus))))))
 ;;   (redraw-buttons)))
 
 (define-button 4 r "Gain +" #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-gain! *camera* (+ (camera-get-gain *camera*)
				   (* 3 (control-step *camera*
						      v4l2-cid-gain))))))
   (redraw-buttons)))
 (define-button 4 (+ 1 r)
  (lambda () (format #f "~a -" (camera-get-gain *camera*)))
  #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-gain! *camera* (- (camera-get-gain *camera*)
				   (* 3 (control-step *camera*
						      v4l2-cid-gain))))))
   (redraw-buttons)))
 (define-button 5 r "Sharpness +" #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-sharpness! *camera* (+ (camera-get-sharpness *camera*)
					(* 3 (control-step
					      *camera*
					      v4l2-cid-sharpness))))))
   (redraw-buttons)))
 (define-button 5 (+ 1 r)
  (lambda () (format #f "~a -" (camera-get-sharpness *camera*)))
  #f
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (camera-set-sharpness! *camera* (- (camera-get-sharpness *camera*)
					(* 3 (control-step
					      *camera*
					      v4l2-cid-sharpness))))))
   (redraw-buttons)))
 (define-button 6 r "Tilt +" #f
  (lambda () (with-hack-track-paused (lambda () (camera-tilt *camera* 200))) (redraw-buttons)))
 (define-button 6 (+ 1 r) "Tilt -" #f
  (lambda () (with-hack-track-paused (lambda () (camera-tilt *camera* -200))) (redraw-buttons)))
 (define-button 7 r "Pan +" #f
  (lambda () (with-hack-track-paused (lambda () (camera-pan *camera* 200))) (redraw-buttons)))
 (define-button 7 (+ 1 r) "Pan -" #f
  (lambda () (with-hack-track-paused (lambda () (camera-pan *camera* -200))) (redraw-buttons)))
 (define-button 8 r "Pan reset" #f
  (lambda () (with-hack-track-paused (lambda () (camera-pan-reset *camera*))) (redraw-buttons)))
 (define-button 8 (+ 1 r) "Tilt reset" #f
  (lambda () (with-hack-track-paused (lambda () (camera-tilt-reset *camera*))) (redraw-buttons)))
 (define-toggle-button 9 r "Adjusting" *auto?*
  (lambda ()
   (with-hack-track-paused
    (lambda ()
     (if *auto?*
	 (begin
	  (camera-set-auto-white-balance! *camera* 1)
	  (camera-set-exposure-auto! *camera* 3)
	  (camera-set-backlight-compensation! *camera* 1))
	 (begin
	  (camera-set-auto-white-balance! *camera* 0)
	  (camera-set-exposure-auto! *camera* 0)
	  (camera-set-backlight-compensation! *camera* 0)))))
   (redraw-buttons))))

(define (flush-stale-frames camera)
 (hack-track-turn-on-capturing! camera 0 1 4)
 (wait-for-camera-next-frame camera 3))

(define (get-fresh-frame camera)
 (hack-track-turn-on-capturing! camera 0 1 5)
 (wait-for-camera-next-frame camera 4)
 (hack-track-frame->ppm
  camera
  (hack-track-get-current-frame camera)))

(define (dummy-var) histogram-variance)

(define (get-image-variance c)
 (let* ((image (pnm->pgm (get-fresh-frame c)))
	(hist (find-histogram (pgm-grey image) (pgm-maxval image))))
  (histogram-variance hist (histogram-mean hist 0) 0)))

(define (get-autofocus-details c step)
 (let ((v (get-image-variance c)))
  (camera-set-focus! c (- (camera-get-focus c)
			  (* step (control-step c v4l2-cid-focus))))
  (let ((v- (get-image-variance c)))
   (camera-set-focus! c (+ (camera-get-focus c)
			   (* 2 step (control-step c v4l2-cid-focus))))
   (let ((v+ (get-image-variance c)))
    `#(,(cond ((and (> v- v) (> v- v+)) -1)
	      ((and (> v+ v) (> v+ v-)) +1)
	      (else 0))
       ,v+)))))

(define (autofocus-camera! c step)
 (let* ((a (get-autofocus-details c step))
	(dir (x a))
	(var (y a)))
  (let loop ((oldv var))
   (camera-set-focus! c (+ (camera-get-focus c)
			   (* dir step (control-step c v4l2-cid-focus))))
   (let ((newv (get-image-variance c)))
    (when (negative? (* (- newv oldv) dir)) (loop newv))))))

(define (camera->video-device-name n) (format #f "video~a" n))

(define (camera->serial-number n)
 (let ((output
	(system-output
	 (format #f "cd /sys/devices; cat $(echo $(find . -name ~a) | cut -d/  -f -6 -)/serial 2> /dev/null"
		 (camera->video-device-name n)))))
  (if (or (null? output) (substring? "No such" (first output)))
      #f
      (first output))))

(define (camera-s/n-*settings*-mount s/n)
 (*settings-lookup* 'camera s/n 'mount))
(define (camera-*settings*-mount c)
 (camera-s/n-*settings*-mount (camera->serial-number c)))

(define (camera-s/n-*settings*-set-mount! s/n mount)
 (unless (or (eq? mount 'head-left)
	     (eq? mount 'head-right)
	     (eq? mount 'unused)
	     (eq? mount 'palm))
  (panic "invalid mounting type"))
 (*settings-update!* mount 'camera s/n 'mount))
(define (camera-*settings*-set-mount! c mount)
 (camera-s/n-*settings*-set-mount! (camera->serial-number c) mount))

(define (camera-s/n-*settings*-set-robot! s/n robot)
 (*settings-update!* robot 'camera s/n 'robot))
(define (camera-*settings*-set-robot! c robot)
 (camera-s/n-*settings*-set-robot! (camera->serial-number c) robot))

(define (camera-s/n-*settings*-robot! s/n)
 (*settings-lookup* 'camera s/n 'robot))
(define (camera-*settings*-robot! c)
 (*settings-lookup* 'camera (camera->serial-number c) 'robot))

(define (get-cameras)
 (remove-if-not
  identity
  (map-n
   (lambda (n) (let ((s/n (camera->serial-number n)))
	   (when s/n `(,n ,s/n))))
   *maximum-number-of-cameras*)))

(define (get-mounted-cameras . mounts)
 (remove-if-not
  identity
  (map
   (lambda (c)
    (let ((mount (camera-s/n-*settings*-mount (cadr c))))
     (when (find mount mounts)
      `(,(car c) ,(cadr c) ,mount))))
   (get-cameras))))

(define (detect-robot-from-cameras)
 (let ((cs
	(remove-if-not
	 identity
	 (map (lambda (c) (camera-s/n-*settings*-robot! (cadr c)))
	      (get-cameras)))))
  (unless (null? cs) (car cs))))

(define (requested-cameras->list nr palm head-right head-left)
 (if nr
     (map-n (lambda (n) `(,n ,(camera->serial-number n) unused)) nr)
     (get-mounted-cameras (when palm 'palm)
			  (when head-right 'head-right)
			  (when head-left 'head-left))))
