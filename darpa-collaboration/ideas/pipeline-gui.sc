(MODULE
  PIPELINE-GUI
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

;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "pipeline-gui.sch")

(set! *program* "pipeline-gui")
(set! *panic?* #f)

;;; Macros
;;; Structures
;;; Variables

;;; Run examples:
;; darpa-wrap ./pipeline-gui -darpa  HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2 
;; (run (string->darpa-video "HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2"))

;; debug:
(define *debug-flg* #f)
;; drawing parameters:
(define *drawing-thickness* 2)
;; optical flow paramters:
(define *quiver-length-multiplier* 50)
(define *optical-flow-block-size-in-pixels* 16)
;; voc4 detector paramters:
(define *k-objects-per-model* 4)
;; down-sample paramters:
(define *down-sample-width* 320)
(define *down-sample-height* 180)
;; video:
(define *refresh-rate* 4)
(define *frames-to-fast-forward* 0)
(define *current-frame-index* #f)
(define *ffmpeg-video* #f)
(define *video-width* #f)
(define *video-height* #f)
(define *video-path-name* #f)
(define *video* #f)
(define *is-playing* #f)
(define *is-new-frame* #t)
(define *composite-width* (* 3 *down-sample-width*))
(define *composite-height* (* 3 *down-sample-height*))
;; panel coordinate:  TBF Could be more efficent using a list.
(define *pan1-xy* #f)
(define *pan2-xy* #f)
(define *pan3-xy* #f)
(define *pan4-xy* #f)
(define *pan5-xy* #f)
(define *pan6-xy* #f)
(define *pan7-xy* #f)
(define *pan8-xy* #f)
(define *pan9-xy* #f)
;; panel images:  TBF Could be more efficent using a list.
(define *pan1-image* #f)
(define *pan2-image* #f)
(define *pan3-image* #f)
(define *pan4-image* #f)
(define *pan5-image* #f)
(define *pan6-image* #f)
(define *pan7-image* #f)
(define *pan8-image* #f)
(define *pan9-image* #f)
(define *down-sampled-current-raw-frame* #f)
(define *current-full-raw-image* #f)
(define *composite-image* #f)
;; misc
(define *optical-flow-blocks* #f)
(define *models-available* #f)
;; simulate from data file (run manually for now)
(when #t
 (define *klt-movie* #f)
 (define *optical-flow-movie* #f)
 (define *voc4-smooth-tracked-named-boxes* #f)
 (define *voc4-tracked-named-boxes* #f)
 (define *voc4-predicted-named-boxes* #f)
 (define *voc4-detection-named-boxes* #f))

(define *colors* '(#(255 0 0) #(0 128 255) #(128 255 0) #(0 255 128)))


(define (copy-imlib-image! destination-image source-image)
 ;; Usage:  (set! d-img (copy-imlib-image! d-img s-img))
 (let* ((dummy (imlib-context-set-image! source-image))
        (width (imlib-get-image-width))
        (height (imlib-get-image-height))
        (dummy (imlib-context-set-image! destination-image)))
  (imlib-blend-image-onto-image
   source-image 0 0 0 width height 0 0 width height))
 destination-image)

(define (allocate-box-colors! num-colors)
 (when (> num-colors (length *colors*))
  (set! *colors* (append *colors* (generate-colours-n
                                   (- num-colors (length *colors*)))))))

(define (create-panel-image-data!)
 (set! *pan1-xy* '(0 0))
 (set! *pan2-xy* (list *down-sample-width* 0))
 (set! *pan3-xy* (list (* 2 *down-sample-width*) 0))
 (set! *pan4-xy* (list 0 *down-sample-height*))
 (set! *pan5-xy* (list *down-sample-width* *down-sample-height*))
 (set! *pan6-xy* (list (* 2 *down-sample-width*) *down-sample-height*))
 (set! *pan7-xy* (list 0 (* 2 *down-sample-height*)))
 (set! *pan8-xy* (list *down-sample-width* (* 2 *down-sample-height*)))
 (set! *pan9-xy* (list (* 2 *down-sample-width*) (* 2 *down-sample-height*))) 
 (set! *pan1-image* (imlib-create-image
                     *down-sample-width* *down-sample-height*))
 (set! *pan2-image* (imlib-create-image
                     *down-sample-width* *down-sample-height*))
 (set! *pan3-image* (imlib-create-image
                     *down-sample-width* *down-sample-height*))
 (set! *pan4-image* (imlib-create-image
                     *down-sample-width* *down-sample-height*))
 (set! *pan5-image* (imlib-create-image
                     *down-sample-width* *down-sample-height*))
 (set! *pan6-image* (imlib-create-image
                     *down-sample-width* *down-sample-height*))
 (set! *pan7-image* (imlib-create-image
                     *down-sample-width* *down-sample-height*))
 (set! *pan8-image* (imlib-create-image
                     *down-sample-width* *down-sample-height*))
 (set! *pan9-image* (imlib-create-image
                     *down-sample-width* *down-sample-height*))
 (set! *composite-image* (imlib-create-image
                          *composite-width* *composite-height*))
 (set! *down-sampled-current-raw-frame*
       (imlib-create-image *down-sample-width* *down-sample-height*))
 (set! *current-full-raw-image* (imlib-create-image
                                 *down-sample-width* *down-sample-height*))
 (for-each (lambda (image)
            (imlib-context-set-image! image)
            (imlib-context-set-color! 255 255 200 255) ; yellowish background
            (imlib-image-fill-rectangle
             0 0 (imlib-get-image-width) (imlib-get-image-height)))
           (list *pan1-image* *pan2-image* *pan3-image* *pan4-image*
                 *pan5-image* *pan6-image* *pan7-image* *pan8-image*
                 *pan9-image* *composite-image*)))

(define (copy-raw-image-data-to-panels!)
 ;; TBF: This function is not necessary.
 (imlib-context-set-image! *down-sampled-current-raw-frame*) (imlib-free-image)
 (imlib-context-set-image! *current-full-raw-image*)
 (set! *down-sampled-current-raw-frame*
       (imlib-create-cropped-scaled-image
        0 0  *video-width* *video-height*
        *down-sample-width* *down-sample-height*))
 (map (lambda (pan-image)
       (imlib-context-set-image! pan-image)
       (imlib-blend-image-onto-image
        *down-sampled-current-raw-frame* 0 0 0
        *down-sample-width* *down-sample-height* 0 0
        *down-sample-width* *down-sample-height*))
      (list *pan1-image* *pan2-image* *pan3-image*
            *pan4-image* *pan5-image* *pan6-image*
            *pan7-image* *pan8-image* *pan9-image*)))

(define (update-current-raw-image-data!)
 (imlib-context-set-image! *current-full-raw-image*) (imlib-free-image)
 (set! *current-full-raw-image* (ffmpeg-video-frame-data-as-imlib
                                 *ffmpeg-video*)))

(define (draw-panels-to-composite-image!)
 (imlib-context-set-image! *composite-image*)
 (map (lambda (pan-image pan-xy)
       (imlib-blend-image-onto-image
        pan-image 0 0 0 *down-sample-width* *down-sample-height*
        (first pan-xy) (second pan-xy)
        *down-sample-width* *down-sample-height*))
      (list *pan1-image* *pan2-image* *pan3-image*
            *pan4-image* *pan5-image* *pan6-image*
            *pan7-image* *pan8-image* *pan9-image*)
      (list *pan1-xy* *pan2-xy* *pan3-xy*
            *pan4-xy* *pan5-xy* *pan6-xy*
            *pan7-xy* *pan8-xy* *pan9-xy*)))

(define (down-sample-image-and-destroy full-image)
 ;; Convert the full image to down-sampled result.  Note: The original
 ;; full-image is destroyed.
 (imlib-context-set-image! full-image)
 (set! down-sampled-image (imlib-create-cropped-scaled-image
			   0 0 *video-width* *video-height*
			   *down-sample-width* *down-sample-height*))
 (imlib-free-image) down-sampled-image)

(define (at-most-first-k-of-each l k)
 (map (lambda (e)
       (if (list? e)
	   (if (<= (length e) k) e (sublist e 0 k))
	   (fuck-up))) l))

(define (draw-named-boxes-at-most-k-per-model
	 named-boxes video-name image index
	 thickness name? colours parts? top-k)
 ;; It looks like the named-boxes is already sorted by the strength of
 ;; detection.  We just pick thte top k object of each model
 (imlib-context-set-image! image)
 (draw-on-image-from-boxes-details
  video-name image thickness
  (map first named-boxes)
  (at-most-first-k-of-each
   (map (lambda (a) (list-ref (second a) index)) named-boxes) top-k)
  colours name?
  50 parts?))

(define (draw-named-boxes named-boxes video-name image index
			  thickness name? colours parts?)
 (imlib-context-set-image! image)
 (draw-on-image-from-boxes-details
  video-name image thickness
  (map first named-boxes)
  (map (lambda (a) (list-ref (second a) index)) named-boxes)
  colours name?
  50 parts?))

(define (overlay-raw-result!)
 (imlib-context-set-image! *current-full-raw-image*)
 (set! full-image (imlib-clone-image))
 (imlib-context-set-image! *pan1-image*) (imlib-free-image)
 (set! *pan1-image* (down-sample-image-and-destroy full-image)))
 
(define (overlay-klt-result!)
 (unless (or (= 0 *current-frame-index*)
             (> *current-frame-index* (length *klt-movie*)))
  (let ((current-klt-result
         (list-ref *klt-movie* (- *current-frame-index* 1))))
   (imlib-context-set-image! *current-full-raw-image*)
   (set! full-image (imlib-clone-image))
   (for-each (lambda (klt-pair)
              (let* ((pt1 (klt-pair-current klt-pair))
                     (pt2 (klt-pair-next klt-pair)))
               (unless (and (= (x pt1) (x pt2)) (= (y pt1) (y pt2)))
                (draw-imlib-line full-image pt1 pt2
				 (+ 2 *drawing-thickness*)
                                 (vector 0 255 0)))
               (draw-imlib-circle full-image pt2 *drawing-thickness*
                                  (vector 255 0 0) #t)))
             current-klt-result)
   (imlib-context-set-image! *pan2-image*) (imlib-free-image)
   (set! *pan2-image* (down-sample-image-and-destroy full-image)))))

(define (overlay-optical-flow-result!)
 (unless (or (= 0 *current-frame-index*)
         (> *current-frame-index* (length *optical-flow-movie*)))
  (let ((current-optical-flow-result
         (list-ref *optical-flow-movie* (- *current-frame-index* 1))))
   (imlib-context-set-image! *current-full-raw-image*)
   (set! full-image (imlib-clone-image))
   (let* ((flow-center-dir-pairs
           (map (lambda (rectangle)
                 (let* ((x0 (first rectangle)) (y0 (second rectangle))
                        (x1 (third rectangle)) (y1 (fourth rectangle)))
                  (list (vector (list-mean (list x0 x1))
                                (list-mean (list y0 y1)))
                        (average-integral-optical-flow-in-c
                         current-optical-flow-result
                         x0 y0 x1 y1)))) *optical-flow-blocks*)))
    (for-each (lambda (center-dir-pair)
               (let* ((center (first center-dir-pair))
                      (v-direction (second center-dir-pair))
                      (v-magnitude (magnitude v-direction))
                      (quiver
                       (vector (* *quiver-length-multiplier* (x v-direction))
                               (* *quiver-length-multiplier* (y v-direction))))
                      (shifted-center 
                       (vector (+ (x center) (x quiver))
                               (+ (y center) (y quiver)))))
                (draw-imlib-line full-image center shifted-center
                                 (+ 2 *drawing-thickness*) (vector 0 255 0))
		;; (draw-imlib-circle full-image center
                ;;                    *drawing-thickness* (vector 255 0 0) #t)
		))
              flow-center-dir-pairs))
   (imlib-context-set-image! *pan3-image*) (imlib-free-image)
   (set! *pan3-image* (down-sample-image-and-destroy full-image)))))

(define (overlay-top-k-voc4-detection-results! named-boxes pan-image)
 (unless (or (null? named-boxes)
	     (>= *current-frame-index*
		 (length (second (first named-boxes)))))
  (allocate-box-colors! (length named-boxes))
  (imlib-context-set-image! *current-full-raw-image*)
  (set! full-image (imlib-clone-image))
  (draw-named-boxes-at-most-k-per-model
   named-boxes
   *video* full-image *current-frame-index*
   (+ 2 *drawing-thickness*) #f *colors* #f *k-objects-per-model*)
  (draw-legend-on-image full-image
			(map second *models-available*)
			*colors*
			30)
  (imlib-context-set-image! pan-image) (imlib-free-image)
  (set! pan-image (down-sample-image-and-destroy full-image))))

(define (overlay-voc4-detector-result!)
 (overlay-top-k-voc4-detection-results! *voc4-detection-named-boxes*
					*pan4-image*))

(define (overlay-voc4-predicted-result!)
 (overlay-top-k-voc4-detection-results! *voc4-predicted-named-boxes*
					*pan5-image*))

(define (overlay-voc4-detection-results! named-boxes pan-image)
 (unless (or (null? named-boxes)
	     (>= *current-frame-index*
		 (length (second (first named-boxes)))))
  (allocate-box-colors! (length named-boxes))
  (imlib-context-set-image! *current-full-raw-image*)
  (set! full-image (imlib-clone-image))
  (draw-named-boxes named-boxes
		    *video* full-image *current-frame-index*
		    (+ 2 *drawing-thickness*) #f *colors* #f)
  (draw-legend-on-image full-image
  			(map second *models-available*)
  			*colors*
  			30)
  (imlib-context-set-image! pan-image) (imlib-free-image)
  (set! pan-image (down-sample-image-and-destroy full-image))))

(define (overlay-voc4-tracked-result!)
 (overlay-voc4-detection-results! *voc4-tracked-named-boxes*
				  *pan7-image*))

(define (overlay-voc4-smooth-tracked-result!)
 (overlay-voc4-detection-results! *voc4-smooth-tracked-named-boxes*
				  *pan8-image*))


(define (do-overlays!)
 (overlay-raw-result!)
 (overlay-klt-result!)
 (overlay-optical-flow-result!)
 (overlay-voc4-detector-result!)
 (overlay-voc4-predicted-result!)
 (overlay-voc4-tracked-result!)
 (overlay-voc4-smooth-tracked-result!))

(define (next-frame-callback)
 (if (ffmpeg-next-frame! *ffmpeg-video*)
     (begin
      (set! *is-new-frame* #t)
      (set! *current-frame-index* (+ 1 *current-frame-index*))
      #t)
     (begin
      (message "No more video frames")
      (set! *is-new-frame* #f)
      #f)))

(define (save-composite-image-to-file!)
 (imlib-context-set-image! *composite-image*)
 (imlib-image-set-format "png")
 (let ((filename (format #f "debug-frames/~a.png"
                         (number->padded-string-of-length
                          *current-frame-index* 6))))
  (display "Save png file to ") (write filename) (newline)
  (rm-if-necessary filename)
  (system (format #f "mkdir -p \"$(dirname \"~a\")\"" filename))
  (imlib-save-image filename)))

(define-application viewer 960 (+ 540 20) 0 1 6
 (lambda ()                             ; pre-initialize-procerue
  (setup-extra-x-gcs)
  (standard-buttons 6 (lambda () (message (format "calling help"))))
  (define-button 1 0 "Next frame" #f
   (lambda ()
    (next-frame-callback)
    (redraw-display-pane)))
  (define-button 2 0 "Play" #f
   (lambda ()
    (set! *is-playing* #t)
    (pipeline-gui-startup-with-viewfinder
     (cdr *display*) *screen* *display-pane* 0 0
     *display-pane-width* *display-pane-height* *refresh-rate*)))
  (define-button 3 0 "Stop" #f
   (lambda ()
    (pipeline-gui-shutdown)
    (set! *is-playing* #f)
    (redraw-display-pane)))
  (set! *clear-display-pane?* #f))
 (lambda ()                             ; post-initialize-procerue
  #f)
 (lambda () (free-extra-x-gcs))         ; finalize-procedure
 (lambda ()                             ; redraw-procedure
  (xremove-expose-events)
  (when *is-playing* (next-frame-callback))
  (when *is-new-frame*
   (update-current-raw-image-data!)
   (copy-raw-image-data-to-panels!)
   (do-overlays!)
   (draw-panels-to-composite-image!)
   (if *debug-flg*
       (save-composite-image-to-file!))
   (display (format "done processing new frame: index = ~a~%"
		    *current-frame-index*)))
  (draw-imlib-pixmap *composite-image* 0 0)
  (set! *is-new-frame* #f))
 (lambda ()                             ; listener-procedure
  #f))

(define (fast-forward-video! frames)
 (let loop ((i 0))
  (and (< i frames)
       (or (not (next-frame-callback)) (loop (+ i 1))))))

(define (open-video! video)
 (set! *video* video)
 (set! *video-path-name* (video-pathname video))
 (set! *ffmpeg-video* (ffmpeg-open-video *video-path-name*))
 (when (= *ffmpeg-video* 0)
  (display (video-pathname video)) (newline)
  (panic "Couldn't open ffmpeg video"))
 (display (format "Opened video ~s" (video-pathname video))) (newline)
 (let ((dimensions (video-dimensions video)))
  (set! *video-width* (x dimensions))
  (set! *video-height* (y dimensions)))
 (set! *current-frame-index* 0)
 (fast-forward-video! *frames-to-fast-forward*))

(define (close-vidoe!)
 (display (format "Closed video ~s" *video-path-name*)) (newline)
 (ffmpeg-close-video *ffmpeg-video*)
 (set! *ffmpeg-video* #f)
 (set! *current-frame-index* #f))

(define (read-smooth-tracked-named-boxes video-name)
 (map (lambda (voc4-avail)
       (let ((model-name (second voc4-avail))
 	     (id-number (third voc4-avail)))
 	(list model-name (read-voc4-smooth-tracked-boxes
 			  video-name model-name id-number))))
      (video-voc4-smooth-tracked-boxes-available video-name)))

(define (read-tracked-named-boxes video-name)
 (map (lambda (voc4-avail)
       (let ((model-name (second voc4-avail))
 	     (id-number (third voc4-avail)))
 	(list model-name (read-voc4-tracked-boxes
 			  video-name model-name id-number))))
      (video-voc4-tracked-boxes-available video-name)))

(define (read-detection-named-boxes video-name)
 (map (lambda (voc4-avail)
       (let ((model-name (second voc4-avail)))
 	(list model-name (read-voc4-detector-boxes
 			  video-name model-name))))
      (video-voc4-detector-boxes-available video-name)))

(define (read-predicted-named-boxes video-name)
 (map (lambda (voc4-avail)
       (let ((model-name (second voc4-avail)))
 	(list model-name (read-voc4-predicted-boxes
			  video-name model-name))))
      (video-voc4-predicted-boxes-available video-name)))

(define (setup-and-initialize!)
 (imlib-context-disconnect-display)
 (create-panel-image-data!)
  ;; simulate from data file for now:
 (open-video! *video*)
 (when #t
  (set! *klt-movie* (read-klt-movie *video*))
  (set! *optical-flow-movie* (read-optical-flow-movie-in-c *video*))
  (set! *voc4-smooth-tracked-named-boxes*
        (read-smooth-tracked-named-boxes *video*))
  (set! *voc4-tracked-named-boxes*
	(read-tracked-named-boxes *video*))
  (set! *voc4-predicted-named-boxes*
	(read-predicted-named-boxes *video*))  
  (set! *voc4-detection-named-boxes* (read-detection-named-boxes *video*))
  #f)
 ;; pre-compute optical-flow blocks
 (set! *optical-flow-blocks*
       (chop-rectangle-by-block-size
	(list 0 0 (- *video-width* 1) (- *video-height* 1))
	*optical-flow-block-size-in-pixels*
	*optical-flow-block-size-in-pixels*))
 ;; pre-compute the model availables:
 (set! *models-available*
       (video-voc4-detector-boxes-available *video*)))

(define (clean-ups!)
 (pipeline-gui-shutdown)
 (close-vidoe!))

(define (run video)
 (set! *video* video)
 (setup-and-initialize!)
 (viewer '()) ; start the GUI
 (clean-ups!))
 
(define-command
 (main (exactly-one
        ("standard" standard?
         (corpus "corpus" string-argument "")
         (sequence "sequence" string-argument "")
         (person "person" string-argument "")
         (location "location" string-argument "")
         (n "n" integer-argument 0))
        ("darpa" darpa? (name "name" string-argument ""))
        ("stand-alone" stand-alone? (path "path" string-argument ""))
        ("demo" demo? (demo-name "name" string-argument ""))))
 (let* ((video
         (cond (standard?
                (standard-corpus-video corpus sequence person location n))
               (darpa? (string->darpa-video name))
               (stand-alone? (make-stand-alone-video path))
               (demo? (string->demo-video demo-name))
               (else (fuck-up)))))
  (run video)))

;(run (string->darpa-video "HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2"))
