(MODULE
 VIDEO-TO-SENTENCES
 (WITH
  QOBISCHEME
  XLIB
  CUPEDRO-BINDINGS
  HMM-TRAIN-CLASSIFY
  HMM-WBM
  IDEALIB-HASH-TABLE
  IDEALIB-MATPLOTLIB
  IDEALIB-PREGEXP
  IDEALIB-TRACKS
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
;;; Copyright 2011 Purdue University. All rights reserved.

;; darpa-wrap ./video-to-sentences -m person -t 5 -cuda-object-detector -0.25 0 -cuda-optical-flow -cuda-klt -darpa Chase1_A1_C1_Act1_4_PARK1_ML_MIDD_DARK_4433d840-c5af-11df-bed3-e80688cb869a -event-models-file /home/abarbu/event-hmm/event-models.sc /home/abarbu/video-datasets/C-D1/voc4-models/ foo_v2.mov
;; (main '("XXX" "-gui" "-m" "person,person-down" "-m" "car" "-m" "suv" "-cuda-object-detector" "-0.1" "-0.5" "1" "-cuda-klt" "-cuda-optical-flow" "-event-models-file" "/home/abarbu/event-hmm/event-models.text" "-model-path" "/net/upplysingaoflun/aux/qobi/video-datasets/C-D1/voc4-models" ))
;; (main '("XXX" "-downsample" "320" "180" "10" "-gui" "-m" "person,person-down" "-m" "car" "-m" "suv" "-cuda-object-detector" "-0.1" "-0.5" "1" "-cuda-klt" "-cuda-optical-flow" "-event-models-file" "/home/abarbu/event-hmm/event-models.text" "-model-path" "/net/upplysingaoflun/aux/qobi/video-datasets/C-D1/voc4-models" "foo_v2"))

(include "QobiScheme.sch")
(include "video-to-sentences.sch")
(include "toollib-c-macros.sch")
(include "toollib-c-externals.sch")
(include "toollib-controls.sch")

(set! *program* "video-to-sentences")
(set! *panic?* #f)

;;; Macros
;;; Structures

;;; Parameters
;; Background task lists
(define *background-task-enablers* '())
(define *background-task-disablers* '())

;; These are used by both the command line and GUI versions:
(define *minimum-track-length* 10)
(define *maximum-track-overlap-ratio* 0.7)
(define *model-threshold-tracker-offset* -0.4)
(define *overgeneration-minimum-track-length* 0)
(define *overgeneration-maximum-track-overlap-ratio* 0.1)
(define *profile-best-boxes?* #f)

;;; Variables
;; These variables are use by the original command line version only:
(define *max-sentences* 20)

;;; Variables
;; These variables are use by the GUI version only:
(define *debug-flg* #f)
(define *verbose-count* 0)
(define *pane-width* 320)
(define *pane-height* 180)
(define *caption-height* 20)
(define *frame-number* #f)
(define *stopped-frame* #f)
(define *played-from-frame* #f)
(define *video* #f)
(define *ffmpeg-video* #f)
(define *video-width* #f)
(define *video-height* #f)
(define *video-length* #f)
(define *video-fps* #f)
(define *cuda-device* #f)
(define *imlib-image* #f)
(define *preview-snapshot* #f)
(define *video-name-caption* #f)
(define *help-activated* #f)
(define *processing-state* #f)
(define *tracked-movies-names* '())
(define *predicted-boxes-movies* #f)
(define *tracked-boxes-movies-groups* #f)
(define *smooth-tracked-boxes-movies-groups* #f)
(define *top-n-sentences* 3)
(define *likelihoods* #f)
(define *sentences* #f)
(define *likelihoods-histogram-image* #f)
(define *started-processing?* #f)
(define *finished-processing?* #f)
(define *stop-requested* #f)
(define *premature-stop?* #f)
(define *colors* '(#(255 0 0) #(0 128 255) #(128 255 0) #(0 255 128)))
(define *viewfinder-visible* #f)
(define *capture-mode?* #f)
(define *n-seconds-to-capture* 4)
(define *default-capture-width* 640)
(define *default-capture-height* 480)
(define *default-capture-fps* 10)
(define *save-captured-video-filename-index* 0)
(define *captured-video-name* #f)
(define *adjusting-camera?* #f)
(define (*model-names*) (join (model-groups)))
;; possible modes: klt, optical-flow, mixed (traditional default)
(define *forward-projection-mode* 'optical-flow)
(define *on-exit-thunks* '())
(define *fsm-start-time* #f)
(define *auto-usleep-start-time* #f)

(define *min-flow* #f)

(define (remove-box-toy-demo? box flow)
 (cond ((equal? (voc4-detection-model box) "person")
	(or (<= (voc4-detection-height box) (/ *video-height* 2))
	    (not (<= 0.1 (voc4-detection-aspect-ratio box) 0.75))))
       ((equal? (voc4-detection-model box) "gun")
	(not (<= 1.7 (voc4-detection-aspect-ratio box) 3.5)))
       ((equal? (voc4-detection-model box) "ball")
	(not (<= 0.8 (voc4-detection-aspect-ratio box) 1.2)))
       ((equal? (voc4-detection-model box) "sign")
	(not (<= 0.8 (voc4-detection-aspect-ratio box) 1.2)))
       ((equal? (voc4-detection-model box) "giraffe")
	(or (not (<= 0.8 (voc4-detection-aspect-ratio box) 1.2))
	    (let ((c (voc4-detection-center box)))
	     (and (< (y c) 100)
		  (or (< (x c) 100) (> (x c) 500))))))
       (else #f)))

;;; Timing

(define-structure pipeline-timing
 all flow klt detector tracker
 saving write-videos
 draw-likelihoods likelihoods sentences)

(define (create-empty-pipeline-timings)
 (make-pipeline-timing 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))

(define *timings* (create-empty-pipeline-timings))

(define (increment-timing getter setter! seconds)
 (unless (getter *timings*) (setter! *timings* 0.0))
 (setter! *timings* (+ (getter *timings*) seconds)))

(define (increment-all-timing seconds)
 (increment-timing pipeline-timing-all set-pipeline-timing-all! seconds))

(define (increment-flow-timing seconds)
 (increment-timing pipeline-timing-flow set-pipeline-timing-flow! seconds))

(define (increment-klt-timing seconds)
 (increment-timing pipeline-timing-klt set-pipeline-timing-klt! seconds))

(define (increment-detector-timing seconds)
 (increment-timing pipeline-timing-detector
		   set-pipeline-timing-detector!
		   seconds))

(define (increment-tracker-timing seconds)
 (increment-timing pipeline-timing-tracker
		   set-pipeline-timing-tracker!
		   seconds))

(define (increment-saving-timing seconds)
 (increment-timing pipeline-timing-saving
		   set-pipeline-timing-saving!
		   seconds))

(define (increment-write-videos-timing seconds)
 (increment-timing pipeline-timing-write-videos
		   set-pipeline-timing-write-videos!
		   seconds))

(define (increment-likelihoods-timing seconds)
 (increment-timing pipeline-timing-likelihoods
		   set-pipeline-timing-likelihoods!
		   seconds))

(define (increment-draw-likelihoods-timing seconds)
 (increment-timing pipeline-timing-draw-likelihoods
		   set-pipeline-timing-draw-likelihoods!
		   seconds))

(define (increment-sentences-timing seconds)
 (increment-timing pipeline-timing-sentences
		   set-pipeline-timing-sentences!
		   seconds))

(define (pipeline-timing->pretty-string timings)
 (define (format-line getter)
  (if (getter timings)
      (number->string-of-length-and-precision (getter timings) 8 3)
      "?"))
 (apply
  string-append
  (list
   (format #f "Optical flow:            ~a~%" (format-line pipeline-timing-flow))
   (format #f "KLT:                     ~a~%" (format-line pipeline-timing-klt))
   (format #f "Detector:                ~a~%" (format-line pipeline-timing-detector))
   (format #f "Tracker:                 ~a~%" (format-line pipeline-timing-tracker))
   (format #f "Saving boxes/flow/KLT:   ~a~%" (format-line pipeline-timing-saving))
   (format #f "Computing likelihoods:   ~a~%" (format-line pipeline-timing-likelihoods))
   (format #f "Drawing likelihoods:     ~a~%" (format-line pipeline-timing-draw-likelihoods))
   (format #f "Generating sentences:    ~a~%" (format-line pipeline-timing-sentences))
   (format #f "Rendering videos:        ~a~%" (format-line pipeline-timing-write-videos))
   (format #f "---------------------------------~%")
   (format #f "All:                     ~a~%" (format-line pipeline-timing-all)))))

;;; Global options

(define-structure global-option
 gui?
 alpha beta lookahead
 model-groups with-dt?
 model-path
 event-models-file
 event-models
 medoids
 in-place? video-path darpa?
 downsample?
 downsample-width downsample-height downsample-fps
 final-cascade-adjustment threshold-offset nms
 top-n max-detections
 klt-choice optical-flow-choice detector-choice
 write-object-detector? write-klt?
 write-optical-flow? write-tracker?
 read-tracker? stop-before-tracker? stop-after-tracker?
 rank-box-colors? video-destination-directory
 suppression-delta
 max-tracks
 toy-demo?)

(define (create-default-global-options)
 (make-global-option #t ; gui?
                     10 15 2 ; Alpha beta lookahead
                     '() #f ; model-groups with-dt?
                     "/net/upplysingaoflun/aux/qobi/video-datasets/C-D1/voc4-models" ; model-path
                     "/home/abarbu/event-hmm/event-models.text" ; event-models-file
		     #f ; event-models
                     #f ; medoids
                     #f #f #f ; in-place, video-path darpa?
                     #f ; downsample?
                     0 0 0 ;  downsample: width height fps
                     ;; These parameters are used by Siddharth, we make them
                     ;; the default now:
                     -1.0 0 0.7 ; final-cascade, threshold, nms
                     20 100000 ; max-detections
                     "cuda" "cuda" "cuda" ; klt-choice, flow-choice, detector-choice
                     #f #f ; write-object-detector write-klt
                     #f #f ; write-flow write-tracker
		     #f #f #f ; read-tracker? stop-before-tracker? stop-after-tracker?
		     #f ; rank-box-colors?
		     #f ; video-destination-directory
		     0.1 ; suppression delta
		     2 ; max tracks
		     #f)) ; toy demo?

(define *parameters* (create-default-global-options))

(define *cmd-parameters* #f)

(define (global-options->pretty-string options)
 (apply
  string-append
  (list
   (format #f "Gui:                          ~a~%" (global-option-gui? options))
   (format #f "Alpha:                        ~a~%" (global-option-alpha options))
   (format #f "Beta:                         ~a~%" (global-option-beta options))
   (format #f "Lookahead:                    ~a~%" (global-option-lookahead options))
   (format #f "Model-groups:                 ~a~%" (global-option-model-groups options))
   (format #f "With-dt:                      ~a~%" (global-option-with-dt? options))
   (format #f "Model-path:                   ~a~%" (global-option-model-path options))
   (format #f "Event-model-path:             ~a~%" (global-option-event-models-file options))
   (format #f "In-place:                     ~a~%" (global-option-in-place? options))
   (format #f "video-path:                   ~a~%" (global-option-video-path options))
   (format #f "Darpa:                        ~a~%" (global-option-darpa? options))
   (format #f "Downsample:                   ~a~%"
	   (if (global-option-downsample? options)
	       (format #f "~ax~a@~a"
		       (global-option-downsample-width options)
		       (global-option-downsample-height options)
		       (global-option-downsample-fps options))
	       "#F"))
   (format #f "Fincal-cascade-threshold: ~a~%" (global-option-final-cascade-adjustment options))
   (format #f "Threshold-adjustment:     ~a~%" (global-option-threshold-offset options))
   (format #f "Non-maximal-suppression:  ~a~%" (global-option-nms options))
   (format #f "Top-n:                    ~a~%" (global-option-top-n options))
   (format #f "Max-detections:           ~a~%" (global-option-max-detections options))
   (format #f "Klt-choice:               ~a~%" (global-option-klt-choice options))
   (format #f "Optical-flow-choice:      ~a~%" (global-option-optical-flow-choice options))
   (format #f "Detector-choice:          ~a~%" (global-option-detector-choice options))
   (format #f "Write-object-detector:    ~a~%" (global-option-write-object-detector? options))
   (format #f "Write-klt:                ~a~%" (global-option-write-klt? options))
   (format #f "Write-optical-flow:       ~a~%" (global-option-write-optical-flow? options))
   (format #f "Write-tracker:            ~a~%" (global-option-write-tracker? options))
   (format #f "Read-Tracker:             ~a~%" (global-option-read-tracker? options))
   (format #f "Stop-before-tracker:      ~a~%" (global-option-stop-before-tracker? options))
   (format #f "Stop-after-tracker:       ~a~%" (global-option-stop-after-tracker? options))
   (format #f "Rank-box-colors:          ~a~%" (global-option-rank-box-colors? options))
   (format #f "Video-destination-directory:  ~a~%" (global-option-video-destination-directory options))
   (format #f "Suppression delta:          ~a~%" (global-option-suppression-delta options))
   (format #f "Toy-demo:          ~a~%" (global-option-toy-demo? options)))))

(define (gui?) (global-option-gui? *parameters*))
(define (alpha) (global-option-alpha *parameters*))
(define (beta) (global-option-beta *parameters*))
(define (lookahead) (global-option-lookahead *parameters*))
(define (model-groups) (global-option-model-groups *parameters*))
(define (with-dt?) (global-option-with-dt? *parameters*))
(define (model-path) (global-option-model-path *parameters*))
(define (event-models-file) (global-option-event-models-file *parameters*))
(define (event-models) (global-option-event-models *parameters*))
(define (medoids) (global-option-medoids *parameters*))
(define (option-in-place?) (global-option-in-place? *parameters*))
(define (video-path) (global-option-video-path *parameters*))
(define (darpa?) (global-option-darpa? *parameters*))
(define (downsample?) (global-option-downsample? *parameters*))
(define (downsample-width) (global-option-downsample-width *parameters*))
(define (downsample-height) (global-option-downsample-height *parameters*))
(define (downsample-fps) (global-option-downsample-fps *parameters*))
(define (final-cascade-adjustment) (global-option-final-cascade-adjustment *parameters*))
(define (threshold-offset) (global-option-threshold-offset *parameters*))
(define (nms) (global-option-nms *parameters*))
(define (max-detections) (global-option-max-detections *parameters*))
(define (top-n) (global-option-top-n *parameters*))
(define (klt-choice) (global-option-klt-choice *parameters*))
(define (optical-flow-choice) (global-option-optical-flow-choice *parameters*))
(define (detector-choice) (global-option-detector-choice *parameters*))
(define (write-object-detector?) (global-option-write-object-detector? *parameters*))
(define (write-klt?) (global-option-write-klt? *parameters*))
(define (write-optical-flow?) (global-option-write-optical-flow? *parameters*))
(define (write-tracker?) (global-option-write-tracker? *parameters*))
(define (read-tracker?) (global-option-read-tracker? *parameters*))
(define (stop-before-tracker?) (global-option-stop-before-tracker? *parameters*))
(define (stop-after-tracker?) (global-option-stop-after-tracker? *parameters*))
(define (rank-box-colors?) (global-option-rank-box-colors? *parameters*))
(define (suppression-delta) (global-option-suppression-delta *parameters*))
(define (max-tracks) (global-option-max-tracks *parameters*))
(define (toy-demo?) (global-option-toy-demo? *parameters*))
(define (video-destination-directory) (global-option-video-destination-directory *parameters*))

;;; Procedures

;;; General stuff

;;; I/O

;;; Commands

;;; Top Level

(define (verbose?) (not *quiet-mode?*))

(define (debug-message! text)
 (when (verbose?)
  (format #t "[~a]: ~a~%" *verbose-count* text)
  (set! *verbose-count* (+ 1 *verbose-count*))))

(define (user-feedback! text)
 (when (and (not (string=? "" text))
	    (or (not (gui?)) (verbose?)))
  (format #t "~a~%" text))
 (when *display* (message text)))

(define (user-feedback-with-gui-flag! text gui?)
 (when (and (not (string=? "" text))
	    (or (not gui?) (verbose?)))
  (format #t "~a~%" text))
 (when *display* (message text)))

(define (user-warning! text)
 (user-feedback! text)
 (abort))

(define (niffty-bell micro-seconds-delay)
 ;; Plays a simple bell, and then sleeps for the specified number of
 ;; micro-seconds
 (when #f (system (format #f
			  "mplayer ~a/darpa-collaboration/ideas/KDE_Beep_ClassicBeep.wav >/dev/null 2>&1"
			  (getenv "HOME"))))
 (system "xkbbell")
 (interruptible-usleep micro-seconds-delay))

(define (scale)
 ;; Scale == 0.5 implies that the video is 640 pixels wide
 (/ *video-width* 1280))

;;; Background tasks

(define (add-background-task-enabler! thunk)
 (set! *background-task-enablers* (cons thunk *background-task-enablers*)))

(define (remove-background-task-enabler! thunk)
 (set! *background-task-enablers* (remove thunk *background-task-enablers*)))

(define (add-background-task-disabler! thunk)
 (set! *background-task-disablers* (cons thunk *background-task-disablers*)))

(define (remove-background-task-disabler! thunk)
 (set! *background-task-disablers* (remove thunk *background-task-disablers*)))

(define (initialize-background-task-enabler!)
 (set-background-task-enabler!
  (lambda ()
   (for-each
    (lambda (thunk) (thunk))
    *background-task-enablers*))))

(define (initialize-background-task-disabler!)
 (set-background-task-disabler!
  (lambda ()
   (for-each
    (lambda (thunk) (thunk))
    *background-task-disablers*))))

;;; On Exit Thunks

(define (execute-on-exit-thunks!)
 (map (lambda (on-exit-thunk) (on-exit-thunk)) *on-exit-thunks*)
 #f)

(define (add-on-exit-thunk! thunk)
 (set! *on-exit-thunks* (cons thunk *on-exit-thunks*)))

;;; GUI operation

(define (process-running?)
 ;; Process has not been started; or has finished
 *started-processing?*)

(define (process-finished?)
 ;; Process has been completed successfully for the entire video
 *finished-processing?*)

(define (process-running-or-ran?)
 ;; Process is running or been ran since the video was loaded
 (or (process-finished?) (process-running?)))

(define (video-selected?) *video*)

(define (video-opened?) (not (equal? #f *video-length*)))

(define (hide-stop-button-on-processing-states?)
 (or (equal? *processing-state* 'start)
     (equal? *processing-state* 'sentences)
     (equal? *processing-state* 'likelihoods)
     (vector? *processing-state*)))

(define (has-data-to-navigate?)
 (or *stop-requested*
     *premature-stop?*
     (process-finished?)))

(define (set-no-data-to-navigate!)
 (set! *stop-requested* #f)
 (set! *premature-stop?* #f)
 (set! *finished-processing?* #f))

(define (has-only-video-data?)
 ;; no processed data
 (and (not (equal? *preview-snapshot* #f))
      (not (process-running-or-ran?))))

(define (action-not-availiable)
 (when (verbose?) (user-warning! "Invalid operation")))

(define (clear-transcript-pane!)
 ;; belongs in QobiScheme
 (set! *transcript* '())
 (redraw-transcript-pane))

(define (save-composite-image-to-file)
 (imlib-context-set-image! *imlib-image*)
 (imlib-image-set-format "png")
 (let ((filename (format #f "debug-frames/~a.png"
                         (number->padded-string-of-length
                          *frame-number* 6))))
  (display "Save png file to ") (write filename) (newline)
  (rm-if-necessary filename)
  (system (format #f "mkdir -p \"$(dirname \"~a\")\"" filename))
  (imlib-save-image filename)))

(define (video->image video frame)
 (let ((ffmpeg-video (ffmpeg-open-video (video-pathname video))))
  (let loop ((i 0))
   (and (< i frame) ; frame 1 means we want 2nd image
        (or (not (ffmpeg-next-frame! ffmpeg-video)) (loop (+ i 1)))))
  (let ((image (ffmpeg-video-frame-data-as-imlib ffmpeg-video)))
   (ffmpeg-close-video ffmpeg-video)
   image)))

(define (save-img-to-file image idx)
 (imlib-context-set-image! image)
 (imlib-image-set-format "png")
 (let ((filename (format #f "debug-frames/~a.png"
                         (number->padded-string-of-length
                          idx 6))))
  (display "Save png file to ") (write filename) (newline)
  (rm-if-necessary filename)
  (system (format #f "mkdir -p \"$(dirname \"~a\")\"" filename))
  (imlib-save-image filename)) #f)

(define (extract-all-frames)
 (when *ffmpeg-video* (ffmpeg-close-video *ffmpeg-video*))
 (set! *ffmpeg-video* (ffmpeg-open-video (video-pathname *video*)))
 (let loop ((i 0))
  (format #t "i is ~a~%" i)
  (and (< i 1000)
       (or (save-img-to-file
            (ffmpeg-video-frame-data-as-imlib *ffmpeg-video*) i)
	   (not (ffmpeg-next-frame! *ffmpeg-video*))
	   (loop (+ i 1))))))

(define (copy-imlib-image! destination-image source-image)
 ;; Usage:  (set! d-img (copy-imlib-image! d-img s-img))
 (imlib-context-set-image! source-image)
 (let* ((width (imlib-get-image-width))
        (height (imlib-get-image-height)))
  (imlib-context-set-image! destination-image)
  (imlib-blend-image-onto-image
   source-image 0 0 0 width height 0 0 width height))
 destination-image)

(define (allocate-box-colors! num-colors)
 (when (> num-colors (length *colors*))
  (set! *colors* (append *colors* (generate-colours-n
                                   (- num-colors (length *colors*)))))))

(define (clear-composite-image-with-snapshot-and-caption!)
 (debug-message! (format #f "**** calling clear-composite-image-..."))
 (when *imlib-image*
  (imlib-context-set-image! *imlib-image*)
  (imlib-context-set-color! 255 255 255 255)
  (imlib-image-fill-rectangle
   0 0 (imlib-get-image-width) (imlib-get-image-height)))
 (unless *help-activated*
  (when (not (viewfinder-running?))
   (when *preview-snapshot*
    (imlib-context-set-image! *imlib-image*)
    (imlib-blend-image-onto-image
     *preview-snapshot* 0 0 0 *pane-width* *pane-height* 0 0
     *pane-width* *pane-height*))
   (when (and *video* (equal? *processing-state* 'done))
    (update-pixmaps!))
   (when *video-name-caption*
    (imlib-context-set-image! *imlib-image*)
    (imlib-draw-text-on-image
     *imlib-image* *video-name-caption*
     '#(0 0 0)
     (/ *caption-height* 2)		     ; font size
     20					     ; x coord of upper left
     (- (imlib-get-image-height) 20) #f))))) ; y coord


(define (set-preview-snapshot!)
 (if (not *video*) #f
     (begin
      (when *preview-snapshot*
       (imlib-context-set-image! *preview-snapshot*)
       (imlib-free-image-and-decache))
      (let ((frame0 (video->image *video* 0)))
       (set! *preview-snapshot*
             (create-padded-image frame0 *pane-width* *pane-height*))
       (imlib-context-set-image! frame0)
       (imlib-free-image-and-decache))
      #t)))

(define (get-video-length video)
 (when (equal? #f video)
  (fuck-up))
 (+ -1 (video-length video)))

(define (clear-preview-snapshot!)
 (when *preview-snapshot*
  (imlib-context-set-image! *preview-snapshot*)
  (imlib-free-image-and-decache)
  (set! *preview-snapshot* #f)))

(define (set-video-name-caption! caption)
 (set! *video-name-caption* caption))

(define (clear-video-name-caption!)
 (set! *video-name-caption* #f))

(define (clean-up-tmp-video video-name)
 (let ((clean-up-cmd
        (format #f "rm -rf \"/tmp/$(basename \"~a\" .avi)\"*" video-name)))
  (user-feedback! (format #f "Executing: ~a" clean-up-cmd))
  (system clean-up-cmd)))

(define (clean-up-track-and-caption-video-temporaries video)
 (let ((clean-up-cmd
        (format #f "rm -rf ~a " (generic-root-pathname video (format #f "/*/~a.png" "tracked"))))
       (clean-up-cmd2
        (format #f "rm -rf ~a " (generic-root-pathname video (format #f "/*/~a.png" "captioned")))))
  (user-feedback! (format #f "Executing: ~a" clean-up-cmd))
  (user-feedback! (format #f "Executing: ~a" clean-up-cmd2))
  (system clean-up-cmd)
  (system clean-up-cmd2)))

(define (video-aspect-ratios-consistent? video downsample? width height gui?)
 (when (not downsample?)
  (if (not gui?) (fuck-up) #t))
 (let* ((video-info-list (video-info video))
        (original-width (first video-info-list))
        (original-height (second video-info-list))
        (ratio-diff (abs (- (/ original-width original-height)
                            (/ width height))))
        (width-ratio (/ original-width width))
        (height-ratio (/ original-height height)))
  (debug-message! (format #f "ratio-diff is ~a~%" ratio-diff))
  (debug-message! (format #f "width-ratio is ~a~%" width-ratio))
  (debug-message! (format #f "height-ratio is ~a~%" height-ratio))
  (cond ((not (and (= original-width (* (round width-ratio) width))
                   (= original-height (* (round height-ratio) height))))
         (user-feedback-with-gui-flag!
          (format #f "~a ~a"
                  "Sorry, original dimension is not an integral"
                  "multiple of the downsample dimension.") gui?)
         #f)
        ((> ratio-diff 0.01)
         (user-feedback-with-gui-flag!
          (format #f "~a ~a"
                  "Sorry, downsample dimension ratio must be"
                  "equal to the original dimension ratio") gui?)
         #f)
        (else #t))))

(define (get-video darpa? video-path downsample? width height
                   fps in-place? gui?)
  ;;; In this function, we must not call any function that depends on
  ;;; *parameters*, eg., we must use user-feedback-with-gui-flag!  instead of
  ;;; user-feedback!.
 (if (not video-path)
     #f
     (let ((video (cond (darpa? (string->darpa-video video-path))
			(else (make-stand-alone-video video-path)))))
      (debug-message! (format #f "get-video, video is ~a, path is ~a~%"
                              video video-path))
      (cond
       ((and (stand-alone-video? video)
             (not (video-exists? video)))
        (user-feedback! (format #f "Can't find ~a.*" video-path))
        #f)
       (downsample?
	(if (not (video-aspect-ratios-consistent? video downsample?
						  width height gui?))
	    (begin
	     (unless gui?
	      (panic "Please change the downsample parameters."))
	     #f)
	    (let ((downsampled-video-name
		   (if in-place?
                       (generic-full-pathname
			*video-pathname* video
			(format #f "-~ax~a@~a.avi" width height fps))
                       (unique-temporary-file
			(format #f "/tmp/~a-~ax~a@~a.avi"
				(any-video->string video) width height fps)))))
	     (if (file-exists? downsampled-video-name)
		 (when (verbose?) (format #t "Skipping downsampling, ~a ~a ~a~%"
					  "in-place mode, and file "
					  downsampled-video-name
					  "already exists"))
		 (begin
		  (when (verbose?)
		   (format #t "Downsampling video to [~ax~a] ~afps~%"
			   width height fps))
		  (system
		   (format #f "ffmpeg -y -i ~a -s ~ax~a -r ~a ~a ~a"
			   (video-pathname video)
			   width height fps
			   downsampled-video-name
                           (if (verbose?)
                               "" ; Don't disable output
                               "-loglevel quiet 2> /dev/null")))
		  (unless in-place?
		   (add-on-exit-thunk!
		    (lambda () (clean-up-tmp-video downsampled-video-name))))))
	     (make-stand-alone-video downsampled-video-name))))
       (in-place? video)
       (else
	(let* ((video-name
		(unique-temporary-file
		 (format #f "/tmp/~a.avi" (any-video->string video))))
	       (ln-cmd
		(format #f "ln -s ~a~a ~a"
			(if (has-directory? (video-pathname video))
			    ""
			    (string-append (pwd) "/"))
			(video-pathname video)
			video-name)))
	 (debug-message! (format #f "~a" ln-cmd))
	 (debug-message! (format #f "video-name is ~a" video-name))
	 (system ln-cmd)
	 (add-on-exit-thunk! (lambda () (clean-up-tmp-video video-name)))
	 (make-stand-alone-video video-name)))))))

(define (reset-navigation-states!)
 (set! *stop-requested* #f)
 (set! *premature-stop?* #f)
 (set! *stopped-frame* #f)
 (set! *played-from-frame* #f))

(define (reset-process!)
 (for-each (o free c-optical-flow-handle) *optical-flow-movie*)
 (set! *processing-state* #f)
 (set! *frame-number* 0)
 (set! *callback-function* #f)
 (set! *optical-flow-continuation* #f)
 (when *detector-handle*
  (cupedro-delete! *detector-handle*)
  (set! *detector-handle* #f))
 (set! *optical-flow-movie* '())
 (set! *klt-continuation* #f)
 (set! *klt-movie* '())
 (set! *detector-continuation* #f)
 (set! *detector-boxes-movies* '())
 (set! *predicted-boxes-movies* '())
 (set! *tracked-boxes-movies-groups* '())
 (set! *smooth-tracked-boxes-movies-groups* '())
 (set! *klt-movie* '())
 (set! *tracked-movies-names* '())
 (set! *fsm-start-time* #f)
 (set! *auto-usleep-start-time* #f)
 (set! *timings* (create-empty-pipeline-timings))
 (set! *likelihoods* '())
 (set! *sentences* '())
 (reset-navigation-states!))

(define (xpending-events?) (> (xpending *display*) 0))

(define call/cc call-with-current-continuation)

(define (recache-strings!)
 #f)

(define (display-selected-video-info pre-text)
 (debug-message! "Calling display-selected-video-info")
 (unless *video* (fuck-up))
 (let* ((video-info-list (video-info *video*))
        (width (first video-info-list))
        (height (second video-info-list))
        (fps (third video-info-list))
        (fps (if (integer? fps) (inexact->exact fps) fps)))
  (user-feedback!
   (format #f "~a video selected (~a frames ~ax~a@~a)"
           pre-text (get-video-length *video*)
           width height fps))))

(define (refresh-gui-after-video-selection)
 (set-preview-snapshot!)
 (clear-composite-image-with-snapshot-and-caption!)
 (set-no-data-to-navigate!)
 (clear-transcript-pane!)
 (redraw-display-pane)
 (redraw-buttons))


(define (update-pixmaps!)
 (let* ((index (- (min (- (video-last-frame *video*) 1) *frame-number*)
		  (video-first-frame *video*)))
	(frame-number (min (- (video-last-frame *video*) 1) *frame-number*)))
  (when (< index 0) (fuck-up))
  (when *video*
   (let* ((frame (ffmpeg-video-frame-data-as-imlib *ffmpeg-video*))
	  (named-track-boxes
	   (join (map (lambda (movies name)
		       (map (lambda (movie) `((#f ,name #f #f) ,movie)) movies))
		      *tracked-boxes-movies-groups*
		      *tracked-movies-names*)))
	  (named-smooth-track-boxes
	   (join (map (lambda (movies name)
		       (map (lambda (movie) `((#f ,name #f #f) ,movie)) movies))
		      *smooth-tracked-boxes-movies-groups*
                      *tracked-movies-names*)))
          (named-detector-boxes
           (map (lambda (movie name) `((#f ,name #f #f) ,movie))
                (topn-boxes-movies (top-n) *detector-boxes-movies*)
                (*model-names*))))
    (allocate-box-colors! (max (length named-smooth-track-boxes)
                               (length named-detector-boxes)))
    (if (has-only-video-data?)
	;; we just want to preview the raw video
	(begin
	 (let ((preview-frame
                (create-padded-image frame *pane-width* *pane-height*)))
	  (imlib-context-set-image! *imlib-image*)
	  (imlib-blend-image-onto-image
	   preview-frame 0 0 0 *pane-width* *pane-height* 0 0
	   *pane-width* *pane-height*)
	  (imlib-context-set-image! preview-frame)
	  (imlib-free-image-and-decache)))
	(begin
	 (draw-9-tiles
	  frame
	  ;; TODO should fill the #fs here
	  named-detector-boxes
	  (map (lambda (movie name) `((#f ,name #f #f) ,movie))
	       (topn-boxes-movies (top-n) *predicted-boxes-movies*)
	       *tracked-movies-names*)
	  named-track-boxes named-smooth-track-boxes
	  *optical-flow-movie* *klt-movie*
	  *imlib-image* *pane-width* *pane-height*
	  frame-number			; frame-number
	  index				; index
	  (max 2 (/ *video-width* 256))	; thickness depends on video dimension
	  *colors*
	  (max 30 (floor (/ *video-width* 40))) ; optical flow rectange width
	  (max 30 (floor (/ *video-width* 40))) ; optical flow rectange height
	  *likelihoods-histogram-image*
	  (not (null? named-smooth-track-boxes))
	  0 #f (if *sentences* (first *sentences*) #f) (rank-box-colors?))))
    (when *debug-flg*
     (save-composite-image-to-file))
    (imlib-context-set-image! frame)
    (imlib-free-image-and-decache)))))

(define (draw-verb-likelihood-histogram likelihoods)
 (let ((likelihoods
        (remove
         #f
         (map
          (lambda (verb)
           (find-if (lambda (r) (equal? (first verb) (result-verb r)))
                    likelihoods))
          *verbs*))))
  (mplot-run-imlib
   (mplot->>=*
    (mplot-subplot-bars-labelled
     (list (map (lambda (a) a)
                (map log (map abs (map result-loglk likelihoods)))))
     (map result-verb likelihoods))
    (mplot-vertical-axes)))))

(define (ffmpeg-seek-frame! frame)
 ;;; frame = 1 means we want the 2nd image.
 (when *ffmpeg-video* (ffmpeg-close-video *ffmpeg-video*))
 (set! *ffmpeg-video* (ffmpeg-open-video (video-pathname *video*)))
 ;;; when ffmpeg-open-video is called, it sets the pointer at the beginning of
 ;;; frame buffer.  So the first frame can be read.  The next
 ;;; ffmpeg-next-frame! call will move the pointer after the first frame.  The
 ;;; number of ffmpeg-next-frame! that returns non-nil will be the number of
 ;;; frames in the video.  So, if you want the 2nd frame, call
 ;;; ffmpeg-next-frame! once.
 (let loop ((i 0))
  ;; (format #t "i is ~a~%" i)
  (and (< i frame) ; frame 1 means we want 2nd image
       (or (not (ffmpeg-next-frame! *ffmpeg-video*)) (loop (+ i 1))))))

(define (every-raw-box-available?)
 (every
  (lambda (model-name)
   (some (lambda (available-boxes)
          (string=? (second available-boxes) model-name))
         (video-voc4-detector-boxes-available *video*)))
  (*model-names*)))

(define (interruptible-video-length)
 (if (has-only-video-data?)
     *video-length*
     (if *premature-stop?*
	 (+ 1 (- *stopped-frame* (video-first-frame *video*)))
	 *video-length*)))

(define (run-cuda-klt? klt-choice)
 (equal? "cuda" klt-choice))

(define (run-c-klt? klt-choice)
 (equal? "c" klt-choice))

(define (run-precomputed-klt? klt-choice)
 (equal? "precomputed" klt-choice))

(define (run-auto-klt? klt-choice)
 (equal? "auto" klt-choice))

(define (read-klt-from-disk?)
 (or (run-precomputed-klt? (klt-choice))
     (and (run-auto-klt? (klt-choice))
	  (klt-movie-available? *video*))))

(define (compute-c-klt?)
 (or (run-c-klt? (klt-choice))
     (and (run-auto-klt? (klt-choice))
	  (not (klt-movie-available? *video*))
	  (= 0 (n-cuda-cards)))))

(define (compute-cuda-klt?)
 (or (run-cuda-klt? (klt-choice))
     (and (run-auto-klt? (klt-choice))
	  (not (klt-movie-available? *video*))
	  (< 0 (n-cuda-cards)))))

(define (write-klt-to-disk?)
 (and (not (read-klt-from-disk?))
      (write-klt?)))

(define (run-cuda-optical-flow? optical-flow-choice)
 (equal? "cuda" optical-flow-choice))

(define (run-matlab-optical-flow? optical-flow-choice)
 (equal? "matlab" optical-flow-choice))

(define (run-precomputed-optical-flow? optical-flow-choice)
 (equal? "precomputed" optical-flow-choice))

(define (run-auto-optical-flow? optical-flow-choice)
 (equal? "auto" optical-flow-choice))

(define (read-optical-flow-from-disk?)
 (or (run-precomputed-optical-flow? (optical-flow-choice))
     (and (run-auto-optical-flow? (klt-choice))
	  (file-exists? (optical-flow-pathname *video*)))))

(define (compute-matlab-optical-flow?)
 (or (run-matlab-optical-flow? (optical-flow-choice))
     (and (run-auto-optical-flow? (optical-flow-choice))
	  (not (file-exists? (optical-flow-pathname *video*)))
	  (= 0 (n-cuda-cards)))))

(define (compute-cuda-optical-flow?)
 (or (run-cuda-optical-flow? (optical-flow-choice))
     (and (run-auto-optical-flow? (optical-flow-choice))
	  (not (file-exists? (optical-flow-pathname *video*)))
	  (< 0 (n-cuda-cards)))))

(define (write-optical-flow-to-disk?)
 (and (not (read-optical-flow-from-disk?))
      (write-optical-flow?)))

(define (run-cuda-boxes? detector-choice)
 (equal? "cuda" detector-choice))

(define (run-precomputed-boxes? detector-choice)
 (equal? "precomputed" detector-choice))

(define (run-matlab-boxes? detector-choice)
 (equal? "matlab" detector-choice))

(define (run-auto-boxes? detector-choice)
 (equal? "auto" detector-choice))

(define (read-boxes-from-disk?)
 (or (run-precomputed-boxes? (detector-choice))
     (and (run-auto-boxes? (detector-choice))
	  (every-raw-box-available?))))

(define (compute-matlab-boxes?)
 (or (run-matlab-boxes? (detector-choice))
     (and (run-auto-boxes? (detector-choice))
	  (not (every-raw-box-available?))
	  (= 0 (n-cuda-cards)))))

(define (compute-cuda-boxes?)
 (or (run-cuda-boxes? (detector-choice))
     (and (run-auto-boxes? (detector-choice))
	  (not (every-raw-box-available?))
	  (< 0 (n-cuda-cards)))))

(define (write-object-detector-to-disk?)
 (and (not (read-boxes-from-disk?))
      (write-object-detector?)))

(define (write-tracker-to-disk?) (write-tracker?))

(define (read-tracker-from-disk?) (read-tracker?))

(define (write-captioned-and-tracked-videos video)
 (user-feedback! "Generating captioned and tracked videos")
 (let* ((named-track-boxes
         (join (map (lambda (movies name)
                     (map (lambda (movie) `((#f ,name #f #f) ,movie)) movies))
                    *tracked-boxes-movies-groups*
                    *tracked-movies-names*)))
        (named-smooth-track-boxes
         (join (map (lambda (movies name)
                     (map (lambda (movie) `((#f ,name #f #f) ,movie)) movies))
                    *smooth-tracked-boxes-movies-groups*
                    *tracked-movies-names*)))
        (named-detector-boxes
         (map (lambda (movie name)
               `((#f ,name #f #f) ,movie))
              (topn-boxes-movies (top-n) *detector-boxes-movies*)
              (*model-names*))))
  (allocate-box-colors! (max (length named-smooth-track-boxes)
                             (length named-detector-boxes)))
  (render-sentence-9-tile-frames
   video
   (if (null? *sentences*) #f (first *sentences*))
   *colors*
   named-detector-boxes
   (map (lambda (movie name) `((#f ,name #f #f) ,movie))
        (topn-boxes-movies (top-n) *predicted-boxes-movies*)
        *tracked-movies-names*)
   named-track-boxes
   named-smooth-track-boxes
   *optical-flow-movie*
   *klt-movie*
   ;; TODO These should be abstracted into functions:
   *pane-width* *pane-height*
   (max 2 (/ *video-width* 256))
   *caption-height*
   "tracked" (max 8 (floor (/ *video-width* 40)))
   (max 8 (floor (/ *video-width* 40)))
   (if (equal? #f *display*) #f *likelihoods-histogram-image*)
   (rank-box-colors?)))
 (let ((destination-directory (if (video-destination-directory)
				  (video-destination-directory)
				  ".")))
  (ffmpeg (if (and (downsample-fps) (not (= (downsample-fps) 0)))
	      (downsample-fps) *video-fps*)
	  (generic-root-pathname video (format #f "/%06d/~a.png" "tracked"))
	  (format #f "~a/~a-tracked.avi" destination-directory
		  (any-video->string video)))
  (if (null? *sentences*)
      (user-feedback! "No sentences were produced")
      (begin
       (user-feedback! "Generating captioned video")
       (when (verbose?)
	(for-each (lambda (sentence)
		   (format #t "~a ~a~%" (first sentence) (second sentence)))
		  *sentences*))
       (render-sentence-frames video (first *sentences*) "captioned" (rank-box-colors?))
       (ffmpeg (if (and (downsample-fps) (not (= (downsample-fps) 0)))
		   (downsample-fps) *video-fps*)
	       (generic-root-pathname video
				      (format #f "/%06d/~a.png" "captioned"))
	       (format #f "~a/~a-captioned.avi" destination-directory
		       (any-video->string video))))))
 (add-on-exit-thunk!
  (lambda () (clean-up-track-and-caption-video-temporaries video)))
 (user-feedback! "Done writing captioned and tracked videos"))

(define (read-predicted-boxes-of-a-model video-name model-name-only)
 (map (lambda (voc4-avail)
       (let ((model-name (second voc4-avail)))
 	(read-voc4-predicted-boxes video-name model-name)))
      (remove-if-not (lambda (voc4-avail)
                      (equal? (second voc4-avail) model-name-only))
                     (video-voc4-predicted-boxes-available video-name))))

(define (read-tracked-boxes-of-a-model video-name model-name-only)
 (map (lambda (voc4-avail)
       (let ((model-name (second voc4-avail))
 	     (id-number (third voc4-avail)))
 	(read-voc4-tracked-boxes video-name model-name id-number)))
      (remove-if-not (lambda (voc4-avail)
                      (equal? (second voc4-avail) model-name-only))
                     (video-voc4-tracked-boxes-available video-name))))

(define (read-smooth-tracked-boxes-of-a-model video-name model-name-only)
 (map (lambda (voc4-avail)
       (let ((model-name (second voc4-avail))
 	     (id-number (third voc4-avail)))
 	(read-voc4-smooth-tracked-boxes video-name model-name id-number)))
      (remove-if-not (lambda (voc4-avail)
                      (equal? (second voc4-avail) model-name-only))
                     (video-voc4-smooth-tracked-boxes-available video-name))))

(define (pp-results results)
 (when #f
  (for-each
   (lambda (r)
    (format #t "Loglk=~a tracks=~a~%" (result-loglk r) (result-track-names r)))
   results))
 results)

(define (likelihoods->results likelihoods)
 (append
  (take-if-possible
   5
   (sort (remove-if-not
	  (lambda (a) (= (length (result-named-tracks a)) 2)) likelihoods)
	 > result-loglk))
  (take-if-possible
   5
   (sort (remove-if-not
	  (lambda (a) (= (length (result-named-tracks a)) 1)) likelihoods)
	 > result-loglk))))

(define (process-write-commands)
 (when (and (write-object-detector-to-disk?) *detector-boxes-movies*)
  (timed-f
   increment-saving-timing
   (lambda ()
    (for-each
     (lambda (boxes model-name)
      (when (verbose?)
       (format #t "Writing raw '~a' felzenszwalb results~%"
	       model-name))
      (write-voc4-boxes-movie boxes *video* model-name))
     *detector-boxes-movies*
     (*model-names*)))))
 (when (and (write-klt-to-disk?) *klt-movie*)
  (when (verbose?) (format #t "Writing klt to disk~%"))
  (timed-f
   increment-saving-timing
   (lambda ()
    (write-klt-movie *video* *klt-movie*))))
 (when (and (write-optical-flow-to-disk?) *optical-flow-movie*)
  (when (verbose?) (format #t "Writing optical flow to disk~%"))
  (timed-f
   increment-saving-timing
   (lambda ()
    (write-optical-flow-movie *video* *optical-flow-movie*)))))

(define (auto-usleep micro-seconds)
 ;;; Automatically determine the amount of time to sleep in order to maintain
 ;;; a fixed playback rate.
 (unless *auto-usleep-start-time*
  (set! *auto-usleep-start-time* (clock-sample)))
 (let* ((new-start-time (clock-sample))
        (elasped-time (- new-start-time *auto-usleep-start-time*))
        (remaining-time (- micro-seconds (* 1000000 elasped-time))))
  (when (> remaining-time 0)
   (usleep remaining-time))
  (set! *auto-usleep-start-time* new-start-time)))

(define (run-felzenszwalb)
 (when *detector-handle*
  (cupedro-delete! *detector-handle*)
  (set! *detector-handle* #f))
 (timed-f
  increment-detector-timing
  (lambda ()
   (call/cc (lambda (k)
	     (set! *callback-function* k)
	     (run-felzenszwalb-detector-with-*flow*
	      *video*
	      (*model-names*)
	      (final-cascade-adjustment)
	      (threshold-offset)
	      (nms) (top-n)
	      (max-detections)))))))

(define (process-next-frame-with-callbacks)
 (let loop ()
  (define (next)
   (when *display*
    (debug-message! "calling loop")
    (recache-strings!)
    (update-pixmaps!)
    (send *display-pane* 'expose)
    (redraw-buttons))
   (loop))
  (when (or (not *display*) (not (xpending-events?)))
   (when (and *processing-state* *video*)
    ;; A FSM
    ;;
    ;; start -#t> next-frame -(= *frame-number* *video-length*)> #(track model-groups)
    ;; -(null? model-groups)> likelihoods -#t> sentences -#t> done
    ;;
    ;; play -(= *frame-number* *video-length*)> done
    (cond ((equal? *processing-state* 'done)
           (stop-callbacks!)
           (when *fsm-start-time*
            (set-pipeline-timing-all! *timings*
                                      (- (clock-sample) *fsm-start-time*))
            (format #t "~%      Profiling Information      ~%")
            (format #t "      ---------------------      ~%~%")
            (format #t "~a~%~%"
                    (pipeline-timing->pretty-string *timings*)))
           (when *stop-requested* ; stop prematurely
            (if (has-only-video-data?)
                (user-feedback! (format #f "Stopped playing (at frame ~a)"
                                        *frame-number*))
                (user-feedback! (format #f "Stopped processing (at frame ~a)"
                                        *frame-number*)))
            (set! *stop-requested* #f)
            (set! *stopped-frame* *frame-number*)
            (set! *premature-stop?* #t)))
          ((and *stop-requested* (not *premature-stop?*))
           ;; this happens when stop button was pressed but immediately
           ;; followed by another button.  As a result the stop button never
           ;; gets handled in process-next-frame-with-callbacks.  So, here's
           ;; the fix to redo the missing stop action.
           (when *stop-requested* ; stop prematurely
            (if (has-only-video-data?)
                (user-feedback! (format #f "Stopped playing (at frame ~a)"
                                        *frame-number*))
                (user-feedback! (format #f "Stopped processing (at frame ~a)"
                                        *frame-number*)))
            (set! *stop-requested* #f)
            (set! *stopped-frame* *frame-number*)
            (set! *premature-stop?* #t))
           )
          ((and (equal? *processing-state* 'play-continuous)
                (= *frame-number* (interruptible-video-length)))
           (set! *frame-number* *played-from-frame*) ; return just before play
           (set! *played-from-frame* #f)
           (ffmpeg-seek-frame! *frame-number*)
           (set! *fsm-start-time* #f)
           (set! *processing-state* 'done)
           (next))
          ((equal? *processing-state* 'play-continuous)
           (set! *frame-number* (+ *frame-number* 1))
           (ffmpeg-next-frame! *ffmpeg-video*)
           (auto-usleep (inexact->exact (round (/ 1000000.0 *video-fps*))))
           (next))
          ((and (equal? *processing-state* 'play-next)
                (>= *frame-number* (interruptible-video-length)))
           (user-feedback! "At the end of the processed data")
           (niffty-bell 1000000)
           (set! *fsm-start-time* #f)
           (set! *processing-state* 'done)
           (next))
          ((equal? *processing-state* 'play-next)
           (set! *frame-number* (+ *frame-number* 1))
           (ffmpeg-next-frame! *ffmpeg-video*)
           (set! *fsm-start-time* #f)
           (set! *processing-state* 'done)
           (next))
          ((and (equal? *processing-state* 'play-previous)
                (<= *frame-number* 1))
           (user-feedback! "At the beginning of the processed data")
           (niffty-bell 1000000)
           (set! *fsm-start-time* #f)
           (set! *processing-state* 'done)
           (next))
          ((equal? *processing-state* 'play-previous)
           (set! *frame-number* (- *frame-number* 1))
           (ffmpeg-seek-frame! *frame-number*)
           (set! *fsm-start-time* #f)
           (set! *processing-state* 'done)
           (next))
          ;;; STATE = start
          ((equal? *processing-state* 'start)
           (set! *fsm-start-time* (clock-sample))
           (set! *timings* (create-empty-pipeline-timings))
	   (set! *likelihoods* '())
	   (set! *sentences* '())
           (user-feedback! "Initializing ...")
           ;; Display forward-porjection-mode
           (when (verbose?) (format #t "Forward projection mode = ~a~%"
                                    *forward-projection-mode*))
           (unless (equal? *forward-projection-mode* 'optical-flow)
            ;; klt
            (cond
             ((read-klt-from-disk?)
              (user-feedback! "Reading KLT from disk")
              (timed-f
               increment-klt-timing
               (lambda ()
                (set! *klt-movie* (read-klt-movie *video*)))))
             ((compute-c-klt?)
              (panic "C klt not implemented yet"))
             ((compute-cuda-klt?)
              (timed-f
               increment-klt-timing
               (lambda ()
                (call/cc (lambda (k)
                          (set! *callback-function* k)
                          ;; (run-klt-cuda *video*
                          ;;               (ffmpeg-video-width *ffmpeg-video*)
                          ;;               (ffmpeg-video-height *ffmpeg-video*)
                          ;;               1000 3 15 11 23)
                          ;;; Change smooth-size depending on the image height
                          (let* ((width (ffmpeg-video-width *ffmpeg-video*))
                                 (height (ffmpeg-video-height *ffmpeg-video*))
                                 (smooth-size (round (/ height 30)))
                                 (smooth-size (if (even? smooth-size)
                                                  (+ 1 smooth-size)
                                                  smooth-size)))
                           (run-klt-cuda *video*
                                         width height
                                         1000 3 15 11 ; default values
                                         smooth-size)))))))
             (else (panic "Must specify a KLT switch"))))
           (unless (equal? *forward-projection-mode* 'klt)
            ;; optical-flow
            (cond
             ((read-optical-flow-from-disk?)
              (user-feedback! "Reading optical flow from disk")
              (timed-f
               increment-flow-timing
               (lambda ()
                (set! *optical-flow-movie*
                      (read-optical-flow-movie-in-c *video*)))))
             ((compute-matlab-optical-flow?)
              (panic "Matlab optical flow not implemented yet"))
             ((compute-cuda-optical-flow?)
              (timed-f
               increment-flow-timing
               (lambda ()
                (call/cc (lambda (k)
                          (set! *callback-function* k)
                          (cuda-optical-flow *video*))))))
             (else (panic "Must specify an optical switch"))))
           ;; boxes files
           (cond
            ((read-boxes-from-disk?)
             (user-feedback! "Reading boxes files from disk")
             (timed-f
              increment-detector-timing
              (lambda ()
               (set! *detector-boxes-movies*
                     (map (lambda (model-name)
                           (read-voc4-detector-boxes *video* model-name))
                          (*model-names*))))))
            ((compute-matlab-boxes?)
             (panic "Matlab detector not plugged in yet"))
            ((compute-cuda-boxes?)
             (when (verbose?)
              (format #t "Running cuda-felzenszwalb on ~a frames~%"
                      (video-length *video*))
              (format #t "  Final-cascade-adjustment = ~a~%"
                      (final-cascade-adjustment))
              (format #t "  Threshold-adjustment     = ~a~%"
                      (threshold-offset))
              (format #t "  Non-maximal-suppression   = ~a~%" (nms))
              (format #t "  Max detections           = ~a~%" (max-detections)))
             (run-felzenszwalb))
            (else (panic "Failed to load/compute boxes")))
	   (when (toy-demo?)
	    ;; TODO ANDREI This is a hack for the demo
	    (let* ((maximum1 (lambda (l) (if (null? l) (- infinity) (maximum l))))
		   (skipped-models
		    (map
		     second
		     (cdr
		      (sort
		       (remove-if
			(lambda (a) (equal? "person" (second a)))
			(zip
			 (map (lambda (b) (maximum1 (map voc4-detection-strength (first b))))
			      *detector-boxes-movies*)
			 (*model-names*)))
		       >
		       first)))))
	     (set! *detector-boxes-movies*
		   (map first
			(remove-if
			 (lambda (a) (member (second a) skipped-models))
			 (zip *detector-boxes-movies* (*model-names*)))))
	     (set-global-option-model-groups!
	      *parameters*
	      (remove '() (map (lambda (model-group)
				(set-difference model-group skipped-models))
			       (model-groups))))
	     (format #t "Skipping models ~a and keeping ~a~%"
		     skipped-models (*model-names*))
	     (when (compute-cuda-boxes?) (run-felzenszwalb))))
           (set! *frame-number* (+ *frame-number* 1))
           (set! *processing-state* 'next-frame)
           (ffmpeg-next-frame! *ffmpeg-video*)
           (next))
          ;;; STATE = next-frame
          ((equal? *processing-state* 'next-frame)
           (user-feedback! (format #f "Processing ~a/~a"
                                   *frame-number* *video-length*))
           (when *optical-flow-continuation*
            (timed-f
             increment-flow-timing
             (lambda ()
              (call/cc (lambda (k)
                        (set! *callback-function* k)
                        (*optical-flow-continuation* #f))))))
           (when *detector-continuation*
            (timed-f
             increment-detector-timing
             (lambda ()
              (call/cc (lambda (k)
                        (set! *callback-function* k)
                        (*detector-continuation* #f))))))
           (when *klt-continuation*
            (timed-f
             increment-klt-timing
             (lambda ()
              (call/cc (lambda (k)
                        (set! *callback-function* k)
                        (*klt-continuation* #f))))))
           (set! *frame-number* (+ *frame-number* 1))
	   (when (= *frame-number* *video-length*)
            (user-feedback! "Tracking")
            (set! *processing-state*
                  ;; we cons results later on so we reverse the names
                  ;; first, this just keeps everything in the same
                  ;; order
                  (vector 'track (reverse (model-groups)))))
           (unless (ffmpeg-video-finished? *ffmpeg-video*)
            (ffmpeg-next-frame! *ffmpeg-video*))
           (next))
	  ;; STATE = tracking, but we want to stop
	  ((and (vector? *processing-state*)
		(equal? (x *processing-state*) 'track)
		(stop-before-tracker?))
	   (user-feedback! "Not tracking")
	   (set! *processing-state* 'sentences)
	   (process-write-commands)
	   (next))
          ;; STATE = tracking, but no track groups to do
          ((and (vector? *processing-state*)
                (equal? (x *processing-state*) 'track)
                (null? (y *processing-state*)))
           (user-feedback! "Done tracking")
	   (process-write-commands)
	   (if (stop-after-tracker?)
	       (set! *processing-state* 'sentences)
	       (set! *processing-state* 'likelihoods))
           (next))
          ;; STATE = tracking
          ((and (vector? *processing-state*)
                (equal? (x *processing-state*) 'track)
                (not (null? (y *processing-state*))))
	   (when *detector-handle*
	    (cupedro-delete! *detector-handle*)
	    (set! *detector-handle* #f))
           (let* ((model-group (first (y *processing-state*)))
                  (model-name-only (first model-group)))
            (if (read-tracker-from-disk?)
                (begin
                 (user-feedback!
                  "Reading predicted and tracked boxes files from disk")
                 (user-feedback! (format #f "Tracking ~a" model-group))
                 (set! *tracked-movies-names*
                       (append *tracked-movies-names* (list model-name-only)))
                 (set! *predicted-boxes-movies*
                       (append *predicted-boxes-movies*
                               (read-predicted-boxes-of-a-model
				*video* model-name-only)))
                 (set! *tracked-boxes-movies-groups*
                       (append *tracked-boxes-movies-groups*
                               (list (read-tracked-boxes-of-a-model
                                      *video* model-name-only))))
                 (set! *smooth-tracked-boxes-movies-groups*
                       (append *smooth-tracked-boxes-movies-groups*
                               (list (read-smooth-tracked-boxes-of-a-model
                                      *video* model-name-only)))))
                (begin
                 (user-feedback! (format #f "Tracking ~a" model-group))
                 (let ((results
                        (timed-f
                         increment-tracker-timing
                         (lambda ()
                          (viterbi-track-group-in-memory
                           *video*
                           (cond ((equal? *forward-projection-mode* 'optical-flow)
                                  *optical-flow-movie*)
                                 ((or (equal? *forward-projection-mode* 'klt)
                                      (equal? *forward-projection-mode* 'mixed))
                                  *klt-movie*)
                                 (else (fuck-up)))
                           (cond ((or (equal? *forward-projection-mode* 'mixed)
                                      (equal? *forward-projection-mode* 'optical-flow))
                                  *optical-flow-movie*)
                                 ((equal? *forward-projection-mode* 'klt)
                                  *klt-movie*)
                                 (else (fuck-up)))
                           model-group
                           *detector-boxes-movies*
                           (*model-names*)
                           (nms)
                           (* (if (downsample?)
                                  (/ (downsample-fps) 30)
                                  (/ *video-fps* 30))
                              (alpha))
			   (* (if (downsample?)
                                  (/ (downsample-fps) 30)
                                  (/ *video-fps* 30))
                              (beta))
                           (top-n)
                           (with-dt?)
                           (model-path)
                           *model-threshold-tracker-offset*
                           *profile-best-boxes?*
                           (lookahead)
                           *minimum-track-length*
                           *maximum-track-overlap-ratio*
                           *overgeneration-minimum-track-length*
                           *overgeneration-maximum-track-overlap-ratio*
                           (suppression-delta)
                           (max-tracks))))))
                  (when results
                   (when (write-tracker-to-disk?)
                    (when (verbose?)
                     (format #t "Writing tracker results to disk~%"))
                    (timed-f
                     increment-saving-timing
                     (lambda ()
                      (write-viterbi-track-results *video*
                                                   (first model-group)
                                                   results))))
                   (set! *tracked-movies-names*
                         (append *tracked-movies-names*
                                 (list (first model-group))))
                   (set! *predicted-boxes-movies*
                         (append *predicted-boxes-movies*
                                 (list (vector-ref results 0))))
                   (set! *tracked-boxes-movies-groups*
                         (append *tracked-boxes-movies-groups*
                                 (list (vector-ref results 3))))
                   (set! *smooth-tracked-boxes-movies-groups*
                         (append *smooth-tracked-boxes-movies-groups*
                                 (list (vector-ref results 4)))))))))
           (set! *processing-state*
                 (vector 'track (cdr (y *processing-state*))))
           (next))
          ;;; STATE = likelihood
          ((equal? *processing-state* 'likelihoods)
           (user-feedback! "Computing likelihoods")
           (timed-f
            increment-likelihoods-timing
            (lambda ()
             (set! *likelihoods*
                   (sort
                    (join
                     ((if (toy-demo?)
			  compute-pseudo-likelihoods
			  compute-likelihoods)
                      *video*
                      (join
                       (map (lambda (track-name tracks)
                             (map (lambda (track) (list track-name track)) tracks))
                            *tracked-movies-names*
                            *smooth-tracked-boxes-movies-groups*))
                      (event-models)
		      (medoids)
		      (lambda (a b c d) (d)) #f))
                    >
                    result-loglk))))
           (timed-f
	    increment-draw-likelihoods-timing
	    (lambda ()
	     (set! *likelihoods-histogram-image*
		   (imlib-verb-likelihood-histogram! *likelihoods* 1280 720 15 130 5))))
           (set! *processing-state* 'sentences)
           (next))
          ;;; STATE = sentences
	  ((equal? *processing-state* 'sentences)
           (user-feedback! "Computing sentences")
           (set! *sentences*
                 (timed-f
                  increment-sentences-timing
                  (lambda ()
                   (map (lambda (result) (result->sentence *video* result))
			(likelihoods->results *likelihoods*)))))
	   (when *display*
            (if (null? *sentences*)
                (begin (user-feedback! "No sentence produced")
		       (say "No sentence produced")
                       (speak "No sentence produced"))
                (begin (user-feedback! (first (first *sentences*)))
		       (say (first (first *sentences*)))
                       (speak (first (first *sentences*))))))
	   (unless *display*
	    (when (verbose?)
	     (if (null? *sentences*)
		 (user-feedback! "No sentence produced")
		 (for-each user-feedback! (map first *sentences*)))))
           (set! *processing-state* 'done)
           (set! *started-processing?* #f)
           (set! *finished-processing?* #t)
           (user-feedback! "Done processing")
           (when (or (not (gui?)) (video-destination-directory))
            (timed-f
	     increment-write-videos-timing
	     (lambda ()
	      (write-captioned-and-tracked-videos *video*))))
           (next))
          ;;; STATE = done
          (else (panic
                 "Unimplemented process-next-frame-with-callbacks state")))))))

(define (*frame-index*)
 (- (min (- (video-last-frame *video*) 1) *frame-number*)
    (video-first-frame *video*)))

(define (run-felzenszwalb-detector-with-*flow*
	 video models
         final-cascade-adjustment threshold-offset
         nms-threshold top-n max-detections)
 (run-felzenszwalb-cuda
  video
  (map cuda-model-pathname models)
  (cuda-pca-coefficients-pathname)
  final-cascade-adjustment
  threshold-offset
  max-detections
  (lambda (boxes model-index)
   (remove-if
    (lambda (box)
     (and (toy-demo?)
	  (or
	   (>= *frame-number* (length *optical-flow-movie*))
	   (remove-box-toy-demo?
	    box (list-ref *optical-flow-movie* *frame-number*)))))
    ((if *min-flow*
	 (lambda (boxes)
	  (let
	    ((min-boxes
	      (remove-if
	       (lambda (box)
		(< (magnitude
		    (average-flow-in-box
		     box
		     (list-ref *optical-flow-movie* *frame-number*)))
		   ;; FIXME Andrei This is fps dependent
		   *min-flow*))
	       boxes)))
	   (debug-message!
	    (format #f "Min flow started with ~a boxes and ended with ~a boxes~%"
		    (length boxes) (length min-boxes)))
	   min-boxes))
	 identity)
     (top-n-non-maximal-suppression-with-ringing boxes nms-threshold top-n))))))

(define (start-callbacks!)
 (add-background-task-enabler! process-next-frame-with-callbacks))
;;; (set-background-task-enabler! ))

(define (stop-callbacks!)
 (remove-background-task-enabler! process-next-frame-with-callbacks)
 ;;(set-background-task-disabler! (lambda () #f))
 (set! *callback-function* #f))

(define (is-playing?)
 (equal? *processing-state* 'play-continuous))

(define (show-process-button?)
 (and (video-selected?)
      (not (or (viewfinder-running?)
               (process-running?)))))

(define (show-stop-button?)
 (and (process-running?)
      (not (or (viewfinder-running?)
               (process-finished?)))))

(define (show-navigation-buttons?)
 (and (or (has-data-to-navigate?)
          (and (has-only-video-data?)
               (video-opened?)))
      (not (or (viewfinder-running?)
               (not (video-selected?))))))

(define (show-play-buttons?)
 (and (or (has-only-video-data?)
	  (is-playing?)
	  (show-navigation-buttons?))
      (not (viewfinder-running?))))

(define (call-process-button!)
 (define (process-button!)
  (debug-message! (format #f "**** called process-button"))
  (user-feedback! "")
  (set! *started-processing?* #t)
  (set! *finished-processing?* #f)
  (reset-process!)
  (when *display*
   (set! *likelihoods-histogram-image* #f)
   (clear-preview-snapshot!)
   (clear-composite-image-with-snapshot-and-caption!)
   (clear-transcript-pane!)
   (redraw-display-pane)
   (redraw-buttons))
  (when *ffmpeg-video* (ffmpeg-close-video *ffmpeg-video*))
  (set! *ffmpeg-video* (ffmpeg-open-video (video-pathname *video*)))
  (set! *video-length* (get-video-length *video*))
  (user-feedback! "Processing ...")
  (set! *processing-state* 'start)
  (start-callbacks!))
 (if (or (equal? #f *display*) (show-process-button?))
     (process-button!)
     (action-not-availiable)))

(define (call-play-button!)
 (define (play-button!)
  (debug-message! (format #f "**** called play-button"))
  (user-feedback! "")
  (if (is-playing?)
      (begin
       (user-feedback! (format #f "Stopped playing (at frame ~a)"
                               *frame-number*))
       (set! *processing-state* 'done)
       (when (has-only-video-data?)
	(set! *stop-requested* #t))
       (redraw-buttons))
      (begin
       (when (and (equal? *stopped-frame* #f) (has-only-video-data?))
	(when *ffmpeg-video* (ffmpeg-close-video *ffmpeg-video*))
	(set! *ffmpeg-video* (ffmpeg-open-video (video-pathname *video*)))
	(set! *video-length* (+ -1 (video-length *video*))))
       ;;; set to play from current frame:
       (set! *played-from-frame* (max 1 *frame-number*))
       (set! *processing-state* 'play-continuous)
       (when (= *frame-number* (interruptible-video-length))
	(set! *frame-number* 0)
	(when *ffmpeg-video* (ffmpeg-close-video *ffmpeg-video*))
        (set! *ffmpeg-video* (ffmpeg-open-video (video-pathname *video*))))
       (set! *processing-state* 'play-continuous)))
  (start-callbacks!))
 (if (show-play-buttons?)
     (play-button!)
     (action-not-availiable)))

(define (call-stop-button!)
 (define (stop-button!)
  (debug-message! (format #f "**** called stop-button"))
  (user-feedback! "")
  (set! *processing-state* 'done)
  (set! *started-processing?* #f)
  (set! *finished-processing?* #f)
  (set! *stop-requested* #t)
  (redraw-buttons))
 (if (show-stop-button?)
     (stop-button!)
     (action-not-availiable)))

(define (call-beginning-button!)
 (define (beginning-button!)
  (debug-message! (format #f "**** called beginning-button"))
  (user-feedback! "")
  (set! *frame-number* 0)
  (when *ffmpeg-video* (ffmpeg-close-video *ffmpeg-video*))
  (set! *ffmpeg-video* (ffmpeg-open-video (video-pathname *video*)))
  (set! *processing-state* 'play-next)
  (start-callbacks!))
 (if (show-navigation-buttons?)
     (beginning-button!)
     (action-not-availiable)))

(define (call-previous-button!)
 (define (previous-button!)
  (debug-message! (format #f "**** called previous-button"))
  (user-feedback! "")
  (set! *processing-state* 'play-previous)
  (start-callbacks!))
 (if (show-navigation-buttons?)
     (previous-button!)
     (action-not-availiable)))

(define (call-next-button!)
 (define (next-button!)
  (debug-message! (format #f "**** called next-button"))
  (user-feedback! "")
  (set! *processing-state* 'play-next)
  (start-callbacks!))
 (if (show-navigation-buttons?)
     (next-button!)
     (action-not-availiable)))

(define (call-end-button!)
 (define (end-button!)
  (debug-message! (format #f "**** called end-button"))
  (user-feedback! "")
  (set! *frame-number* (+ -1 (interruptible-video-length)))
  (ffmpeg-seek-frame! *frame-number*)
  (set! *processing-state* 'play-next)
  (start-callbacks!))
 (if (show-navigation-buttons?)
     (end-button!)
     (action-not-availiable)))

(define (set-video-and-parameters! parameters)
 (set! *video* (get-video (global-option-darpa? parameters)
			  (global-option-video-path parameters)
			  (global-option-downsample? parameters)
			  (global-option-downsample-width parameters)
			  (global-option-downsample-height parameters)
			  (global-option-downsample-fps parameters)
			  (global-option-in-place? parameters)
                          (global-option-gui? parameters)))
 (debug-message! (format #f "setting video and para~a~%" *video*))
 (set! *parameters* parameters)
 (if (equal? #f *video*)
     (begin
      (when (not downsample?)
       (user-feedback! "Sorry, video could not be loaded"))
      (unless (gui?) (fuck-up))
      #f)
     (let ((video-info-list (video-info *video*)))
      ;;; until we set *parameters*, we must use user-feedback-with-gui-flag!
      ;;; instead of user-feedback!.
      (set-global-option-event-models!
       *parameters* (map read-hmm (remove "" (read-file (event-models-file)))))
      ;; FIXME, technically pose-codebook.sc is only required when
      ;; using hmms with discrete pose, but for now it must always exist
      (set-global-option-medoids!
       *parameters*
       (read-object-from-file
	(string-append (if (has-directory? (event-models-file))
			   (directory (event-models-file))
			   ".")
		       "/pose-codebook.sc")))
      (set! *video-width* (first video-info-list))
      (set! *video-height* (second video-info-list))
      (set! *video-fps* (third video-info-list))
      (set! *video-fps* (if (integer? *video-fps*)
                            (inexact->exact *video-fps*) *video-fps*))
      (when *ffmpeg-video* (ffmpeg-close-video *ffmpeg-video*))
      (set! *ffmpeg-video* #f)
      (set! *video-length* #f)
      (set! *frame-number* #f)
      (set! *stopped-frame* #f)
      (set-video-name-caption! (format #f "~a (~ax~a@~a)"
                                       (video-path)
                                       *video-width* *video-height*
                                       *video-fps*))
      (when (verbose?)
       (format #t "Setting video:~%")
       (format #t "~a~%" (video-pathname *video*))
       (format #t "Original dimensions: ~ax~a@~a~%"
	       *video-width* *video-height* *video-fps*)
       (format #t "Scale: ~a~%" (scale))
       (format #t "~%")
       (format #t "Setting global options:~%~a"
	       (global-options->pretty-string parameters))
       (format #t "~%"))
      (when (> *video-fps* 30) (fuck-up))
      (reset-process!)
      #t)))

(define (create-demo-parameters video-path model-groups)
 (let ((options (create-default-global-options)))
  (set-global-option-video-path! options video-path)
  (set-global-option-model-groups! options model-groups)
  (set-global-option-alpha! options 50)
  (set-global-option-beta! options 50)
  (set-global-option-downsample?! options #t)
  (set-global-option-downsample-width! options 640)
  (set-global-option-downsample-height! options 360)
  (set-global-option-downsample-fps! options 10)
  (set-global-option-top-n! options 20)
  (set-global-option-in-place?! options (option-in-place?))
  (set-global-option-event-models-file! options (event-models-file))
  (set-global-option-darpa?! options #f)
  (set-global-option-video-destination-directory!
   options (video-destination-directory))
  (set-global-option-write-object-detector?! options (write-object-detector?))
  (set-global-option-write-klt?! options (write-klt?))
  (set-global-option-write-optical-flow?! options (write-optical-flow?))
  (set-global-option-write-tracker?! options (write-tracker?))
  (set-global-option-model-path! options (model-path))
  options))

(define (set-to-demo1-video!)
 (set-video-and-parameters!
  (create-demo-parameters
   "EXIT8_A1_C1_Act1_PARK_MC_AFTN_438c8a26-1dc6-11e0-ad1b-e80688ca39a2"
   '(("person" "person-crouch" "person-down") ("bicycle")))))

(define (set-to-demo2-video!)
 (set-video-and-parameters!
  (create-demo-parameters
   "LIFT8_A1_C1_Act6_2_PARK_MC_AFTN_b46791cf-07b6-11e0-8ae7-e80688cb869a"
   '(("person" "person-crouch" "person-down") ("closet")))))

(define (set-to-demo3-video!)
 (set-video-and-parameters!
  (create-demo-parameters
   "ARRIVE10_A1_C1_Act5_6_URBAN_MC_AFTN_4379062c-1dc6-11e0-ad1b-e80688ca39a2"
   '(("person" "person-crouch" "person-down") ("motorcycle")))))

(define (set-to-demo4-video!)
 (set-video-and-parameters!
  (create-demo-parameters
   "ARRIVE3_A1_C1_Act2_URBAN_MC_AFTN_b42f499c-07b6-11e0-80fb-e80688cb869a"
   '(("person" "person-crouch" "person-down") ("skateboard")))))

(define (set-to-demo5-video!)
 (set-video-and-parameters!
  (create-demo-parameters
   "ARRIVE6_A1_C2_Act2_URBAN_ML_AFTN_DARK_b430112e-07b6-11e0-885b-e80688cb869a"
   '(("person" "person-crouch" "person-down") ("bicycle")))))

(define (set-to-demo6-video!)
 (set-video-and-parameters!
  (create-demo-parameters
   "BURY1_A1_C1_Act6_PARK_MC_AFTN_DARK_b432c39c-07b6-11e0-af21-e80688cb869a"
   '(("person" "person-crouch" "person-down") ("bag")))))

(define (set-to-demo7-video!)
 (set-video-and-parameters!
  (create-demo-parameters
   "PICKUP7_A1_C1_Act1_URBAN_MR_AFTN_DARK_43ad47b6-1dc6-11e0-ad1b-e80688ca39a2"
   '(("person" "person-crouch" "person-down") ("dog")))))

(define (set-to-command-line-video!)
 (set-video-and-parameters! *cmd-parameters*))

(define (set-to-captured-video!)
 (debug-message! (format #f "projection mode is ~a" *forward-projection-mode*))
 (let ((options (create-demo-parameters
                 (captured-video-name)
                 '(("giraffe") ("sign") ("ball") ("gun") ("person")))))
  (set-global-option-top-n! options 4)
  (set-global-option-toy-demo?! options #t)
  (set-global-option-beta! options 4)
  (set-global-option-final-cascade-adjustment! options -1.5)
  (set-global-option-threshold-offset! options -0.9)
  (set-global-option-nms! options 0.7)
  (set-global-option-max-tracks! options 1)
  (set-global-option-suppression-delta! options -2)
  (set-global-option-rank-box-colors?! options #t)
  (set-global-option-alpha! options 10)
  (set-global-option-downsample?! options #f)
  (set-video-and-parameters! options)))

(define (save-command-line-video-and-parameters! parameters)
 (set! *cmd-parameters* parameters))

(define (clean-up-gui-states!)
 (set! *cuda-device* #f)
 (reset-process!)
 ;;; video:
 (when *ffmpeg-video*
  (ffmpeg-close-video *ffmpeg-video*))
 (set! *ffmpeg-video* #f)
 (set! *video* #f)
 (set! *video-length* #f)
 (set! *video-width* #f)
 (set! *video-height* #f)
 (set! *video-fps* #f)
 ;;; images:
 (imlib-context-set-image! *imlib-image*)
 (imlib-free-image-and-decache)
 (set! *imlib-image* #f)
 (clear-preview-snapshot!)
 (clear-video-name-caption!)
 ;;; processing states:
 (set! *processing-state* #f)
 (reset-navigation-states!)
 (set! *frame-number* #f)
 (set! *help-activated* #f))

;;; Camera operation

(define (viewfinder-running?) *viewfinder-visible*)

(define (capture-mode?) *capture-mode?*)

(define (camera-buttons-available?)
 (and (viewfinder-running?)
      (not (capture-mode?))
      (not (process-running?))))

(define (current-camera)
 (if (viewfinder-running?) (first *cameras*) '()))

(define (number-captured-frames camera)
 (hack-track-get-hack-track-i camera))

;; (define (captured-video-name) (dtrace "Captured video path is" *captured-video-name*))
(define (captured-video-name) *captured-video-name*)

(define (capture-buffer-size)
 ;; Total number of frames that can be captured at once
 1024)

(define (captured-video-fps)
 ;;; We always capture at the default frame rate.  The downsampling occurs in
 ;;; the get-video routine.
 *default-capture-fps*)

(define (captured-video-width)
 ;;; We always capture at the default resolution.  The downsampling occurs in
 ;;; the get-video routine.
 *default-capture-width*)

(define (captured-video-height)
 ;;; We always capture at the default resolution.  The downsampling occurs in
 ;;; the get-video routine.
 *default-capture-height*)

(define (initialize-hacktrack!)
 (v4l2-initialize)
 (set! *clear-display-pane?* #f)
 (hack-track-set-raw-frame-rate! *default-capture-fps*)
 (add-background-task-enabler!
  (lambda ()
   (hack-track-unblock)
   (hack-track-set-interrupts?! #t)))
 (add-background-task-disabler!
  (lambda ()
   (hack-track-block)
   (hack-track-set-interrupts?! #f)))
 (map-indexed
  (lambda (n i)
   (hack-track-startup-with-viewfinder-size n ; Camera
                                            0 ; x
                                            0 ; y
                                            *default-capture-width* ; width
                                            *default-capture-height* ; height
                                            (capture-buffer-size)
                                            1 ; subsample
                                            *default-capture-width* ; view-width
                                            *default-capture-height*)) ; view-height
  *cameras*)
 (map (lambda (n) (hack-track-turn-on-viewfinder! n 1)) *cameras*))

(define (startup-hacktrack!)
 (unless (null? *cameras*)
  (initialize-hacktrack!)
  (unless (viewfinder-running?)
   ;; Camera should be inactive when viewfinder is initially off
   (map (lambda (camera) (hack-track-turn-off! camera)) *cameras*))))

(define (shutdown-hacktrack!)
 (map hack-track-shutdown *cameras*))

(define (turn-viewfinder-on!)
 (when (viewfinder-running?) (panic "Viewfinder was already on"))
 (when (null? *cameras*) (panic "No cameras initialized"))
 (set! *viewfinder-visible* #t)
 (set! *frame-rate* (captured-video-fps))
 (for-each (lambda (camera) (hack-track-turn-off! camera)) *cameras*)
 (hack-track-turn-on-viewfinder! (current-camera) 1)
 (hack-track-exposed))

(define (turn-viewfinder-off!)
 (when (not (viewfinder-running?)) (panic "Viewfinder was /not/ already on"))
 (set! *viewfinder-visible* #f)
 (map (lambda (camera) (hack-track-turn-off! camera)) *cameras*)
 (xclearwindow *display* *display-pane*)
 (redraw-display-pane))

(define (capture! camera)
 (let ((n-frames (inexact->exact (round (* *n-seconds-to-capture* *frame-rate*)))))
  ;; Check n-frames is in bounds
  (when (<= n-frames 0) (panic "Cannot capture 0 or less frames"))
  (when (> n-frames (capture-buffer-size))
   (panic "Cannot capture ~a or more frames" (capture-buffer-size)))
  (hack-track-turn-on-viewfinder! camera 1)
  ;; 3-2-1!
  (user-feedback! "3")
  (niffty-bell 980000)
  (user-feedback! "2")
  (niffty-bell 980000)
  (user-feedback! "1")
  (niffty-bell 980000)
  (niffty-bell 400000)
  (niffty-bell 125000)
  (user-feedback! (format #f "Capturing ~as at ~a fps (~ax~a)"
                          *n-seconds-to-capture*
                          *frame-rate*
                          *default-capture-width*
                          *default-capture-height*))
  (hack-track-capture-frames
   camera
   (inexact->exact 0)
   (inexact->exact (round (/ *hack-track-raw-frame-rate* *frame-rate*)))
   n-frames)
  (hack-track-turn-on-viewfinder! camera 1)
  ;; Make sure we are displaying the very last available frame...
  (hack-track-show-frame (current-camera)
                         (- (number-captured-frames (current-camera)) 1))
  (xflush *display*)
  (niffty-bell 1000)
  (user-feedback! "Capture complete"))
 (save-video-command!))

(define (play-video-command!)
 (user-feedback! "")
 (unless (viewfinder-running?) (panic "Cannot save when viewfinder is off"))
 (let ((n-frames (number-captured-frames (current-camera))))
  (when (= n-frames 0) (user-warning! "No video has been captured"))
  (when (> n-frames 1)
   (user-feedback!
    (format #f "Playing back ~a frames at ~a fps" n-frames *frame-rate*))
   (for-each
    (lambda (idx)
     (hack-track-show-frame (current-camera) idx)
     (xflush *display*)
     (usleep (inexact->exact (round (/ 1000000.0 *frame-rate*)))))
    (enumerate n-frames))
   (user-feedback! "Playback complete")
   (redraw-display-pane))))

(define (save-video-command!)
 (user-feedback! "")
 (unless (viewfinder-running?) (panic "Cannot save when viewfinder is off"))
 (with-temporary-file
  "process-video"
  (lambda (tmp-filename)
   ;; Create a temporary directory for movie-frame files
   (system (format #f "mktemp -d /dev/shm/process-video.XXXXX > ~a"
                   tmp-filename))
   (let ((tmp-folder (first (read-file tmp-filename)))
         (output-filename (format #f "/tmp/video-to-sentences_capture_~a_~a.avi"
                                  (username)
                                  *save-captured-video-filename-index*))
         (n-frames (number-captured-frames (current-camera))))
    (when (= n-frames 0) (user-warning! "No video has been captured"))
    (when (> n-frames 1)
     (user-feedback! "Saving video to disk ...")
     (for-each
      (lambda (frame-number)
       ;; Save each frame to ram-disk
       (system (format #f "printf %s/%08d.png \"~a\" ~a > ~a"
                       tmp-folder
                       frame-number
                       tmp-filename))
       (let ((save-filename (first (read-file tmp-filename)))
       	     (imlib-image (hack-track-frame->imlib (current-camera)
       						   frame-number)))
	(when #f
	 (user-feedback! (format #f "Saving frame ~a to temporary file ~a"
				 frame-number
				 save-filename)))
       	(imlib-context-set-image! imlib-image)
       	;; This actually causes a seg-fault when, after quiting the gui
       	;; you attempt to reload it without also quitting dsci.
       	;; Presumably imlib leaves some stale resources lying around.
       	(imlib-save-image save-filename)
       	(imlib-free-image)))
      (enumerate n-frames)))
    ;; Create the movie
    (let* ((ffmpeg-cmd
	    (format #f "ffmpeg -y -r ~a -i ~a/%08d.png -vcodec libx264 -vb 3000k -vpre default -s ~ax~a ~a ~a"
		    (captured-video-fps)
		    tmp-folder
		    (captured-video-width)
		    (captured-video-height)
		    output-filename
		    (if (verbose?)
			"" ; Don't disable output
                        "-loglevel quiet 2>/dev/null")))
	   (ffmpeg-cmd-master-copy
	    (format #f "ffmpeg -y -r ~a -i ~a/%08d.png -vcodec libx264 -vb 3000k -vpre default \"~a/captured-video_$(date '+%F_%T').avi\" ~a"
		    (captured-video-fps)
		    tmp-folder
		    (if (video-destination-directory)
			(video-destination-directory)
			"/tmp")
		    (if (verbose?)
			"" ; Don't disable output
                        "-loglevel quiet 2>/dev/null"))))
     (when #f
      (user-feedback! (format #f "Executing: ~a" ffmpeg-cmd)))
     (system ffmpeg-cmd)
     (when #f
      (user-feedback! (format #f "Executing: ~a" ffmpeg-cmd-master-copy)))
     (system ffmpeg-cmd-master-copy)
     ;; Create a thunk to delete this movie on exit
     (add-on-exit-thunk!
      (lambda ()
       (let ((cmd-text (format #f "rm -f ~a" output-filename)))
        (user-feedback! (format #f "Executing: ~a" cmd-text))
        (system cmd-text))))
     (set! *captured-video-name* output-filename)
     (user-feedback! (format #f "Done"))
     (when #f
      (user-feedback!
       (format #f "Encoding complete, movie file saved to ~a" output-filename))))
    ;; Update file-increment
    (set! *save-captured-video-filename-index*
          (+ *save-captured-video-filename-index* 1))
    ;; Delete temporary folder
    (system (format #f "rm -rf ~a" tmp-folder))))))

;;; Camera buttons

(define (adjusting-camera?) *adjusting-camera?*)

(define (display-camera-off-message)
 (when (verbose?) (user-warning! "Camera is not turned on")))

(define (toggle-viewfinder-command!)
 (user-feedback! "")
 (if (null? *cameras*)
     (user-warning! "No camera initialized")
     (begin
      (if (viewfinder-running?)
          (turn-viewfinder-off!)
          (turn-viewfinder-on!))
      (redraw-buttons)
      (redraw-display-pane))))

(define (video-capture-command!)
 (user-feedback! "")
 (let ((camera (current-camera)))
  (set! *capture-mode?* #t)
  (redraw-buttons)
  (capture! camera)
  (hack-track-turn-off! camera)
  (set! *capture-mode?* #f)
  (redraw-buttons)
  (redraw-display-pane)
  (user-feedback! "Done")
  ;; Select the video as source:
  (set-to-captured-video!)
  (display-selected-video-info "Captured")
  (refresh-gui-after-video-selection)
  ;; Restore the viewfinder
  (for-each (lambda (camera) (hack-track-turn-off! camera)) *cameras*)
  (for-each (lambda (camera) (hack-track-turn-on-viewfinder! camera 1))
            *cameras*)))

(define (camera-with-hacktrack-paused-operation thunk)
 (if (not (viewfinder-running?))
     (display-camera-off-message)
     (begin
      (user-feedback! "")
      (with-hack-track-paused
       (lambda ()
        (thunk)
        (redraw-buttons))))))

(define (camera-toggle-adjusting! camera)
 (with-hack-track-paused
  (lambda ()
   (if (adjusting-camera?)
       (begin
        (camera-set-auto-white-balance! camera 1)
        ;;(camera-set-exposure-auto! camera 1)
        (camera-set-backlight-compensation! camera 1)
        (set! *adjusting-camera?* #f))
       (begin
        (camera-set-auto-white-balance! camera 0)
        ;;(camera-set-exposure-auto! camera 0)
        (camera-set-backlight-compensation! camera 0)
        (set! *adjusting-camera?* #t))))))

(define (camera-toggle-adjusting-command!)
 (user-feedback! "")
 (cond
  ((not (camera-buttons-available?)) (display-camera-off-message))
  (else (camera-toggle-adjusting! (current-camera))))
 (redraw-buttons))

(define (camera-adjusting-label)
 (cond
  ((not (camera-buttons-available?)) "-")
  ((adjusting-camera?) "Adjust off")
  (else "Adjust on")))

(define (camera-button-op-action! op v4l2-cid)
 (lambda ()
  (camera-with-hacktrack-paused-operation
   (lambda ()
    (let* ((current-value (camera-get-control (current-camera) v4l2-cid))
           (step (control-step (current-camera) v4l2-cid))
           (proposed-value (op current-value (* 10 step))))
     (cond
      ((< proposed-value 0)
       (user-warning! "Cannot decrement below 0"))
      ((> proposed-value 255)
       (user-warning! "Cannot increment above 255"))
      (else
       (camera-set-control! (current-camera)
                            v4l2-cid
                            proposed-value))))))))

(define (camera-tilt-reset-action!)
 (camera-with-hacktrack-paused-operation
  (lambda () (camera-tilt-reset (current-camera)))))

(define (camera-pan-reset-action!)
 (camera-with-hacktrack-paused-operation
  (lambda () (camera-pan-reset (current-camera)))))

(define (camera-tilt-action! n)
 (camera-with-hacktrack-paused-operation
  (lambda () (camera-tilt (current-camera) n))))

(define (camera-pan-action! n)
 (camera-with-hacktrack-paused-operation
  (lambda () (camera-pan (current-camera) n))))

(define (camera-plus-label text)
 (cond
  ((not (camera-buttons-available?)) "-")
  (else (format #f "~a" text))))

(define (camera-minus-label v4l2-cid text)
 (cond
  ((not (camera-buttons-available?)) "-")
  (else (format #f "~a ~a" (camera-get-control (current-camera) v4l2-cid) text))))

(define (define-camera-buttons r)
 (define-button 0 r
  (lambda ()
   (cond
    ((process-running?) "-")
    ((capture-mode?) "-")
    ((is-playing?) "-")
    ((null? *cameras*) "No Camera")
    ((viewfinder-running?) "Camera Off")
    (else "Camera On"))) #f
    (lambda ()
     (if (or (process-running?)
             (capture-mode?)
             (is-playing?))
         (action-not-availiable)
         (begin
          (toggle-viewfinder-command!)
          (clear-composite-image-with-snapshot-and-caption!)
	  (xclearwindow *display* *display-pane*)
          (redraw-display-pane)))))
 ;; Capure button
 (define-button 0 (+ 1 r)
  (lambda ()
   (cond
    ((not (viewfinder-running?)) "-")
    (else "Capture")))
  #f
  (lambda ()
   (cond
    ((not (camera-buttons-available?)) (display-camera-off-message))
    (else (video-capture-command!)))))
 ;; Adjusting
 (define-button 1 r
  (lambda () (camera-adjusting-label)) #f
  (lambda () (camera-toggle-adjusting-command!)))
 ;; Brightness
 (define-button 2 r
  (lambda () (camera-plus-label "Bright +  ")) #f
  (camera-button-op-action! + v4l2-cid-brightness))
 (define-button 2 (+ 1 r)
  (lambda () (camera-minus-label v4l2-cid-brightness "-")) #f
  (camera-button-op-action! - v4l2-cid-brightness))
 ;; Contrast
 (define-button 3 r
  (lambda () (camera-plus-label "Contrast +")) #f
  (camera-button-op-action! + v4l2-cid-contrast))
 (define-button 3 (+ 1 r)
  (lambda () (camera-minus-label v4l2-cid-contrast "-")) #f
  (camera-button-op-action! - v4l2-cid-contrast))
 ;; Saturation
 (define-button 4 r
  (lambda () (camera-plus-label "Sat. +")) #f
  (camera-button-op-action! + v4l2-cid-saturation))
 (define-button 4 (+ 1 r)
  (lambda () (camera-minus-label v4l2-cid-saturation "-")) #f
  (camera-button-op-action! - v4l2-cid-saturation))
 ;; Gain
 (define-button 5 r
  (lambda () (camera-plus-label "Gain +")) #f
  (camera-button-op-action! + v4l2-cid-gain))
 (define-button 5 (+ 1 r)
  (lambda () (camera-minus-label v4l2-cid-gain "-")) #f
  (camera-button-op-action! - v4l2-cid-gain))
 ;; Sharpness
 (define-button 6 r
  (lambda () (camera-plus-label "Sharpness +")) #f
  (camera-button-op-action! + v4l2-cid-sharpness))
 (define-button 6 (+ 1 r)
  (lambda () (camera-minus-label v4l2-cid-sharpness "-")) #f
  (camera-button-op-action! - v4l2-cid-sharpness))
 ;; Tilt
 (define-button 7 r
  (lambda () (camera-plus-label "Tilt +")) #f
  (lambda () (camera-tilt-action! 150)))
 (define-button 7 (+ 1 r)
  (lambda () (camera-plus-label "Tilt -")) #f
  (lambda () (camera-tilt-action! -150)))
 ;; Tilt reset
 (define-button 7 (+ 2 r)
  (lambda () (camera-plus-label "Tilt reset")) #f
  (lambda () (camera-tilt-reset-action!)))
 ;; Pan
 (define-button 8 r
  (lambda () (camera-plus-label "Pan +")) #f
  (lambda () (camera-pan-action! 150)))
 (define-button 8 (+ 1 r)
  (lambda () (camera-plus-label "Pan -")) #f
  (lambda () (camera-pan-action! -150)))
 ;; Pan reset
 (define-button 8 (+ 2 r)
  (lambda () (camera-plus-label "Pan reset")) #f
  (lambda () (camera-pan-reset-action!)))
 ;; Redraw
 (redraw-buttons))

(define (define-process-play-and-navigation-buttons r)
 (define-button 1 r
  (lambda () (if (show-process-button?) "Process" "-"))
  #f (lambda () (call-process-button!)))
 (define-button 2 r
  (lambda () (if (show-stop-button?) "Stop" "-"))
  #f (lambda () (call-stop-button!)))
 (define-button 3 r
  (lambda ()
   (cond ((is-playing?) "Stop")
         ((show-play-buttons?) "Play")
         (else "-")))
  #f (lambda () (call-play-button!)))
 (define-button 4 r
  (lambda () (if (show-navigation-buttons?) "Beginning" "-"))
  #f (lambda () (call-beginning-button!)))
 (define-button 5 r
  (lambda () (if (show-navigation-buttons?) "Previous" "-"))
  #f (lambda () (call-previous-button!)))
 (define-button 6 r
  (lambda () (if (show-navigation-buttons?) "Next" "-"))
  #f (lambda () (call-next-button!)))
 (define-button 7 r
  (lambda () (if (show-navigation-buttons?)
	    (if (eq? #f *video-length*)
		(format #f "End")
		(format #f "End ~a/~a" *frame-number* *video-length*))
	    "-"))
  #f (lambda () (call-end-button!))))


(define (show-demo-buttons?)
 (and (or (not (process-running?))
	  (not (video-selected?)))
      (not (or (viewfinder-running?)
	       (is-playing?)))))

(define (show-cmd-demo-buttons?)
 (and *cmd-parameters*
      (global-option-video-path *cmd-parameters*)
      (show-demo-buttons?)))

(define (show-captured-demo-buttons?)
 (and (captured-video-name)
      (show-demo-buttons?)))

(define (define-demo-buttons r)
 (map (lambda (label-name show-button? callback idx)
       (define-button idx r
        (lambda () (if (show-button?) label-name "-")) #f
        (lambda () (if (show-button?)
		  (begin
		   (user-feedback! "Looking for video ...")
		   (if (callback)
		       (display-selected-video-info label-name)
		       (begin
			;; (clean-up-gui-states!)
			(clear-preview-snapshot!)
			(clear-video-name-caption!)
			(clear-transcript-pane!)))
		   (refresh-gui-after-video-selection))
		  (action-not-availiable)))))
      (list "Cmd Line" "Captured"     "Demo 1"
            "Demo 2"       "Demo 3"   "Demo 4"
            "Demo 5"       "Demo 6"   "Demo 7")
      (list show-cmd-demo-buttons? show-captured-demo-buttons?
            show-demo-buttons? show-demo-buttons? show-demo-buttons?
            show-demo-buttons? show-demo-buttons? show-demo-buttons?
            show-demo-buttons?)
      (list set-to-command-line-video! set-to-captured-video!
            set-to-demo1-video! set-to-demo2-video! set-to-demo3-video!
            set-to-demo4-video! set-to-demo5-video! set-to-demo6-video!
            set-to-demo7-video!)
      '(0 1 2 3 4 5 6 7 8)))

(define (call-help-button!)
 (debug-message! (format #f "**** calling help..."))
 (if (viewfinder-running?)
     (message "Help not available under Viewfinder")
     (begin
      (message "") (set! *help-activated* #t)
      (clear-composite-image-with-snapshot-and-caption!)
      (redraw-display-pane)
      (help-command))))

;;; Handwritten event classifier for toys demo.

(define (compute-pseudo-likelihoods video named-tracks models medoids
				    likelihoods-cache-f unordered-pairs?)
 (if (= (length named-tracks) 2)
     (let* ((agent-track
	     (second
	      (find-if
	       (lambda (named-track) (string=? (first named-track) "person"))
	       named-tracks)))
	    (patient-named-track
	     (find-if-not
	      (lambda (named-track) (string=? (first named-track) "person"))
	      named-tracks))
	    (patient-track (second patient-named-track))
	    (patient-object-class (first patient-named-track))
	    (agent-centers (map voc4-detection-center agent-track))
	    (patient-centers (map voc4-detection-center patient-track))
	    (agent-velocities
	     (map v- (but-last agent-centers) (rest agent-centers)))
	    (patient-velocities
	     (map v- (but-last patient-centers) (rest patient-centers)))
	    (agent-patient-distances
	     (map distance agent-centers patient-centers))
	    (average-agent-velocity (list-mean agent-velocities))
	    (average-patient-velocity (list-mean patient-velocities)))
      (write (list 'debugging
		   average-agent-velocity
		   (magnitude average-agent-velocity)
		   (radians->degrees (orientation average-agent-velocity))
		   average-patient-velocity
		   (magnitude average-patient-velocity)
		   (radians->degrees (orientation average-patient-velocity))
		   (list-mean (take 5 agent-patient-distances))
		   (list-mean (take 5 (reverse agent-patient-distances)))))
      (newline)
      (list
       (cons
	(make-result
	 video
	 (list "person" patient-object-class)
	 'non-pose-no-x
	 #f				;states
	 (cond
	  ((< (magnitude average-patient-velocity) 2)
	   (if (> (magnitude average-agent-velocity) 2)
	       (if (< (list-mean (take 5 agent-patient-distances))
		      (list-mean (take 5 (reverse agent-patient-distances))))
		   "leave"
		   "approach")
	       "touch"))
	  ((< (/ pi 4) (orientation average-patient-velocity) (* 3 (/ pi 4)))
	   "pick-up")
	  ((< (- (* 3 (/ pi 4)))
	      (orientation average-patient-velocity)
	      (- (/ pi 4)))
	   "put-down")
	  (else "carry"))
	 1
	 named-tracks)
	(map (lambda (verb)
	      (make-result video
			   (list "person" patient-object-class)
			   'non-pose-no-x
			   #f		;states
			   verb
			   0
			   named-tracks))
	     (list "pick-up" "put-down" "carry" "touch" "approach" "leave")))))
     '()))

;;; Main Window
(define-application viewer
 (* 3 *pane-width*)
 (+ (* 3 *pane-height*) *caption-height*)
 1
 5
 9
 ;;; Pre-initialize procedure:
 (lambda ()
  (imlib-context-disconnect-display)
  (startup-hacktrack!)
  (set! *clear-display-pane?* #f)
  (recache-strings!)
  (standard-buttons 9 (lambda () #f))
  (define-key (control #\h) "Help" call-help-button!)
  (define-button 0 0 "Help" #f call-help-button!)
  (define-process-play-and-navigation-buttons 0)
  (define-demo-buttons 1)
  (define-camera-buttons 2)
  (define-key (meta #\<) "Beginning" call-beginning-button!)
  (define-key (control #\b) "Previous" call-previous-button!)
  (define-key (control #\f) "Next" call-next-button!)
  (define-key (meta #\>) "End" call-end-button!)
  (for-each (lambda (label-name show-button? callback key)
	     (define-key key label-name
	      (lambda ()
	       (if (show-button?)
		   (begin
		    (user-feedback! "Looking for video ...")
		    (if (callback)
			(display-selected-video-info label-name)
			(begin
			 ;; (clean-up-gui-states!)
			 (clear-preview-snapshot!)
			 (clear-video-name-caption!)
			 (clear-transcript-pane!)))
		    (refresh-gui-after-video-selection))
		   (action-not-availiable)))))
	    (list "Cmd Line" "Captured"     "Demo 1"
		  "Demo 2"       "Demo 3"   "Demo 4"
		  "Demo 5"       "Demo 6"   "Demo 7")
	    (list show-cmd-demo-buttons? show-captured-demo-buttons?
		  show-demo-buttons? show-demo-buttons? show-demo-buttons?
		  show-demo-buttons? show-demo-buttons? show-demo-buttons?
		  show-demo-buttons?)
	    (list set-to-command-line-video! set-to-captured-video!
		  set-to-demo1-video! set-to-demo2-video! set-to-demo3-video!
		  set-to-demo4-video! set-to-demo5-video! set-to-demo6-video!
		  set-to-demo7-video!)
	    (list (meta #\l)
		  (meta #\C)
		  (meta #\1)
		  (meta #\2)
		  (meta #\3)
		  (meta #\4)
		  (meta #\5)
		  (meta #\6)
		  (meta #\7)))
  (define-key (meta (control #\p)) "Process" call-process-button!)
  (define-key (control #\p) "Play" call-play-button!)
  (define-key (meta #\s) "Stop" call-stop-button!)
  (define-key (meta #\c) "Camera On/Off"
   (lambda ()
    (if (or (process-running?)
	    (capture-mode?)
	    (is-playing?))
	(action-not-availiable)
	(begin
	 (toggle-viewfinder-command!)
	 (clear-composite-image-with-snapshot-and-caption!)
	 (xclearwindow *display* *display-pane*)
	 (redraw-display-pane)))))
  (define-key (control #\c) "Capture"
   (lambda ()
    (cond
     ((not (camera-buttons-available?)) (display-camera-off-message))
     (else (video-capture-command!)))))
  (define-key (control #\d) "Demo"
   (lambda ()
    (cond
     ((not (camera-buttons-available?)) (display-camera-off-message))
     (else (video-capture-command!)))
    (if (or (process-running?)
	    (capture-mode?)
	    (is-playing?))
	(action-not-availiable)
	(begin
	 (toggle-viewfinder-command!)
	 (clear-composite-image-with-snapshot-and-caption!)
	 (xclearwindow *display* *display-pane*)
	 (redraw-display-pane)))
    (call-process-button!)))
  (initialize-background-task-enabler!)
  (initialize-background-task-disabler!)
  ;; TC: probabaly not the best place to put below
  (debug-message! "Checking command line video")
  (when *video*
   (set-to-command-line-video!)
   (display-selected-video-info "Command line")
   (refresh-gui-after-video-selection)))
 ;;; Post-initialize procedure:
 (lambda () #f)
 ;;; Finalize procedure:
 (lambda () (shutdown-hacktrack!))
 ;;; Redraw procedure:
 (lambda ()
  (xremove-expose-events)
  ;; The redraw procedure does not get called when the help menu is activated.
  ;; So when the redraw is called, we can assume we've already exited the help
  ;; menu.
  (if (viewfinder-running?)
      (let ((frame (hack-track-get-current-frame 0)))
       (for-each
	(lambda (camera) (hack-track-show-frame camera frame))
	*cameras*)
       (hack-track-exposed))
      (begin (draw-imlib-pixmap *imlib-image* 0 0)
	     (when *help-activated*
	      (set! *help-activated* #f)
	      (clear-composite-image-with-snapshot-and-caption!)))))
 ;;; Listener procedure:
 (lambda () (redraw-display-pane) (redraw-buttons) #f))

(define (run-from-gui! cameras verbose? parameters)
 (set! *quiet-mode?* (not verbose?))
 (cuklt-set-quiet! (not verbose?))
 (set-video-and-parameters! parameters)
 (debug-message! (format #f "video is ~a~%" *video*))
 (set! *cameras* cameras)
 (set! *imlib-image* (imlib-create-image
                      (* 3 *pane-width*)
                      (+ (* 3 *pane-height*) *caption-height*)))
 (clear-preview-snapshot!)
 (clear-composite-image-with-snapshot-and-caption!)
 (save-command-line-video-and-parameters! parameters)
 (viewer '())
 ;;; clean ups:
 (clean-up-gui-states!)
 (execute-on-exit-thunks!))

(define (run-from-command-line cameras verbose? parameters)
 ;;; Initializations:
 (set! *quiet-mode?* (not verbose?))
 (cuklt-set-quiet! (not verbose?))
 (set! *cameras* cameras) ;; TC: not used in command line version?
 (set-video-and-parameters! parameters)
 (unless *video*
  (panic "command-line version must provide a video")
  (fuck-up))
 ;;; Execution:
 (call-process-button!)
 (process-next-frame-with-callbacks)
 ;;; clean-ups:
 (execute-on-exit-thunks!))

(define (get-klt-choice c-klt? cuda-klt? precomputed-klt?)
 (cond (cuda-klt? "cuda")
       (c-klt? "c")
       (precomputed-klt? "precomputed")
       (else "auto")))

(define (get-optical-flow-choice cuda-optical-flow?
				 matlab-optical-flow?
				 precomputed-optical-flow?)
 (cond (cuda-optical-flow? "cuda")
       (matlab-optical-flow? "matlab")
       (precomputed-optical-flow? "precomputed")
       (else "auto")))

(define (get-detector-choice cuda-object-detector?
			     matlab-object-detector?
			     precomputed-object-detector?)
 (cond (cuda-object-detector? "cuda")
       (matlab-object-detector? "matlab")
       (precomputed-object-detector? "precomputed")
       (else "auto")))


(define-command
 (main
  (any-number ("m" model-names-groups?
               (model-names-groups "model-name-group" string-argument)))
  (any-number ("camera" cameras? (cameras "camera" integer-argument)))
  (at-most-one ("t" top-n? (top-n "top-n" integer-argument 12))
               ("no-t" no-t?))
  (at-most-one ("min-flow" min-flow? (min-flow "min-flow" real-argument 0)))
  (at-most-one ("darpa" darpa?))
  (at-most-one ("toy-demo" toy-demo?))
  (at-most-one ("downsample" downsample?
                (width "width" integer-argument 0)
                (height "height" integer-argument 0)
                (fps "fps" integer-argument 0)))
  (at-most-one
   ("cuda-object-detector" cuda-object-detector?
    (final-cascade-adjustment "final-cascade-adjustment" real-argument -1)
    (threshold-offset "threshold-offset" real-argument -0.05)
    (nms "nms" real-argument 0.7))
   ("matlab-object-detector" matlab-object-detector?
    (matlab-final-cascade-adjustment "final-cascade-adjustment" real-argument -1)
    (matlab-threshold-offset "threshold-offset" real-argument 0)
    (matlab-nms "nms" real-argument 0.7))
   ("precomputed-object-detector" precomputed-object-detector?))
  (at-most-one ("cuda-klt" cuda-klt?)
	       ("c-klt" c-klt?)
	       ("precomputed-klt" precomputed-klt?))
  (at-most-one ("cuda-optical-flow" cuda-optical-flow?)
	       ("matlab-optical-flow" matlab-optical-flow?)
	       ("precomputed-optical-flow" precomputed-optical-flow?))
  (at-most-one ("write-object-detector" write-object-detector?))
  (at-most-one ("write-klt" write-klt?))
  (at-most-one ("write-optical-flow" write-optical-flow?))
  (at-most-one ("write-tracker" write-tracker?)
	       ("precomputed-tracker" precomputed-tracker?))
  (at-most-one ("video-datasets-directory" video-datasets-directory?
		(video-datasets-directory "pathname" string-argument "")))
  (at-most-one ("alpha" alpha? (alpha "alpha" real-argument 3)))
  (at-most-one ("beta" beta? (beta "beta" real-argument 1e-2)))
  (at-most-one ("max-tracks" max-tracks? (max-tracks "nr" integer-argument 3)))
  (at-most-one ("suppression-delta" suppression-delta?
		(suppression-delta "suppression-delta" real-argument 0.1)))
  (at-most-one ("look-ahead" look-ahead?
                (look-ahead "size" integer-argument 2)))
  (at-most-one ("cuda-device" cuda-device?
                (cuda-device "number" integer-argument 0)))
  (at-most-one ("in-place" in-place?))
  (at-most-one ("model-path" model-path?
                (model-path "pathname" string-argument
                            (string-append
                             (getenv "HOME")
                             "/video-datasets/C-D1/voc4-models/"))))
  (at-most-one ("gui" gui?))
  (at-most-one ("mixed-forward-projection-mode" mixed-forward-projection-mode?)
	       ("klt-forward-projection-mode" klt-forward-projection-mode?)
	       ("optical-flow-forward-projection-mode"
		optical-flow-forward-projection-mode?))
  (at-most-one ("verbose" verbose?))
  (at-most-one ("stop-before-tracker" stop-before-tracker?))
  (at-most-one ("stop-after-tracker" stop-after-tracker?))
  (at-most-one ("rank-box-colors" rank-box-colors?))
  (exactly-one ("event-models-file"
                event-models-file?
                (event-models-file "pathname" string-argument "")))
  (at-most-one ("window-position"
                window-position?
                (window-position-x "x" integer-argument 0)
                (window-position-y "y" integer-argument 0)))
  (at-most-one ("panel-dimension"
                panel-dimension?
                (panel-width "width" integer-argument 320)
                (panel-height "height" integer-argument 180)))
  (at-most-one ("video-destination-directory"
		video-destination-directory?
		(video-destination-directory "string" string-argument #f)))
  (optional (video-path "video-path" string-argument #f)))
 (when min-flow? (set! *min-flow* min-flow))
 (when video-datasets-directory?
  (set! *video-pathname* video-datasets-directory))
 (set! *voc4-models-root* model-path)
 (set! *window-position?* #f)
 (when window-position?
  (set! *window-position?* #t)
  (set! *window-position-x* window-position-x)
  (set! *window-position-y* window-position-y))
 (when panel-dimension?
  (set! *pane-width* panel-width)
  (set! *pane-height* panel-height))
 (when mixed-forward-projection-mode? (set! *forward-projection-mode* 'mixed))
 (when klt-forward-projection-mode? (set! *forward-projection-mode* 'klt))
 (when optical-flow-forward-projection-mode?
  (set! *forward-projection-mode* 'optical-flow))
 (let* ((final-cascade-adjustment (if matlab-object-detector?
				      matlab-final-cascade-adjustment
				      final-cascade-adjustment))
	(threshold-offset (if matlab-object-detector?
			      matlab-threshold-offset threshold-offset))
	(nms (if matlab-object-detector? matlab-nms nms))
	(model-groups
	 (cond
	  (model-names-groups?
	   (map (lambda (group) (pregexp-split "," group))
		model-names-groups))
	  ((not (equal? #f video-path))
	   (annotated-models-for-video
	    (cond (darpa? (string->darpa-video video-path))
		  (else (make-stand-alone-video video-path)))))
	  (else #f)))
	(klt-choice (get-klt-choice c-klt? cuda-klt? precomputed-klt?))
	(optical-flow-choice (get-optical-flow-choice
			      cuda-optical-flow?
			      matlab-optical-flow?
			      precomputed-optical-flow?))
	(detector-choice (get-detector-choice cuda-object-detector?
					      matlab-object-detector?
					      precomputed-object-detector?)))
  (when (and video-path (or (not model-groups) (null? model-groups)))
   (panic "No models found and this video has no annotation"))
  (when verbose?
   (if (null? cameras)
       (format #t "No cameras attached~%")
       (format #t "Using camera /dev/video~a~%" (first cameras)))
   (if downsample?
       (format #t "Downsample ~ax~a@~a~%" width height fps)
       (format #t "Not downsampling~%")))
  ;; siddharth: accomodating a hack for finite-difference & temporal downsampling
  (when downsample? (set! *finite-difference-scale* (/ fps 30)))
  (when cuda-device? (set-cuda-device cuda-device))
  (set! *cuda-device* cuda-device)
  (apply (if gui? run-from-gui! run-from-command-line)
         (list cameras verbose?
               (make-global-option gui?
				   alpha beta look-ahead
		                   model-groups #f ;; with-dt?
				   model-path
				   event-models-file
				   #f ; event-models -- read from file
				   #f ; medoids -- read from file
		                   in-place? video-path darpa?
				   downsample?
				   width height fps
                                   final-cascade-adjustment threshold-offset nms
                                   (if no-t? infinity top-n)
                                   100000 ; max detections
                                   klt-choice optical-flow-choice detector-choice
                                   write-object-detector? write-klt?
                                   write-optical-flow? write-tracker?
                                   precomputed-tracker? stop-before-tracker?
				   stop-after-tracker? rank-box-colors?
				   video-destination-directory
				   suppression-delta
				   max-tracks
				   toy-demo?)))))

;;; Tam V'Nishlam Shevah L'El Borei Olam

;; (main '("XXX" "-verbose" "-in-place" "-camera" "0" "-gui" "-m" "person,person-down" "-m" "car" "-m" "suv" "-cuda-object-detector" "-0.1" "-0.5" "1" "-cuda-klt" "-cuda-optical-flow" "-event-models-file" "/home/abarbu/event-hmm/event-models.text" "-model-path" "/net/upplysingaoflun/aux/qobi/video-datasets/C-D1/voc4-models" "-darpa" "Chase1_A1_C1_Act1_4_PARK1_ML_MIDD_DARK_4433d840-c5af-11df-bed3-e80688cb869a"))

;; (main '("XXX"  "-verbose" "-in-place" "-m" "axe-3-8-cascade" "-model-path" "/home/amichaux/toy_videos"  "-event-models-file" "/home/abarbu/event-hmm/event-models.text" "/home/amichaux/toy_videos/captured-video_2012-01-26_14:58:59.avi"))

;; (main '("video-to-sentences" "-verbose" "-downsample" "320" "180" "10" "-in-place" "-model-path" "/net/upplysingaoflun/aux/qobi/video-datasets/c-d1/voc4-models" "-event-models-file" "/home/abarbu/event-hmm/event-models-200.text" "-write-object-detector" "-write-klt" "-write-optical-flow" "-write-tracker" "/aux/amichaux/tmp/video-datasets/friday-01/c-d1/recognition/flee9_a1_c2_act3_2_park_bl_aftn_dark_438f3992-1dc6-11e0-ad1b-e80688ca39a2.mov"))

;; (main '("video-to-sentences" "-verbose" "-downsample" "320" "180" "10" "-m" "person,person-down" "-m" "car" "-m" "suv" "-in-place" "-model-path" "/net/upplysingaoflun/aux/qobi/video-datasets/C-D1/voc4-models" "-event-models-file" "/home/abarbu/event-hmm/event-models.text" "-darpa" "APPROACH10_A1_C1_Act1_2_URBAN_ML_MORN_b41d318a-07b6-11e0-9a5b-e80688cb869a"))

					;(main '("/home/amichaux/jobs/pipline-on-new-models-01/bin/video-to-sentences" "-verbose" "-in-place" "-write-object-detector" "-write-klt" "-write-optical-flow" "-stop-before-tracker" "-cuda-object-detector" "0" "0" "0.7" "-cuda-klt" "-cuda-optical-flow" "-m" "umbrella-8-4-cascade" "-model-path" "/net/seykhl/aux/amichaux/models/ryan" "-event-models-file" "/home/abarbu/event-hmm/event-models.text" "/aux/amichaux/jobs/pipline-on-new-models-01/gun1.avi"))
