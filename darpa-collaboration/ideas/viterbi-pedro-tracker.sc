(MODULE
  VITERBI-PEDRO-TRACKER
  (WITH
    QOBISCHEME
    XLIB
    TOOLLIB-MATLAB
    TOOLLIB-MISC
    TOOLLIB-C-BINDINGS
    TOOLLIB-IMAGE-PROCESSING
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    IDEALIB-MATPLOTLIB
    HMM-WBM
    HMM-TRAIN-CLASSIFY)
  (MAIN MAIN))

(include "QobiScheme.sch")
(include "viterbi-pedro-tracker.sch")

(set! *program* "viterbi-pedro-tracker")
(set! *panic?* #f)

(define *dt* #f)
(define *box-movies* #f)
(define *large* 1e20)
(define (w v) (vector-ref v 3))
(define (min-movie-cost a b) (if (<= (first a) (first b)) a b))

(define (from-matlab matlab-expr)
 (matlab (format #f "ret=~a" matlab-expr)) (matlab-get-variable "ret"))
(define (vector-from-matlab matlab-expr) (x (from-matlab matlab-expr)))
(define (double-from-matlab matlab-expr) (x (x (from-matlab matlab-expr))))
(define (int-from-matlab matlab-expr) (inexact->exact (double-from-matlab matlab-expr)))

(define (pyramid-from-matlab var)
 (let ((l (int-from-matlab (format #f "length(~a)" var))))
  (map-n-vector (lambda (i) (from-matlab (format #f "~a{~a}" var (+ i 1)))) l)))

(define (infinite? n)
 ;; (not (number? n)) for reading precomputed file \-Inf
 (or (equal? infinity n) (equal? minus-infinity n)))

(define (read-pedro-model video frame label)
 (read-object-from-gzip-file
  (frame-model-pedro-scores-pathname video frame label)))

(define (virtpadding padding ds)
 ;; analogue to virtpadding in getdetections.cc
 (* padding (- (expt 2 ds) 1)))

(define (ip->box l i j val scale padding rules)
 (let* ((detwin (pedro-rule-detwindow
		 (find-if
		  (lambda (rule)
		   (equal?
		    (matrix-ref (vector-ref (pedro-rule-scores rule) l)
				(- i (virtpadding (x padding) 0))
				(- j (virtpadding (y padding) 0)))
		    val))
		  rules)))
	(x1 (* (- j (* (y padding) (expt 2 0))) scale))
	(y1 (* (- i (* (x padding) (expt 2 0))) scale)))
  (vector (+ x1 1) (+ y1 1) (+ x1 (* (y detwin) scale)) (+ y1 (* (x detwin) scale)))))

(define (get-displacement box transformation)
 ;; returns #(x y) in image coords
 (average-optical-flow
  transformation
  (bound (exact-round (/ (vector-ref box 0) 2)) 0 (- *optical-flow-width* 1))
  (bound (exact-round (/ (vector-ref box 2) 2)) 0 (- *optical-flow-width* 1))
  (bound (exact-round (/ (vector-ref box 1) 2)) 0 (- *optical-flow-height* 1))
  (bound (exact-round (/ (vector-ref box 3) 2)) 0 (- *optical-flow-height* 1))))

(define (update-dt-and-box-movies pyramid scales padding sbin rules
				  alpha dt box-movies transformation)
 (when (xor dt box-movies) (panic "get-dt-and-box-movies"))
 (let* ((max-row-size (matrix-rows (x pyramid)))
	(max-col-size (matrix-columns (x pyramid)))
	(info
	 (map-indexed-vector
	  (lambda (s l)
	   (let ((new-m (make-matrix max-row-size max-col-size (list *large* #f)))
		 (new-p (make-matrix (matrix-rows s) (matrix-columns s) 0))
		 (sr (/ max-row-size (matrix-rows s)))
		 (sc (/ max-col-size (matrix-columns s)))
		 (scale (/ sbin (vector-ref scales l))))
	    (for-each-indexed-matrix
	     (lambda (v i j)
	      (let* ((box (ip->box l i j v scale padding rules))
		     (displacement (get-displacement box transformation))
		     (new (vector (+ (/ (+ (x box) (x displacement)) scale)
				     (* (y padding) (expt 2 0)))
				  (+ (/ (+ (y box) (y displacement)) scale)
				     (* (x padding) (expt 2 0)))))
		     (new-i (exact-round (* (y new) sr)))
		     (new-j (exact-round (* (x new) sc)))
		     (val (if (infinite? v) *large* (- v)))
		     (num (* alpha val))
		     (ref (if dt (matrix-3d-ref (voc4-dt-position dt) l i j) #f))
		     (ref-pyr (if dt (matrix-3d-ref (voc4-dt-ref dt) (x ref) (y ref) (z ref)) #f))
		     (dist (if dt (matrix-3d-ref (voc4-dt-distance dt) l i j) #f)))
	       (if (and dt box-movies)
		   (matrix-set!
		    new-p i j
		    `(,(+ num dist)
		      ,(cons (append-vector box `#(,val))
			     (second (matrix-3d-ref box-movies (x ref-pyr) (y ref-pyr) (z ref-pyr))))))
		   (matrix-set! new-p i j (list num (list (append-vector box `#(,val))))))
	       (when (and (not (infinite? v))
			  (<= 0 (x new) (- (matrix-columns s) 1))
			  (<= 0 (y new) (- (matrix-rows s) 1))
			  (< (* alpha (- v)) (first (matrix-ref new-m new-i new-j))))
		(matrix-set! new-m new-i new-j (list (+ (* alpha (- v)) (if dt dist 0)) `#(,l ,i ,j))))))
	     s)
	    (vector new-m new-p)))
	  pyramid))
	(new-ms (map-vector x info))
	(dt (euclidean-3d-dt (map-matrix-3d first new-ms))))
  (set-voc4-dt-ref! dt (map-matrix-3d second new-ms))
  (vector dt (map-vector y info))))

(define (viterbi-pedro-track video label transformation-movie alpha)
 (unless (and (>= (length transformation-movie) 2)
	      (= (- (video-length video) 1) (+ (length transformation-movie) 1)))
  (panic "viterbi-pedro-track 1"))
 (set! *dt* #f)
 (set! *box-movies* #f)
 (let ((first-frame 1) (last-frame (- (video-length video) 1)) (result #f))
  (for-each-m-n
   (lambda (frame)
    (format #t "viterbi pedro track ~a~%" frame)
    ;; do scoring per-frame in matlab
    (matlab
     "im=double(ffmpegGetFrame());"
     "clear scored_models;"
     (format #f "scored_models=pedro_score_frame('~a',models,0.0,padx,pady,im);"
	     (generic-pathname video frame ""))
     "dummy_frame = ffmpegNextFrame();")
    (matlab "drop_top=scored_models{1}.interval;"
	    "pyramid=scored_models{1}.symbols(scored_models{1}.start).score;"
	    "pyramid=pyramid(drop_top+1:end);"
	    "sbin=scored_models{1}.sbin;"
	    "scales=scored_models{1}.pyra_scales;"
	    "scales=scales(drop_top+1:end);"
	    "padding=scored_models{1}.pyra_pad;"
	    "rules=scored_models{1}.rules(scored_models{1}.start);")
    (let* ((pyramid (pyramid-from-matlab "pyramid"))
	   (scales (vector-from-matlab "scales'"))
	   (padding (map-vector inexact->exact (vector-from-matlab "padding")))
	   (sbin (int-from-matlab "sbin"))
	   (interval (int-from-matlab "drop_top"))
	   (rules (map-n
		   (lambda (i)
		    (make-pedro-rule
		     #f #f #f
		     (vector-from-matlab (format #f "rules{1}(~a).detwindow" (+ i 1)))
		     #f #f #f #f
		     (subvector
		      (pyramid-from-matlab (format #f "rules{1}(~a).score" (+ i 1)))
		      interval
		      (int-from-matlab (format #f "length(rules{1}(~a).score)" (+ i 1))))))
		   (int-from-matlab "length(rules{1})"))))
     (matlab "clear pyramid rules scored_models;")
     (if (= frame last-frame)
	 (set! result
	       (reduce-matrix-3d
		min-movie-cost
		(map-indexed-matrix-3d
		 (lambda (v l i j)
		  ;; min -f + g -> max f - g
		  (let* (;; - f
			 (num (* alpha (if (infinite? v) *large* (- v))))
			 ;; recovering the box
			 (box (ip->box l i j v (/ sbin (vector-ref scales l)) padding rules))
			 ;; ?
			 (ref (matrix-3d-ref (voc4-dt-position *dt*) l i j))
			 ;; ?
			 (ref-pyr (matrix-3d-ref (voc4-dt-ref *dt*) (x ref) (y ref) (z ref)))
			 ;; + g
			 (dist (matrix-3d-ref (voc4-dt-distance *dt*) l i j)))
		   `(,(+ num dist)
		     ,(cons (append-vector box `#(,(if (infinite? v) *large* (- v))))
			    (second (matrix-3d-ref *box-movies* (x ref-pyr) (y ref-pyr) (z ref-pyr)))))))
		 pyramid)
		(list infinity #f)))
	 (let* ((transformation (list-ref transformation-movie (- frame 1)))
		(updated-dt-and-box-movies
		 (update-dt-and-box-movies pyramid scales padding sbin rules alpha
					   *dt* *box-movies* transformation)))
	  (set! *dt* (x updated-dt-and-box-movies))
	  (set! *box-movies* (y updated-dt-and-box-movies))))))
   first-frame
   last-frame)
  result))

(define (hog-pyramid-pathname video frame)
 (generic-pathname video frame "hog-pyramid.mat"))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))
		    ("stand-alone" stand-alone? (path "path" string-argument "")))
       (at-most-one ("alpha" alpha? (alpha "alpha" real-argument 10)))
       (at-most-one ("model-path" model-path? (model-path "model-path" string-argument "")))
       (at-most-one ("-with-label" with-label?)))
 (let* ((video (cond (standard?
		      (standard-corpus-video corpus sequence person location n))
		     (darpa? (string->darpa-video name))
		     (stand-alone? (make-stand-alone-video path))
		     (else (fuck-up))))
	(optical-flow-movie (read-optical-flow-movie-in-c video))
	(model-names (get-model-names-list video))
	(fixed-model-names
	 ;; (remove '("person" "person-crawl" "person-crouch" "person-down" "person-wheelbarrow")
	 ;;  model-names)
	 ;; ANDREI
	 ;; DEBUGGING
	 ;; (let* ((all-people
	 ;; 	 '("person" "person-crawl" "person-crouch" "person-down" "person-wheelbarrow"))
	 ;; 	(found (find all-people model-names)))
	 ;;  (if found (cons '("person") (remove all-people model-names)) model-names))
	 '(("person"))
	 ))
  (start-matlab!)
  (matlab "addpath('~/darpa-collaboration/pedro/detector');"
	  "addpath('~/darpa-collaboration/ffmpeg/');")
  (for-each-indexed
   (lambda (model-name i)
    (matlab (format #f "all_models{~a} = load('~a/~a.mat');" (+ i 1) model-path model-name)
	    (format #f "[pads(~a,1),pads(~a,2)]=getpadding(all_models{~a}.model);"
		    (+ i 1) (+ i 1) (+ i 1))))
   (reduce append fixed-model-names '()))
  ;; matlab: compute max padding over models
  (matlab "if numel(pads) <= 2; padding=pads; else; padding=max(pads); end;"
	  "padx=padding(1)"
	  "pady=padding(2)")
  (for-each
   (lambda (model-names)
    (matlab (format #f "ffmpegOpenVideo('~a');" (mov-pathname video))
	    "clear models")
    (write model-names) (newline)
    (for-each-indexed
     (lambda (model-name i)
      (matlab (format #f "models{~a} = load('~a/~a.mat');" (+ i 1) model-path model-name)))
     model-names)
    (let ((track (viterbi-pedro-track video (first model-names) optical-flow-movie alpha)))
     (write (first track)) (newline)
     (write-box-movie
      (map (lambda (b) (make-voc4-detection (x b) (y b) (z b) (w b) '() -1 (vector-ref b 4) #f #f))
	   (reverse (second track)))
      (tracked-box-pathname
       video
       (if with-label? (format #f "voc4_pedro-~a" alpha) "voc4_pedro")
       (first model-names) 1)))
    (matlab "ffmpegCloseVideo();"))
   fixed-model-names)
  ;; cleanup
  (for-each-frame (lambda (frame) (rm (hog-pyramid-pathname video frame))) video)
  (for-each free optical-flow-movie)
  (format #t "done~%")))
