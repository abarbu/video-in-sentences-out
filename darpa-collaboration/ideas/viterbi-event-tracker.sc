(MODULE
  VITERBI-EVENT-TRACKER
  (WITH
    QOBISCHEME
    XLIB
    TOOLLIB-MATLAB
    TOOLLIB-MISC
    TOOLLIB-C-BINDINGS
    TOOLLIB-IMAGE-PROCESSING
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    IDEALIB-MATPLOTLIB)
  (MAIN MAIN))

(include "QobiScheme.sch")
(include "viterbi-event-tracker.sch")

(set! *program* "viterbi-event-tracker")
(set! *panic?* #f)

(define-structure trained-hmm verb videos states log-likelihood model)
(define-structure psi name features kk parameters a b)
(define-structure cached-box id voc4 projections features)

(define *detection-polarity-fix* 5)

;; single-features    x y h/w d(h/w) v_r v_theta a_r a_theta area d(area) p
;; pairwise-features  c1->c2 d(c1->c2) theta(c1->c2) p(rt(c1,c2))

(define (single-features-new->old sf)
 ;; (sublist sf 0 8)
 sf
 )
(define (pairwise-features-new->old pf)
 ;; (sublist pf 0 3)
 pf
 )

;; will preserve order and execute each operation only once
;; meant for structure sharing not memoization, slow for large l
(define (map-duplicates1 f p l)
 (map
  car
  (sort
   (join
    (map (lambda (l)
	  (let ((value  (f (car (first l)))))
	   (map (lambda (e) (cons value (cdr e))) l)))
	 (transitive-equivalence-classesp
	  (lambda (a b) (equal? (p (car a)) (p (car b))))
	  (map-indexed cons l))))
   <
   cdr)))

(define (map3 f l1 l2 l3)
 (unless (and (= (length l1) (length l2)) (= (length l1) (length l3)))
  (panic "map3"))
 (map f l1 l2 l3))

(define (map3-reduce g i f l1 l2 l3)
 (unless (and (= (length l1) (length l2)) (= (length l1) (length l3)))
  (panic "map3-reduce"))
 (map-reduce g i f l1 l2 l3))

(define (map-reduceb g i f l1 l2)
 (if (null? l1)
     i
     (map-reduceb
      g
      (g i (f (car l1) (car l2)))
      f
      (cdr l1)
      (cdr l2))))

(define (get-single-features box-movies)
 (map
  (lambda (box-movie)
   (map
    single-features-new->old
    (matrix->list-of-lists
     (single-track-features (map cached-box-voc4 box-movie) 2))))
  box-movies))

(define (get-single-feature box-movie lookahead)
 (map
  single-features-new->old
  (matrix->list-of-lists (single-track-features box-movie lookahead))))

(define (get-single-and-pairwise-features box-movies)
 (let* ((lookahead 2)
	(box-movies (map (lambda (a) (take-if-possible (+ lookahead 1) a))
			 box-movies))
	(features-movies
	 (map (lambda (box-movie)
	       (let* ((id
	       	       (list
	       		(cached-box-id (first box-movie))
	       		(cached-box-id (second box-movie))
	       		(cached-box-id (third box-movie))))
	       	      (fs (cached-box-features (car box-movie)))
	       	      (r (find-if (lambda (f) (equal? (car f) id)) fs)))
	       	(unless r
		 (let ((f (cons id
				(get-single-feature
				 (map cached-box-voc4 box-movie)
				 lookahead))))
		  (set-cached-box-features!
		   (first box-movie)
		   (cons f fs))
		  (set! r f)))
	       	(cdr r)))
	      box-movies)))
  (map
   join
   (transpose-list-of-lists
    (append
     features-movies
     (map-all-pairs
      (lambda (v1 v2)
       (map
       	pairwise-features-new->old
       	(matrix->list-of-lists
       	 (pairwise-track-features (map cached-box-voc4 (second v1))
	 			  (map cached-box-voc4 (second v2))
	 			  lookahead))))
      (map list features-movies box-movies)))))))

(define (boxes-movie->box-movies boxes-movie)
 (transpose-list-of-lists boxes-movie))

(define (features->displacement-features number-of-movies features)
 (map-indexed
  (lambda (feature i)
   (if (< i (* 8 number-of-movies))
       (if (< (modulo i 8) 3) feature #f)
       (if (= (modulo i (* 8 number-of-movies)) 1) #f feature)))
  features))

(define (features->displacement-and-velocity-features number-of-movies features)
 (map-indexed
  (lambda (feature i)
   (if (< i (* 8 number-of-movies))
       (if (< (modulo i 8) 6) feature #f)
       feature))
  features))

(define (initial-feature-vector boxes)
 ;; boxes here is a list of objects
 ;; each object is a list of boxes, one for each object
 (let ((boxes-movie (list boxes boxes boxes boxes)))
  (list->vector
   (features->displacement-features
    (length boxes)
    (first (get-single-and-pairwise-features
	    (boxes-movie->box-movies boxes-movie)))))))

(define (pad-list-with-last l n)
 (if (and (not (null? l)) (< (length l) n))
     (let ((end (last l)))
      (append l (map-n (lambda (e) end) (- n (length l)))))
     l))

(define (feature-vector boxes-movie-so-far new-boxes)
 (let* ((boxes-movie (cons new-boxes boxes-movie-so-far))
	(l (length boxes-movie)))
  (list->vector
   (case l
    ((1) (panic "feature-vector: Length cannot be 1"))
    ((2) (features->displacement-and-velocity-features
	  (length new-boxes)
	  (first (get-single-and-pairwise-features
		  (boxes-movie->box-movies (pad-list-with-last boxes-movie 3))))))
    (else (first (get-single-and-pairwise-features
		  (boxes-movie->box-movies boxes-movie))))))))

(define (voc4-center a)
 (vector (/ (- (voc4-detection-x2 a) (voc4-detection-x1 a)) 2)
	 (/ (- (voc4-detection-y2 a) (voc4-detection-y1 a)) 2)))

(define (box-distance box1 box2)
 ;; This is what the Matlab code does but we believe that it should do
 ;; (distance (box->vector box1) (box->vector box2)).
 (let* ((b1 (voc4-center box1)) (b2 (voc4-center box2)))
  (- (sqrt (+ (sqr (- (x b2) (x b1)))
	      (sqr (- (y b2) (y b1))))))))

(define (box-pair-cost box1 transformation box2)
 (box-distance (forward-project-box box1 transformation 0) box2))

(define (box-cost box) (voc4-detection-strength box))

(define (log-gaussian v mu sigma)
 (unless (>= sigma 0.01) (panic "log-gaussian") (abort))
 (let ((x (- v mu)) (sqrt_2pi 2.506628))
  (- (/ (* -0.5 x x) (* sigma sigma))
     (log (* sigma sqrt_2pi)))))

(define (fold-polynomial x coeffs)
 (let loop ((s 0) (c (reverse coeffs)))
  (if (null? c)
      s
      (loop (* x (+ s (first c))) (rest c)))))

(define (log-bessel0 kappa)
 (let ((ax (abs kappa)))
  (if (< ax 3.75)
      (log
       (+ 1.0
	  (fold-polynomial (sqr (/ kappa 3.75))
			   '(3.5156229
			     3.0899424
			     1.2067492
			     0.2659732
			     0.360768e-1
			     0.45813e-2))))
      (+ (- ax (* 0.5 (log ax)))
	 (log
	  (+ 0.39894228
	     (fold-polynomial (/ 3.75 ax)
			      '(0.39894228
				0.1328592e-1
				0.225319e-2
				-0.157565e-2
				0.916281e-2
				-0.2057706e-1
				0.2635537e-1
				-0.1647633e-1
				0.392377e-2))))))))

(define (log-von-mises v mean kappa)
 (unless (and (> kappa 0.0)
 	      (and (>= mean (- pi)) (< mean pi))
 	      (and (>= v (- pi)) (< v (+ pi 1e-6))))
  (format #t "~a ~a ~a~%" v mean kappa)
  (panic "log-radial") (abort))
 (let ((x (- v mean)) (log_2pi 1.837877))
  (- (* kappa (cos x)) log_2pi (log-bessel0 kappa))))

(define (compute-features-cost features distributions)
 (reduce-vector
  +
  (remove-if-vector
   boolean?
   (map-vector
    (lambda (feature distribution)
     (if feature
	 ((case (first distribution)
	   ('CONTINUOUS log-gaussian)
	   ('RADIAL log-von-mises)
	   (else (fuck-up)))
	  feature
	  (second distribution)
	  (third distribution))
	 #f))
    features
    distributions))
  0))

(define (get-feature-costs states-features-matrix)
 (map-vector
  (lambda (state-features-distribution)
   (lambda (features)
    (compute-features-cost features state-features-distribution)))
  states-features-matrix))

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
			  (let ((ret ,(second form)))
			   (set! *time-depth* (- *time-depth* 1))
			   ret)))
	    expander)))

(define (map-all-tuples-list f l)
 (apply map-all-tuples f l))

(define (map-all-tuples f l . ls)
   (map f (all-values (nondeterministic-map a-member-of (cons l ls)))))

(define (viterbi-event-track
	 object-names
	 objects-movie
	 transformations-movie
	 initial-state-costs
	 state-transition-costs
	 feature-costs
	 alpha
	 beta)
 ;; object-names is a list of strings
 ;; objects-movie is a list of frames
 ;;  each frame is a list of objects, one per object name
 ;;  each object is a list of boxes
 ;; transformations-movie is a list of frames
 ;;  each frame is a list of transformations, one per object name
 (unless (and (>= (length objects-movie) 2)
	      (every
	       (lambda (objects) (= (length objects) (length object-names)))
	       objects-movie)
	      (= (length objects-movie) (+ (length transformations-movie) 1))
	      (= (vector-length initial-state-costs)
		 (vector-length feature-costs)
		 (matrix-rows state-transition-costs)
		 (matrix-columns state-transition-costs)))
  (panic "viterbi-event-track 1"))
 (let ((boxes-movies-state-movies-and-costs
	(time-code
	 (all-values
	   (let ((boxes (nondeterministic-map a-member-of (first objects-movie)))
		 (state (an-integer-between
			 0 (- (vector-length initial-state-costs) 1))))
	    (list (list boxes)
		  (list state)
		  (+ (* alpha (map-reduce + 0 (lambda (a) (box-cost (cached-box-voc4 a)))
				      boxes))
		     ;; Since we are adding instead of multiplying, the initial
		     ;; state cost must be in log space and beta is actually an
		     ;; exponent to the event cost.
		     (* beta
			(+ (vector-ref initial-state-costs state)
			   ((vector-ref feature-costs state)
			    (initial-feature-vector boxes)))))))))))
  (let loop ((frame 1)
	     ;; boxes-movies is a list of boxes-movies
	     ;;  each boxes-movie is a list of frames
	     ;;  each frame is a list of boxes, one per object name
	     (boxes-movies (map first boxes-movies-state-movies-and-costs))
	     (state-movies (map second boxes-movies-state-movies-and-costs))
	     (costs (map third boxes-movies-state-movies-and-costs))
	     (objects-movie (rest objects-movie))
	     (transformations-movie transformations-movie))
   (format #t "viterbi-event-track: ~s~%" (length objects-movie))
   (when (null? costs) (panic "viterbi-event-track 2"))
   (if
    (null? objects-movie)
    (let ((best-with-position (maximum-with-position costs)))
     (list (reverse (list-ref boxes-movies (second best-with-position)))
	   (reverse (list-ref state-movies (second best-with-position)))
	   (first best-with-position)))
    (let*
      ((transformation (first transformations-movie))
       (new-boxes-movies-state-movies-and-costs
	(time-code
	 (join
	  (map-all-tuples-list
	   (lambda (boxes)
	    (let ((boxes-cost
		   (* alpha (map-reduce + 0 (lambda (a) (box-cost (cached-box-voc4 a))) boxes))))
	     (map-n
	       (lambda (state)
		(let*
		  ((new-costs
		    (map3
		     (lambda (boxes-movie state-movie cost)
		      (+ cost
			 boxes-cost
			 (map-reduceb
			  +
			  0
			  ;; box-pair-cost
			  ;; fast cost 0.07 -> 0.30
			  (lambda (box1 box2)
			   (box-distance
			    ;; old slow code 11s
			    ;; (forward-project-box
			    ;;  (cached-box-voc4 box1)
			    ;;  transformation 0)
			    (let* ((p (cached-box-projections box1))
				   (r (find-if (lambda (a) (equal? (car a) transformation)) p)))
			     (unless r
			      (let ((f (cons transformation
					     (forward-project-box
					      (cached-box-voc4 box1)
					      transformation 0))))
			       (set-cached-box-projections! box1 (cons f p))
			       (set! r f)))
			     (cdr r))
			    (cached-box-voc4 box2)))
			  (first boxes-movie)
			  boxes)
			 ;; Since we are adding instead of multiplying, the
			 ;; state transition cost must be in log space and
			 ;; beta is actually an exponent to the event cost.
			 (* beta
			    (+ (matrix-ref state-transition-costs
					   (first state-movie)
					   state)
			       ((vector-ref feature-costs state)
				(feature-vector boxes-movie boxes))))))
		     boxes-movies state-movies costs))
		   (best-cost-and-position (maximum-with-position new-costs)))
		 (list
		  (cons boxes (list-ref boxes-movies (second best-cost-and-position)))
		  (list state)
		  (first best-cost-and-position))))
	      (vector-length initial-state-costs))))
	   (first objects-movie))))))
     (loop (+ frame 1)
	   (map first new-boxes-movies-state-movies-and-costs)
	   (map second new-boxes-movies-state-movies-and-costs)
	   (map third new-boxes-movies-state-movies-and-costs)
	   (rest objects-movie)
	   (rest transformations-movie)))))))

(define (fill-in-objects-movie objects-movie)
 (define (find-null-frames movie)
  (remove-if
   boolean?
   (map-indexed (lambda (boxes i) (if (null? boxes) i #f)) movie)))
 (transpose-list-of-lists
  (map
   (lambda (boxes-movie)
    (let loop ((bsm boxes-movie) (ps (find-null-frames boxes-movie)))
     (if (null? ps)
	 bsm
	 (loop (list-replace bsm (first ps) (list-ref bsm (- (first ps) 1)))
	       (rest ps)))))
   (transpose-list-of-lists objects-movie))))

(define (capitalise string)
 (string-join
  " "
  (map
   (lambda (w)
    (list->string
     (map-indexed (lambda (c i) (if (zero? i) (char-upcase c) c)) (string->list w))))
   (fields string))))

(define (get-appropriate-hmm-model path name number)
 (let ((fname1 (format #f "~a/hmm-~a-5-~a-0.sc" path number (string-downcase name)))
       (fname2 (format #f "~a/hmm-~a-5-~a-0.sc" path number name))
       (fname3 (format #f "~a/hmm-~a-5-~a.sc" path number name))
       (fname4 (format #f "~a/hmm-~a-5-~a.sc" path number (capitalise name))))
  (cond ((file-exists? fname1) (trained-hmm-model (read-object-from-file fname1)))
	((file-exists? fname2) (trained-hmm-model (read-object-from-file fname2)))
	((file-exists? fname3) (trained-hmm-model (read-object-from-file fname3)))
	((file-exists? fname4) (trained-hmm-model (read-object-from-file fname4)))
	(else (panic "hmm model name")))))

(define (normalize-if-possible-around v l)
 (let ((m1 (maximum l)) (m2 (minimum l)))
  (if (<= m1 v m2)
      (map
       (lambda (e)
	(* 0.5 (if (>= e v) (+ (/ (- e v) (- m1 v)) 1) (- 1 (/ (- v e) (- v m2))))))
       l)
      (map (lambda (e) (/ (- e m2) (- m1 m2))) l))))

(define (normalize-voc4-boxes-around boxes m1 m2 v)
 (let* ((p 0.3) (q (- 1 p)))
  (map
   (lambda (box)
    (let ((strength (voc4-detection-strength box)))
     (update-voc4-strength
      box
      (if (>= strength v)
	  (+ (* (/ (- strength v) (- m2 v)) p) q)
	  (- q (* (/ (- v strength) (- v m1)) q))))))
   boxes)))

(define (normalize-voc4-boxes boxes m1 m2)
 (map
  (lambda (box)
   (let ((strength (voc4-detection-strength box)))
    (update-voc4-strength
     box
     (/ (- strength m1) (- m2 m1)))))
  boxes))

(define (get-normalized-boxes video label)
 (let* ((model-thresh
	 (read-voc4-model-threshold
	  (format #f "~a/video-datasets/C-D1/voc4-models/~a.mat" (getenv "HOME") label)))
	(bs (read-voc4-detector-boxes video label))
	(flat-strengths (map voc4-detection-strength (join bs)))
	(m1 (minimum flat-strengths))
	(m2 (maximum flat-strengths)))
  (format #t "~a thresh: ~a~%" label model-thresh)
  (format #t " Min: ~a~% Max: ~a~%" m1 m2)
  (if (<= m1 model-thresh m2)
      (map (lambda (boxes) (normalize-voc4-boxes-around boxes m1 m2 model-thresh)) bs)
      (map (lambda (boxes) (normalize-voc4-boxes boxes m1 m2)) bs))))

(define (get-model-names video)
 (map
  (lambda (c) (remove-if (lambda (b) (or (suffix? "crawl" b) (suffix? "wheelbarrow" b))) c))
  (transitive-equivalence-classesp
   (lambda (a b) (and (prefix? "person" a) (prefix? "person" b)))
   (map second (video-voc4-detector-boxes-available video)))))

(define (iqr vs)
 (let ((svs (sort vs > identity))
       (l (length vs)))
  (- (list-ref svs (exact-round (/ l 4)))
     (list-ref svs (exact-round (* 3 (/ l 4)))))))

(define (make-histogram values bin-size)
 (let* ((s (sort values < identity))
	(n (exact-ceiling (/ (- (last s) (first s)) bin-size)))
	(v (make-vector n 0)))
  (for-each
   (lambda (val)
    (let ((pos (inexact->exact (quotient (- val (first s)) bin-size))))
     (vector-set! v pos (+ (vector-ref v pos) 1))))
   values)
  v))

(define (make-normalized-histogram values bin-size)
 (let ((h (make-histogram values bin-size)) (l (length values)))
  (map-vector (lambda (v) (/ v l)) h)))

(define (make-cumulative-histogram hist )
 (let loop ((h (subvector hist 1 (vector-length hist)))
	    (c `#(,(x hist))))
  (if (equal? '#() h)
      (reverse-vector c)
      (loop (subvector h 1 (vector-length h))
	    (vector-append `#(,(+ (x h) (x c))) c)))))

(define (otsu vs)
 (let* ((bin-size (* 2 (iqr vs) (expt (length vs) (- (/ 3)))))
	(hist (make-normalized-histogram vs bin-size))
	(fcm (make-cumulative-histogram (weighted-histogram hist)))
	(bcs (find-between-class-variances
	      (make-cumulative-histogram hist)
	      fcm
	      (vector-ref fcm (- (vector-length fcm) 1)))))
  (+ (minimum vs)
     (* bin-size (vector-position bcs (reduce-vector max bcs minus-infinity))))))

(define (otsu-hist hist)
 (let* ((fcm (make-cumulative-histogram (weighted-histogram hist)))
	(bcs (find-between-class-variances
	      (make-cumulative-histogram hist)
	      fcm
	      (vector-ref fcm (- (vector-length fcm) 1)))))
  (vector-position bcs (reduce-vector max bcs minus-infinity))))

(define (reverse-vector v)
 (list->vector (reverse (vector->list v))))

(define (draw-ascii-histogram h quantization)
 (for-each-vector
  (lambda (row)
   (for-each-vector (lambda (e) (format #t "~a " e)) row)
   (newline))
  (transpose
   (map-vector
    (lambda (val)
     (let ((filled (exact-round (* val quantization))))
      (append-vector (make-vector (- quantization filled) " ")
		     (make-vector filled "#"))))
    h))))

(define (clip-track box-movie)
 (let* ((strengths (map voc4-detection-strength box-movie))
	(bin-size (* 2 (iqr strengths) (expt (length strengths) (- (/ 3)))))
	(hist (make-normalized-histogram strengths bin-size)))
  (draw-ascii-histogram hist 20)
  (write strengths) (newline)
  (write bin-size) (newline)
  (write (coefficient-of-bimodality hist)) (newline)
  (format #t "Mean: ~a~%" (list-mean strengths))
  (format #t "StdDev: ~a~%" (sqrt (list-variance strengths)))
  (let* ((cutoff (if (>= (coefficient-of-bimodality hist) 0.83)
		     (+ (minimum strengths) (* bin-size (otsu-hist hist)))
		     ;; needs work
		     (- (list-mean strengths) (sqrt (list-variance strengths)))))
	 (dummy (make-voc4-detection -1 -1 -1 -1 '() -1 0 #f #f))
	 (from-start
	  (position-if (Lambda (box) (>= (voc4-detection-strength box) cutoff)) box-movie))
	 (from-end
	  (position-if
	   (Lambda (box) (>= (voc4-detection-strength box) cutoff)) (reverse box-movie))))
   (format #t "cutoff: ~a~%" cutoff)
   (format #t "s: ~a~%e: ~a~%" from-start from-end)
   (let ((foo (append (map-n (lambda (_) dummy) from-start)
		      (sublist box-movie
			       from-start
			       (- (length box-movie) from-end))
		      (map-n (lambda (_) dummy) from-end))))
    (write (length foo)) (newline)
    (write (length box-movie)) (newline)
    foo))))

(define (median l)
 (let ((sl (sort l < identity)) (mid-l (/ (length l) 2)))
  (if (integer? mid-l)
      (/ (+ (list-ref l mid-l) (list-ref l (- mid-l 1))) 2)
      (list-ref l (exact-floor mid-l)))))

(define-command (main
		 (exactly-one
		  ("standard" standard?
		   (corpus "corpus" string-argument "")
		   (sequence "sequence" string-argument "")
		   (person "person" string-argument "")
		   (location "location" string-argument "")
		   (n "n" integer-argument 0))
		  ("darpa" darpa? (name "name" string-argument ""))
		  ("stand-alone" stand-alone? (path "path" string-argument "")))
		 (exactly-one ("hmm-model-path"
			       hmm-model-path?
			       (hmm-model-path
				"path"
				string-argument
				"/aux/qobi/video-datasets/C-D1/hmm")))
		 (exactly-one ("hmm-model" hmm-model?
			       (hmm-model-name "name" string-argument "")
			       (hmm-model-number "number" integer-argument ""))
			      ("all-hmms" all-hmms?))
		 (at-most-one ("topn" topn?
			       (topn "number" integer-argument 5)))
		 (at-most-one ("alpha" alpha?
			       (alpha "number" real-argument 2.3)))
		 (at-most-one ("beta" beta?
			       (beta "number" real-argument 2.1)))
		 (any-number ("role" role?
			      (role-name "name" string-argument)
			      (role-number "number" integer-argument))))
 (let* ((video (cond
		(standard?
		 (standard-corpus-video corpus sequence person location n))
		(darpa? (string->darpa-video name))
		(stand-alone? (make-stand-alone-video path))
		(else (fuck-up))))
	(optical-flow-movie (read-optical-flow-movie-in-c video))
	(models
	 (cond ((all-hmms? (directory-list (string-append hmm-model-path "/hmm-1-*.sc")))
		(hmm-model?
		 (get-appropriate-hmm-model hmm-model-path hmm-model-name hmm-model-number))
		(else (fuck-up)))))
	(transformations-movie optical-flow-movie)
	(state-transition-costs (map-matrix log (psi-a model)))
	(initial-state-costs (map-vector log (psi-b model)))
	(feature-costs (get-feature-costs (psi-parameters model)))
	(object-names (map list role-name role-number))
	(raw-objects-movie
	 (transpose-list-of-lists
	  (map-duplicates1
	   (lambda (object-name)
	    (display object-name)(newline)
	    (let* ((labels (findp member (first object-name) (get-model-names video)))
		   (normalized-boxes-movies
		    (map (lambda (label) (get-normalized-boxes video label)) labels)))
	     (map-n
	      (lambda (i)
	       (let ((all (sort (map-reduce append
					    '()
					    (lambda (e) (list-ref e i))
					    normalized-boxes-movies)
				>
				voc4-detection-strength)))
		(if (and (= i (- (video-first-frame video) 1)) (null? all))
		    (list (make-voc4-detection -1 -1 -1 -1 '() -1 0 #f #f))
		    (if (and topn? (> (length all) topn)) (sublist all 0 topn) all))))
	      (length (first normalized-boxes-movies)))))
	   first
	   object-names)))
	(objects-movie
	 (let* ((filled (fill-in-objects-movie raw-objects-movie))
		(a-len (length filled))
		(b-len (length (first filled))))
	  (map-indexed
	   (lambda (boxes a)
	    (map-indexed
	     (lambda (boxes b)
	      (map-indexed
	       (lambda (box c)
		(make-cached-box (+ (* a a-len) (* b b-len) c) box '() '()))
	       boxes))
	     boxes))
	   filled)))
	(result (viterbi-event-track
		 object-names
		 objects-movie
		 transformations-movie
		 initial-state-costs
		 state-transition-costs
		 feature-costs
		 alpha
		 beta))
	(box-movies (transpose-list-of-lists (first result))))
  (format #t "states:~%~a~%" (second result))
  (format #t "Cost: ~a~%" (third result))
  (for-each-indexed
   (lambda (box-movie i)
    (let ((model-name (list-ref role-name i))
	  (model-number (number->string (list-ref role-number i))))
     ;; (clip-track (map cached-box-voc4 box-movie))
     (write-voc4-event-tracked-box-movie
      (clip-track (map cached-box-voc4 box-movie))
      ;; (map cached-box-voc4 box-movie)
      video model-name model-number)))
   box-movies)
  (for-each free optical-flow-movie)
  (format #t "done~%")))
