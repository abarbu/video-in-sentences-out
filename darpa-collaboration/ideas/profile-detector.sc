(MODULE
  PROFILE-DETECTOR
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
(include "profile-detector.sch")

(set! *program* "profile-detector")
(set! *panic?* #f)

(define *model-threshold-tracker-offset* -0.4)

(define (l-mean p)
 (if (vector? (car p))
     (k*v  (/ 1 (length p)) (reduce v+ p 0))
     (/ (reduce + p 0) (length p))))

(define (l-variance mu s)
 (/ (reduce + (map (lambda (s) (sqr (- s mu))) s) 0) (length s)))

(define (l-skewness mu sigma l)
 (/ (* (/ (length l)) (reduce + (map (lambda (e) (expt (-  e mu) 3)) l) 0))
    (expt sigma (/ 3 2))))

(define (l-kurtosis mu sigma l)
 (- (/ (* (/ (length l)) (reduce + (map (lambda (e) (expt (-  e mu) 4)) l) 0))
	(sqr sigma))
     3))

(define (structure-annotation a)
 (list (first a) (second a)
       (let loop ((l (drop 2 a)) (c '()))
	(if (null? l)
	    (reverse c)
	    (if (or (= (modulo (length l) 4) 0) (> (length l) 4))
		(loop (drop 4 l) (cons (take 4 l) c))
		(loop '() (cons l c)))))))

(define *annotations*
 (map-reduce
  union '()
  (lambda (f) (map (lambda (e) (pregexp-split "," e)) (read-file f)))
  '("/home/snarayan/darpa-collaboration/documentation/C-D1-recognition-annotations.csv"
    "/home/snarayan/darpa-collaboration/documentation/C-E1-recognition-annotations.csv"
    "/home/snarayan/darpa-collaboration/documentation/C-E1-description-annotations.csv")))

(define (annotated-models video)
 (let ((name (darpa-video->string video)))
  (map first (third (structure-annotation (find-if (lambda (e) (equal? name (first e))) *annotations*))))))

(define (test-vector model-path feature)
 (scheme->matlab! "v" feature)
 (matlab (format #f "svmstruct=load('~a');" model-path)
	 "c=svmclassify(svmstruct,v);")
 (inexact->exact (matlab-get-double "c")))

(define-command
 (main
  (exactly-one ("standard" standard?
		(corpus "corpus" string-argument "")
		(sequence "sequence" string-argument "")
		(person "person" string-argument "")
		(location "location" string-argument "")
		(n "n" integer-argument 0))
	       ("darpa" darpa? (name "name" string-argument ""))
	       ("stand-alone" stand-alone? (path "path" string-argument "")))
  (exactly-one ("train" train?)
	       ("test" test?
		(classifier-path "classifier-path" string-argument "")
		(classifier-type "classifier-type" string-argument "")))
  (exactly-one ("output" output? (output "path" string-argument "")))
  (exactly-one ("model-path" model-path? (model-path "model-path" string-argument ""))))
 (let* ((video (cond
		(standard?
		 (standard-corpus-video corpus sequence person location n))
		(darpa? (string->darpa-video name))
		(stand-alone? (make-stand-alone-video path))
		(else (fuck-up))))
	(amodels (annotated-models video)))
  (write name) (newline)
  (start-matlab!)
  (call-with-output-file output
   (lambda (port)
    (for-each
     (lambda (model-names)
      (let ((detector-boxes-movie-model-name-pairs
	     (removeq
	      #f
	      (map
	       (lambda (model-name)
		(let ((detector-boxes-movie (read-voc4-detector-boxes video model-name)))
		 (if (every null? detector-boxes-movie)
		     #f
		     (list detector-boxes-movie model-name))))
	       model-names))))
       (if (null? detector-boxes-movie-model-name-pairs)
	   (format #t "~a has no detections~%" model-names)
	   (let*
	     ((detector-boxes-movies
	       (map first detector-boxes-movie-model-name-pairs))
	      (model-names (map second detector-boxes-movie-model-name-pairs))
	      (model-name (first model-names))
	      (model-thresholds (map (lambda (model-name)
				      (model-threshold model-name model-path))
				     model-names))
	      (thresholds
	       (map (lambda (detector-boxes-movie model-threshold)
		     (min (matlab-threshold-otsu
			   (map-reduce append
				       '()
				       (lambda (a) (map voc4-detection-strength a))
				       detector-boxes-movie))
			  (+ model-threshold *model-threshold-tracker-offset*)))
		    detector-boxes-movies
		    model-thresholds))
	      (max-threshold (reduce max thresholds minus-infinity))
	      (detector-boxes-movies
	       (map
		(lambda (detector-boxes-movie threshold)
		 (let ((delta (- max-threshold threshold)))
		  (map (lambda (detector-boxes)
			(map (lambda (detector-box)
			      (update-voc4-strength
			       detector-box
			       (+ (voc4-detection-strength detector-box) delta)))
			     detector-boxes))
		       detector-boxes-movie)))
		detector-boxes-movies thresholds))
	      (detector-boxes-movie
	       (map-n
		(lambda (i)
		 (sort (map-reduce append
				   '()
				   (lambda (detector-boxes-movie)
				    (list-ref detector-boxes-movie i))
				   detector-boxes-movies)
		       >
		       voc4-detection-strength))
		(length (first detector-boxes-movies)))))
	    (let* (;; (dbm (remove-if null? detector-boxes-movie))
		   ;; (strengths (map-reduce append '() (lambda (c) (map voc4-detection-strength c)) dbm))
		   (strengths (map voc4-detection-strength (reduce append detector-boxes-movie '())))
		   (dets (map length detector-boxes-movie))
		   ;; coeff of bimodality (1+skewness^2)/(kurtosis+3)
		   (mu (l-mean strengths))
		   (sigma (l-variance mu strengths))
		   (skewness (l-skewness mu sigma strengths))
		   (kurtosis (l-kurtosis mu sigma strengths))
		   (cbimodal (/ (+ 1 (* skewness skewness)) (+ 3 kurtosis)))
		   (mu2 (l-mean dets))
		   (sigma2 (l-variance mu2 dets))
		   (low-hit-ratio (/ (count-if (lambda (a) (< a 5)) dets) (length dets)))
		   ;; (skewness2 (l-skewness mu2 sigma2 dets))
		   ;; (kurtosis2 (l-kurtosis mu2 sigma2 dets))
		   (label (if (member model-name amodels) 1 0)))
	     (cond (train? (format port "~a,~a,~a,~a,~a,~a,~a,~a,~a,~a~%"
				   model-name mu sigma skewness kurtosis cbimodal mu2 sigma2 low-hit-ratio
				   ;skewness2 kurtosis2
				   ;cbimodal2
				   label)) 
		   (test? (format port "~a, ~a, ~a~%"
				  model-name
				  label
				  (equal? (test-vector
					   (string-append classifier-path "/" model-name "--" classifier-type ".mat")
					   (list mu sigma skewness kurtosis cbimodal mu2 sigma2 low-hit-ratio
						 ;skewness2 kurtosis2
						 ;cbimodal2
						 ))
					  label)))
		   (else (fuck-up))))))))
     (map
      (lambda (model-class)
       (remove-if (lambda (e) (or (suffix? "wheelbarrow" e) (suffix? "crawl" e))) model-class))
      (let ((models (get-model-names-list video)))
       (if (= (length models) 33) models (panic "All models not found!")))))))
  (format #t "done~%")))
