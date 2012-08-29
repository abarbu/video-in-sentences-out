(MODULE

 SAM-SLIC

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

;;;  darpa-wrap ./sam-slic  -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a -nr-superpixels 10000

(include "QobiScheme.sch")
(include "sam-slic.sch")

(set! *program* "sam-slic")
(set! *panic?* #f)

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
			  (let ((r ,(second form)))
			   (set! *time-depth* (- *time-depth* 1))
			   r)))
	    expander)))

;;; Macros

;;; Structures

(define-structure slic-superpixel n p data)

(define-structure pixel p data)

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

(define (superpixel-exact-p sp)
 (map-vector exact-round (slic-superpixel-p sp)))

(define (gradient data x y)
 (if (and (>= x 1) (< x (- (matrix-columns data) 1))
	  (>= y 1) (< y (- (matrix-rows data) 1)))
     (+ (sqr
	 (magnitude
	  (v- (image-ref data (vector (+ x 1) y))
	      (image-ref data (vector (- x 1) y)))))
	(sqr
	 (magnitude
	  (v- (image-ref data (vector x (+ y 1)))
	      (image-ref data (vector x (- y 1)))))))
     infinity))

(define (slic-distance s sp p affinity)
 (+ (magnitude (v- (slic-superpixel-data sp) (pixel-data p)))
    (* (/ affinity s) (magnitude (v- (pixel-p p) (slic-superpixel-p sp))))))

;;; Given an image, creates a matrix of the l*ab color values
(define (create-data img)
 (let ((data (matrix-copy (ppm-blue img))))
  (for-each-n-matrix
   (lambda (i j)
    (matrix-set! data j i (rgb->l*ab (image-ref img (vector i j)))))
   (matrix-columns data)
   (matrix-rows data))
  data))

;;; Get the average distance between superpixels
(define (get-s data nr_superpixels)
 (exact-round (sqrt (/ (* (matrix-columns data)
			  (matrix-rows data))
		       nr_superpixels))))

;;; Return the default initial superpixel positions for a given image
(define (initial-superpixel-list data nr-superpixels)
 ;; Currently doesn't respect the exact number of superpixels
 (define (seed-centers data s)
  (let ((rows (exact-round (/ (matrix-rows data) s)))
	(columns (exact-round (/ (matrix-columns data) s))))
   (remove-if
    (lambda (p) (or (>= (x p) (matrix-columns data))
		    (>= (y p) (matrix-rows data))))
    (map-reduce-n
     append
     '()
     (lambda (a)
      (map-n (lambda (b)
	      (v+ `#(,(exact-round (/ s 2)) ,(exact-round (/ s 2)))
		  (v+ (k*v b `#(,s 0)) (k*v a `#(0 ,s)))))
	     columns))
     rows))))
 (define (perturb-seeds data seeds)
  (map
   (lambda (p)
    (reduce
     (lambda (p1 p2)
      (if (< (gradient data (x p1) (y p1)) (gradient data (x p2) (y p2)))
	  p1 p2))
     (map
      (lambda (pp) (v+ p pp))
      (list
       (vector 1 1)  (vector 1 0)  (vector 1 -1)
       (vector 0 1)  (vector 0 0)  (vector 0 -1)
       (vector -1 1) (vector -1 0) (vector -1 -1)))
     ;; Returns infinity from gradient
     (vector -1 -1)))
   seeds))
 (map-indexed
  (lambda (sp i)
   (make-slic-superpixel i sp (image-ref data sp)))
  (perturb-seeds data (seed-centers data (get-s data nr-superpixels)))))

(define (superpixels-from-prev-frame-optical-flow optical-flow prev-superpixels)
 (define (optical-flow-offset p)
  (let ((exact-p (map-vector exact-round p)))
  ;; Assuming first optical flow matrix is x
  (v+
   p
   (vector (image-ref (first optical-flow) exact-p)
	  (image-ref (second optical-flow) exact-p)))))
 (map
  (lambda (sp)
   (make-slic-superpixel (slic-superpixel-n sp)
			 (optical-flow-offset
			  (slic-superpixel-p sp))
			 (slic-superpixel-data sp)))
  prev-superpixels))

(define (superpixels-from-prev-frame-optical-flow-resnap
	 data optical-flow prev-superpixels)
 (define (color-distance sp p)
  (magnitude (v- (slic-superpixel-data sp) (image-ref data p))))
 (let ((labeling (labeling-from-superpixels
		  data
		  (superpixels-from-prev-frame-optical-flow optical-flow
							    prev-superpixels)
		  (get-s data (length prev-superpixels))
		  color-distance)))
  (superpixels-from-labeling data labeling)))

(define (labeling-from-superpixels data superpixels radius dist-func)
 (let ((m-sp (make-matrix (matrix-rows data)
			  (matrix-columns data)
			  (list -1 infinity))))
  (time-code
   (for-each
    (lambda (sp)
     (for-each-pixel-in-rectangle
      (x (superpixel-exact-p sp))
      (y (superpixel-exact-p sp))
      (exact-round radius)
      (exact-round radius)
      (lambda (xx yy)
       (let ((p (vector xx yy)))
	(when (and (>= (x p) 0) (< (x p) (matrix-columns data))
		   (>= (y p) 0) (< (y p) (matrix-rows data)))
	 (let ((dist (dist-func sp p)))
	  (when (< dist (second (image-ref m-sp p)))
	   (matrix-set! m-sp (y p) (x p)
			(list (slic-superpixel-n sp) dist)))))))))
    superpixels))
  (map-matrix first m-sp)))

(define (superpixels-from-labeling data m-sp)
 (let ((sp-p-list (map-n (lambda (p) '()) (+ (maximum-matrix m-sp) 1))))
  (time-code
   (for-each-n-matrix
    (lambda (xx yy)
     (list-set!
      sp-p-list (matrix-ref m-sp yy xx)
      (append (list-ref sp-p-list (matrix-ref m-sp yy xx))
	      (list (make-pixel (vector xx yy) (matrix-ref data yy xx))))))
    (matrix-columns m-sp) (matrix-rows m-sp)))
  (time-code
   (let
     ((sp-list (map (lambda (p-list)
		     (make-pixel
		      (k*v (/ 1 (length p-list))
			   (map-reduce v+ (vector 0 0)
				       (lambda (p) (pixel-p p))
				       p-list))
		      (k*v (/ 1 (length p-list))
			   (map-reduce v+ (vector 0 0 0)
				       (lambda (p) (pixel-data p))
				       p-list))))
		    sp-p-list)))
    (map-n
     (lambda (n)
      (let ((sp (list-ref sp-list n)))
       (make-slic-superpixel n (pixel-p sp) (pixel-data sp))))
     (length sp-list))))))

;;; Computes a list of superpixels from labelings of two frames
;;; location-transform is a function, which when passed a location
;;; in the respective frame, returns the location in the superpixel's
;;; reference.
(define (superpixels-from-labeling-two-frame
	 data-1 labeling-1 location-transform-1
	 data-2 labeling-2 location-transform-2)
 (unless (and (= (matrix-columns labeling-1) (matrix-columns labeling-2))
 	      (= (matrix-rows labeling-1) (matrix-rows labeling-2))
	      (= (matrix-columns data-1) (matrix-columns data-2))
 	      (= (matrix-rows data-1) (matrix-rows data-2)))
  (fuck-up))
 (let ((sp-p-list (map-n (lambda (p) '())
			 (+ (maximum (list (maximum-matrix labeling-1)
					   (maximum-matrix labeling-2)))
			    1))))
  (for-each-indexed-matrix
   (lambda (e-1 yy xx)
    (let ((e-2 (matrix-ref labeling-2 yy xx)))
     (list-set! sp-p-list e-1
		(cons (make-pixel (location-transform-1 `#(,xx ,yy))
				  (matrix-ref data-1 yy xx))
		      (list-ref sp-p-list e-1)))
     (list-set! sp-p-list e-2
		(cons (make-pixel (location-transform-1 `#(,xx ,yy))
				  (matrix-ref data-2 yy xx))
		      (list-ref sp-p-list e-2)))))
   labeling-1)
  (let
    ((sp-list (map (lambda (p-list)
		    (make-pixel
		     (k*v (/ 1 (length p-list))
			  (map-reduce v+ (vector 0 0)
				      (lambda (p) (pixel-p p))
				      p-list))
		     (k*v (/ 1 (length p-list))
			  (map-reduce v+ (vector 0 0 0)
				      (lambda (p) (pixel-data p))
				      p-list))))
		   sp-p-list)))
   (map-indexed
    (lambda (sp n)
     (make-slic-superpixel n (pixel-p sp) (pixel-data sp)))
    sp-list))))

(define (iteration data superpixels s num-iterations dist-func)
 (display (format #f "Iteration #: ~a" num-iterations))
 (newline)
 (time-code
  (if (> num-iterations 0)
      (iteration data
		 (time-code
		  (superpixels-from-labeling
		   data
		   (time-code (labeling-from-superpixels data superpixels s dist-func))))
		 s (- num-iterations 1) dist-func)
      (labeling-from-superpixels data superpixels s dist-func))))

(define (iteration-two-frame
	 data-1 data-2 superpixels s num-iterations
	 dist-func-1 dist-func-2 optical-flow)
 (display (format #f "Iteration #: ~a" num-iterations)) (newline)
 (time-code
  (if (> num-iterations 0)
      (iteration-two-frame
       data-1 data-2
       (superpixels-from-labeling-two-frame
	data-1 (labeling-from-superpixels data-1 superpixels (* 3 s) dist-func-1)
	(lambda (p) (vector (matrix-ref (first optical-flow) (y p) (x p))
		       (matrix-ref (second optical-flow) (y p) (x p))))
	data-2 (labeling-from-superpixels data-2 superpixels (* 3 s) dist-func-2)
	identity)
       s (- num-iterations 1) dist-func-1 dist-func-2 optical-flow)
      (list (labeling-from-superpixels data-1 superpixels (* 3 s) dist-func-1)
	    (labeling-from-superpixels data-2 superpixels (* 3 s) dist-func-2)))))

(define (make-slic-matrix ppm nr-superpixels affinity)
 (let ((data (create-data ppm)))
  (iteration data (initial-superpixel-list data nr-superpixels)
	     (get-s data nr-superpixels) 10
	     (lambda (sp p)
	      (slic-distance (get-s data nr-superpixels) sp
			     (make-pixel p (image-ref data p)) affinity)))))

(define (create-slic-matrix-two-frame ppm-1 ppm-2 nr-superpixels
				      affinity optical-flow)
 (let ((data-1 (create-data ppm-1))
       (data-2 (create-data ppm-2)))
  (iteration-two-frame
   data-1 data-2 (initial-superpixel-list data-1 nr-superpixels)
   (get-s data-1 nr-superpixels) 10
   (lambda (sp p)
    (slic-distance (get-s data-1 nr-superpixels) sp
		   (make-pixel
		    (vector (matrix-ref (first optical-flow) (y p) (x p))
			   (matrix-ref (second optical-flow) (y p) (x p)))
		    (image-ref data-1 p))
		   affinity))
   (lambda (sp p)
    (slic-distance (get-s data-2 nr-superpixels) sp
		   (make-pixel p (image-ref data-2 p)) affinity))
   optical-flow)))

(define (create-slic-matrix-optical-flow ppm nr-superpixels affinity
					 optical-flow prev-superpixels)
 (let* ((data (create-data ppm))
	(s (get-s data nr-superpixels)))
  (iteration data
	     (superpixels-from-prev-frame-optical-flow-resnap
	      data optical-flow prev-superpixels)
	     s 10
	     (lambda (sp p)
	      (slic-distance s sp
			     (make-pixel p (image-ref data p)) affinity)))))

(define (postprocessing-connectivity! slic-matrix superpixel-list)
 ;; I'm sure this is somewhere, but I can't find it
 (define (contains l i)
  (cond ((null? l) #f)
	((eq? (first l) i) #t)
	(else (contains (rest l) i))))
 (define (process-orphan-clusters orphan-clusters next-region)
  (unless (null? orphan-clusters)
   (for-each
    (lambda (v)
     (matrix-set! slic-matrix (y (first (vertex-pixels v)))
		  (x (first (vertex-pixels v))) next-region))
    (graph-vertices (first orphan-clusters)))
   (process-orphan-clusters (rest orphan-clusters) (+ next-region 1))))
 (let* ((cc (connected-components (labeling->graph slic-matrix 1)))
	(superpixel-clusters (make-vector (length superpixel-list) (make-graph  '() '())))
	(orphan-clusters '()))
  (for-each
   (lambda (g)
    (let* ((first-pixel (first (vertex-pixels (first (graph-vertices g)))))
	   (this-superpixel (image-ref slic-matrix first-pixel))
	   (g-prime (vector-ref superpixel-clusters this-superpixel)))
     (if (> (length (graph-vertices g))
	    (length (graph-vertices g-prime)))
	 (seq (when (> (length (graph-vertices g-prime)) 0)
	       (set! orphan-clusters (cons g-prime orphan-clusters)))
	      (vector-set! superpixel-clusters this-superpixel g))
	 (set! orphan-clusters (cons g orphan-clusters)))))
   cc)
  (process-orphan-clusters orphan-clusters (+ (maximum-matrix slic-matrix) 1))
  (let* ((regions-neighbours (frame->regions-neighbours slic-matrix))
	 (regions (first regions-neighbours))
	 (neighbours (second regions-neighbours)))
   (while (>= (maximum-matrix slic-matrix) (length superpixel-list))
	  (for-each
	   (lambda (r)
	    (let ((first-pixel (first (y r))))
	     (unless (< (image-ref slic-matrix first-pixel) (length superpixel-list))
	      (let* ((this-region (image-ref slic-matrix first-pixel))
		     (new-region (map-reduce
				  (lambda (a b)
				   (cond ((and (< a (length superpixel-list))
					       (>= b (length superpixel-list)))
					  a)
					 ((and (< b (length superpixel-list))
					       (>= a (length superpixel-list)))
					  b)
					 ((> (length (y (vector-ref regions a)))
					     (length (y (vector-ref regions b))))
					  a)
					 (else b)))
				  this-region
				  (lambda (np)
				   (cond ((= (first np) this-region) (second np))
					 ((= (second np) this-region) (first np))
					 (else infinity))) ;Should never get here
				  (remove-if
				   (lambda (np) (not (contains np this-region)))
				   neighbours))))
	       (for-each
		(lambda (p)
		 (matrix-set! slic-matrix (y p) (x p) new-region))
		(y r))))))
	   (vector->list regions))))))

(define (postprocessing-tail-elimination! slic-matrix)
 (for-each-indexed
  (lambda (g i)
   (for-each
    (lambda (v)
     (matrix-set! slic-matrix (y (first (vertex-pixels v)))
		  (x (first (vertex-pixels v))) i))
    (graph-vertices g)))
  (connected-components
   (labeling->graph
    (frame-remove-stragglers8-converging slic-matrix 6)
    1))))

(define (read-jpg pathname)
 (call-with-video-input-file pathname "Jpeg" read-ppm-from-video-input-port))

(define (default-pnm-or-jpg-extension input-pathname)
 ;; belongs in QobiScheme
 (cond ((has-extension? input-pathname) input-pathname)
       ((can-open-file-for-input? (default-extension input-pathname "ppm"))
	(default-extension input-pathname "ppm"))
       ((can-open-file-for-input? (default-extension input-pathname "pgm"))
	(default-extension input-pathname "pgm"))
       ((can-open-file-for-input? (default-extension input-pathname "pbm"))
	(default-extension input-pathname "pbm"))
       ((can-open-file-for-input? (default-extension input-pathname "jpg"))
	(default-extension input-pathname "jpg"))
       (else (panic (format #f "File not found: ~a" input-pathname)))))

(define (read-pnm-or-jpg pathname)
 (pnm->ppm
  ((if (string=? (extension pathname) "jpg") read-jpg read-pnm) pathname)))

(define (draw-labeling ppm labeling)
 (unless (and (= (pnm-height ppm) (matrix-rows labeling))
	      (= (pnm-width ppm) (matrix-columns labeling)))
  (fuck-up))
 (let ((height (matrix-rows labeling))
       (width (matrix-columns labeling))
       (red (map-matrix identity (ppm-red ppm)))
       (green (map-matrix identity (ppm-green ppm)))
       (blue (map-matrix identity (ppm-blue ppm))))
  (for-each-n
   (lambda (y)
    (for-each-n
     (lambda (x)
      (unless (and (or (= x (- width 1))
		       (= (matrix-ref labeling y x)
			  (matrix-ref labeling y (+ x 1))))
		   (or (= y (- height 1))
		       (= (matrix-ref labeling y x)
			  (matrix-ref labeling (+ y 1) x))))
       (matrix-set! red y x 255)
       (matrix-set! green y x 0)
       (matrix-set! blue y x 0)))
     width))
   height)
  (make-ppm (ppm-raw? ppm) 255 red green blue)))

;;; Commands

;;; Top Level

(define-command (main (at-most-one
		       ("nr-superpixels" nr-superpixels?
			(nr-superpixels "integer" integer-argument 10000)))
		      (at-most-one
		       ("affinity" affinity?
			(affinity "integer" integer-argument 10)))
		      (at-most-one ("connectivity" connectivity?))
		      (at-most-one ("tail-elimination" tail-elimination?))
		      (at-most-one
		       ("optical-flow" optical-flow?
			(optical-flow-pathname "optical-flow-pathname"
					       string-argument "")
			(prev-frame-pathname "prev-frame-name"
					     string-argument "")))
		      (at-most-one
		       ("two-frame" two-frame?
			(two-frame-optical-flow-pathname "optical flow pathname"
							 string-argument "")
			(second-frame-pathname "path to second frame"
					       string-argument "")))
		      (required (pathname "pathname" string-argument)))
 (let* ((pathname (default-pnm-or-jpg-extension pathname))
	(image (read-pnm-or-jpg pathname))
	(labeling (cond
		   (two-frame?
		    (create-slic-matrix-two-frame
		     image
		     (read-pnm-or-jpg
		      (default-pnm-or-jpg-extension second-frame-pathname))
		     nr-superpixels affinity (read-object-from-file
					      two-frame-optical-flow-pathname)))
		   (optical-flow?
		    (create-slic-matrix-optical-flow
		     image nr-superpixels affinity
		     (read-object-from-file optical-flow-pathname)
		     (superpixels-from-labeling
		      (create-data (read-pnm-or-jpg prev-frame-pathname))
		      (read-object-from-file
		       (replace-extension prev-frame-pathname "dat")))))
		   (else (make-slic-matrix image nr-superpixels affinity)))))
  (when connectivity?
   (when two-frame? (panic "connectivity not yet supported in two-frame mode"))
   (postprocessing-connectivity!
    labeling
    (superpixels-from-labeling
     (create-data image) labeling)))
  (when tail-elimination?
   (if two-frame?
       (begin (postprocessing-tail-elimination! (first labeling))
	      (postprocessing-tail-elimination! (second labeling)))
       (postprocessing-tail-elimination! labeling)))
  (if two-frame?
      (begin (write-object-to-file (first labeling)
				   (replace-extension pathname "dat"))
	     (write-object-to-file
	      (second labeling)
	      (replace-extension second-frame-pathname "dat"))
	     (write-pnm
	      (draw-labeling image (first labeling))
	      (replace-extension
	       (string-append (strip-extension pathname) "_slic") "ppm"))
	     (write-pnm
	      (draw-labeling
	       (read-pnm-or-jpg (default-pnm-or-jpg-extension
				 second-frame-pathname))
	       (second labeling))
	      (replace-extension
	       (string-append
		(strip-extension second-frame-pathname) "_slic") "ppm")))
      (begin
       (write-object-to-file labeling (replace-extension pathname "dat"))
       (write-pnm
	(draw-labeling image labeling)
	(replace-extension
	 (string-append (strip-extension pathname) "_slic") "ppm"))))))
