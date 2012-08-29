(MODULE
  RC-SUPERPIXELS
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
(include "rc-superpixels.sch")

(set! *program* "rc-superpixels")
(set! *panic?* #f)

(define-structure solid-edge-superpixel u v w1 w2 pixels id-u id-v)

;;; Superpixels->Contours

;;; needs work: roundoff error
(define (pixel=? p q) (and (= (x p) (x q)) (= (y p) (y q))))

(define (neighboring-pixels? p q)
 ;; needs work: roundoff error
 (or (and (= (abs (- (x p) (x q))) 1) (= (y p) (y q)))
     (and (= (x p) (x q)) (= (abs (- (y p) (y q))) 1))))

(define (diagonal-midpoints? p q)
 ;; needs work: roundoff error
 (and (= (abs (- (x p) (x q))) 0.5) (= (abs (- (y p) (y q))) 0.5)))

(define (duplicatesp? p xs)
 ;; belongs in QobiScheme
 (and (not (null? xs))
      (or (memp p (first xs) (rest xs)) (duplicatesp? p (rest xs)))))

(define (midpoint-neighbors? p q u-region v-region)
 ;; Checks that p and q are not separated by the interior of region u or v as
 ;; happens with snake back around single pixel bleeding.
 (or
  (and (neighboring-pixels? p q)
       ;; Check for pixel interior r between p and q.
       (not (or (some (lambda (r) (pixel=? r (pixel-midpoint p q))) u-region)
		(some (lambda (r) (pixel=? r (pixel-midpoint p q))) v-region))))
  (diagonal-midpoints? p q)))

(define (endpoint? p midpoints u-region v-region)
 (<=
  (count-if (lambda (q) (midpoint-neighbors? p q u-region v-region)) midpoints)
  1))

(define (pixel-midpoint p q)
 (map-vector (lambda (coordinate) (/ coordinate 2)) (v+ p q)))

(define (neighboring-pixel-midpoints u-boundary v-boundary)
 (map-reduce
  append
  '()
  (lambda (p)
   (removeq
    #f
    (map (lambda (q) (if (neighboring-pixels? p q) (pixel-midpoint p q) #f))
	 v-boundary)))
  u-boundary))

(define (sort-midpoints midpoints u-region v-region)
 ;; This is just to sort the midpoints so that they form an 8-connected
 ;; chain as required by compute-non-simple-polygon-area.
 (let ((endpoint (find-if
		  (lambda (p) (endpoint? p midpoints u-region v-region))
		  midpoints)))
  (unless endpoint (fuck-up))
  (let loop ((midpoints (removeq endpoint midpoints))
	     (sorted-midpoints (list endpoint)))
   (if (null? midpoints)
       sorted-midpoints
       (let ((endpoint
	      (find-if (lambda (p)
			(and (endpoint? p midpoints u-region v-region)
			     (midpoint-neighbors?
			      p (first sorted-midpoints) u-region v-region)))
		       midpoints)))
	(unless endpoint (fuck-up))
	(loop (removeq endpoint midpoints)
	      (cons endpoint sorted-midpoints)))))))

(define (partition-midpoints midpoints u-region v-region)
 (equivalence-classesp
  (lambda (p q) (midpoint-neighbors? p q u-region v-region)) midpoints))

(define (partition-edges g)
 (map-reduce
  append
  '()
  (lambda (e)
   (let* ((u (superpixel-edge-u e))
	  (v (superpixel-edge-v e))
	  (u-regions (equivalence-classesp
		      neighboring-pixels? (superpixel-vertex-region-pixels u)))
	  (v-regions (equivalence-classesp
		      neighboring-pixels? (superpixel-vertex-region-pixels v)))
	  (uid (superpixel-vertex-id u))
	  (vid (superpixel-vertex-id v)))
    (map-reduce
     append
     '()
     (lambda (u-region)
      (let ((u-boundary
	     (intersectionp
	      pixel=? u-region (superpixel-vertex-boundary-pixels u))))
       (map-reduce
	append
	'()
	(lambda (v-region)
	 (let ((v-boundary
		(intersectionp
		 pixel=? v-region (superpixel-vertex-boundary-pixels v))))
	  (removeq
	   #f
	   (map
	    (lambda (midpoints)
	     (if (= (length midpoints) 1)
		 ;; This is a kludge to solve a problem that happens in
		 ;; step3_circle.
		 ;; I don't know why there are 1-pixel edges.
		 (if #f
		     (make-solid-edge-superpixel
		      (first midpoints) (first midpoints) midpoints uid vid)
		     #f)
		 (let ((endpoints
			(remove-if-not
			 (lambda (p) (endpoint? p midpoints u-region v-region))
			 midpoints)))
		  (case (length endpoints)
		   ;; There is a circular edge completely surrounding a region.
		   ;; This can occur when one region completely surrounds
		   ;; another region. It occurs in all three fall examples.
		   ;; For now, just discard edge.
		   ((0) #f)
		   ((2)
		    (make-solid-edge-superpixel
		     (first endpoints)
		     (second endpoints)
		     0
		     0
		     (sort-midpoints midpoints u-region v-region)
		     uid
		     vid))
		   (else
		    ;; debugging
		    (write-pnm
		     (points->ppm midpoints endpoints u-region v-region)
		     "/tmp/foo.ppm")
		    (fuck-up))))))
	    (partition-midpoints
	     (neighboring-pixel-midpoints u-boundary v-boundary)
	     u-region
	     v-region)))))
	v-regions)))
     u-regions)))
  (graph-edges g)))

(define (preliminary-vertices g)
 (let ((pixels '())
       (member?
	(make-matrix
	 (+ (map-reduce max
			minus-infinity
			(lambda (v)
			 (map-reduce max
				     minus-infinity
				     (lambda (p) (* 2 (+ (y p) 1)))
				     (superpixel-vertex-boundary-pixels v)))
			(graph-vertices g))
	    1)
	 (+ (map-reduce max
			minus-infinity
			(lambda (v)
			 (map-reduce max
				     minus-infinity
				     (lambda (p) (* 2 (+ (x p) 1)))
				     (superpixel-vertex-boundary-pixels v)))
			(graph-vertices g))
	    1)
	 #f)))
  (define (add! p)
   (unless (matrix-ref member?
		       (exact-round (+ (* 2 (y p)) 1))
		       (exact-round (+ (* 2 (x p)) 1)))
    (matrix-set! member?
		 (exact-round (+ (* 2 (y p)) 1))
		 (exact-round (+ (* 2 (x p)) 1))
		 #t)
    (set! pixels (cons p pixels))))
  (for-each (lambda (v)
	     (for-each (lambda (p)
			(add! (vector (+ (x p) 0.5) (y p)))
			(add! (vector (- (x p) 0.5) (y p)))
			(add! (vector (x p) (+ (y p) 0.5)))
			(add! (vector (x p) (- (y p) 0.5))))
		       (superpixel-vertex-boundary-pixels v)))
	    (graph-vertices g))
  (map (lambda (p) (make-rc-vertex (list p) #f #f)) pixels)))

(define (actual-edges preliminary-vertices preliminary-edges)
 (let ((cache
	(make-matrix
	 (+ (map-reduce
	     max
	     minus-infinity
	     (lambda (v)
	      (exact-round (+ (* 2 (y (first (rc-vertex-pixels v)))) 1)))
	     preliminary-vertices)
	    1)
	 (+ (map-reduce
	     max
	     minus-infinity
	     (lambda (v)
	      (exact-round (+ (* 2 (x (first (rc-vertex-pixels v)))) 1)))
	     preliminary-vertices)
	    1)
	 #f)))
  (for-each
   (lambda (v)
    (matrix-set! cache
		 (exact-round (+ (* 2 (y (first (rc-vertex-pixels v)))) 1))
		 (exact-round (+ (* 2 (x (first (rc-vertex-pixels v)))) 1))
		 v))
   preliminary-vertices)
  (map (lambda (e)
	(make-solid-edge-superpixel
	 (matrix-ref cache
		     (exact-round (+ (* 2 (y (solid-edge-superpixel-u e))) 1))
		     (exact-round (+ (* 2 (x (solid-edge-superpixel-u e))) 1)))
	 (matrix-ref cache
		     (exact-round (+ (* 2 (y (solid-edge-superpixel-v e))) 1))
		     (exact-round (+ (* 2 (x (solid-edge-superpixel-v e))) 1)))
	 0
	 0
	 (solid-edge-superpixel-pixels e)
	 (solid-edge-superpixel-id-u e)
	 (solid-edge-superpixel-id-v e)))
       preliminary-edges)))

(define (person->graph image superpixels)
 (superpixel-graph->graph (person->superpixel-graph image superpixels)))

(define (person->superpixel-graph image superpixels)
 (superpixel-data->graph superpixels edge-weight-image-region image))

(define (superpixel-graph->graph g)
 (let* ((preliminary-edges (partition-edges g))
	(preliminary-vertices (preliminary-vertices g))
	(edges (actual-edges preliminary-vertices preliminary-edges)))
  (for-each (lambda (p) (set-rc-vertex-vertex! p #f)) preliminary-vertices)
  (for-each (lambda (e)
	     (set-rc-vertex-vertex! (solid-edge-superpixel-u e) #t)
	     (set-rc-vertex-vertex! (solid-edge-superpixel-v e) #t))
	    edges)
  (make-graph (remove-if-not rc-vertex-vertex preliminary-vertices) edges)))

;;; Pathnames

(define (default-pnm-extension input-pathname)
 ;; belongs in QobiScheme
 (cond ((has-extension? input-pathname) input-pathname)
       ((can-open-file-for-input? (default-extension input-pathname "ppm"))
	(default-extension input-pathname "ppm"))
       ((can-open-file-for-input? (default-extension input-pathname "pgm"))
	(default-extension input-pathname "pgm"))
       ((can-open-file-for-input? (default-extension input-pathname "pbm"))
	(default-extension input-pathname "pbm"))
       (else (panic (format #f "File not found: ~a" input-pathname)))))

(define (generate-pathname prefix name type . args)
 (let ((extra-args (map-reduce
		    string-append
		    ""
		    (lambda (arg) (string-append "-" arg))
		    (remove-if null? args))))
  (string-append
   prefix
   name
   (case type
    ('base extra-args)
    ('image (string-append extra-args ".ppm"))
    ('pb (string-append "-berkeley" extra-args ".pgm"))
    ('superpixels (string-append "-slic" extra-args ".dat"))
    ('graph (string-append extra-args ".graph"))
    ('lines (string-append extra-args ".lines"))
    ('weights (string-append extra-args ".w"))
    ('cycle (string-append extra-args ".cycle"))
    ('output-image (string-append extra-args "-superpixels.ppm"))
    ('output-contour (string-append extra-args "-contour.pgm"))
    ('output-graph (string-append extra-args "-solid.pgm"))
    ('numbered-graph (string-append extra-args ".ps"))
    ('chains (string-append extra-args ".chains"))
    ('cycle-sc (string-append extra-args "-cycle.sc"))
    ('contour (string-append extra-args "-contour.sc"))
    (else "BAD")))))

(define (cropped-pathname video-name frame model model-number)
 (generic-pathname video-name frame (string-append "frame-cropped-" model "-" (number->string model-number) ".ppm")))

(define (cropped-berkeley-pathname video-name frame model model-number)
 (generic-pathname video-name frame (string-append "frame-cropped-berkeley-" model "-" (number->string model-number) ".pgm")))

(define (cropped-slic-pathname video-name frame model model-number)
 (generic-pathname video-name frame (string-append "frame-cropped-slic-" model "-" (number->string model-number) ".dat")))

(define (cropped-distance-matrix-pathname video-name frame model model-number)
 (generic-pathname video-name frame (string-append "frame-cropped-distance-matrix-" model "-" (number->string model-number) ".dat")))

(define (graph-pathname video-name frame model model-number)
 (generic-pathname video-name frame (string-append "frame-cropped-" model "-" (number->string model-number) ".graph")))

(define (lines-pathname video-name frame model model-number)
 (generic-pathname video-name frame (string-append "frame-cropped-" model "-" (number->string model-number) ".lines")))

(define (weight-pathname video-name frame model model-number)
 (generic-pathname video-name frame (string-append "frame-cropped-" model "-" (number->string model-number) ".w")))

(define (cycle-pathname video-name frame model model-number cycle-number)
 (generic-pathname video-name frame (string-append "frame-cropped-" model "-" (number->string model-number) "-" (number->string cycle-number)  ".cycle")))

(define (output-image-pathname video-name frame model model-number cycle-number)
 (generic-pathname video-name frame (string-append "frame-cropped-" model "-" (number->string model-number) "-" (number->string cycle-number)  "-superpixels.ppm")))

(define (output-contour-pathname video-name frame model model-number cycle-number)
 (generic-pathname video-name frame (string-append "frame-cropped-" model "-" (number->string model-number) "-" (number->string cycle-number)  "-contour.pbm")))

(define (output-graph-pathname video-name frame model model-number cycle-number)
 (generic-pathname video-name frame (string-append "frame-cropped-" model "-" (number->string model-number) "-" (number->string cycle-number)  "-solid.ppm")))

(define (distance-matrix-pb globalpb)
 (distance-transform (binary-threshold-optimal globalpb)))

;;; Weight computation

(define *a* 5000)

(define (decreasing-edge-weight-function e)
 ;; the denominator is chosen so that 0|->*a* and 1|->1.
 (* *a* (exp (- (/ e (/ (- (log (/ *a*)))))))))

(define (decreasing-edge-weight-function-old e)
 (/ 1 (+ 1 e)))

(define (compute-w1 e data)
 ;; (decreasing-edge-weight-function (edge-distance-sum e data))
 ;; (distance-sum e data)
(average-distance-sum e data))

(define (distance-sum e distances)
 (map-reduce + 0
	     (lambda (pt)
	      (let ((pt (midpoint->point pt)))
	       (expt (matrix-ref (first distances) (y pt) (x pt)) 2)))
	     (solid-edge-superpixel-pixels e)))

(define (average-distance-sum e distances)
  (+ 1 (/ (map-reduce + 0
	      (lambda (pt)
	       (let* ((pt (midpoint->point pt)))
		(expt (matrix-ref (first distances) (y pt) (x pt)) 1.5)))
	      (solid-edge-superpixel-pixels e))
     (length (solid-edge-superpixel-pixels e)))))

(define (edge-distance-sum e edge-map)
  (/ (map-reduce + 0
	      (lambda (pt)
	       (let* ((pt (midpoint->point pt)))
		(/ (matrix-ref (pgm-grey edge-map) (y pt) (x pt)) 255) ))
	      (solid-edge-superpixel-pixels e))
     (length (solid-edge-superpixel-pixels e))))

(define (edge-average-intensity e edge-image)
 ;; Formula: 1 / (1 + avg berkley intensity over midpoints)
 (/ 1 (+ 1 (/ (map-reduce +
			  0
			  (lambda (pt)
			   (let ((pt (midpoint->point pt)))
			    (matrix-ref (pgm-grey edge-image) (y pt) (x pt))))
			  (solid-edge-superpixel-pixels e))
	      (length (solid-edge-superpixel-pixels e))))))

(define (compute-non-simple-polygon-area-sparse polygon)
 (let* ((dense-polygon
	 (map-reduce
	  append
	  '()
	  (lambda (p1 p2)
	   (cond ((and (= (y p1) (y p2)) (= (abs (- (x p2) (x p1))) 1))
		  (list p1 `#(,(/ (+ (x p1) (x p2)) 2) ,(y p1)) p2))
		 ((and (= (x p1) (x p2)) (= (abs (- (y p2) (y p1))) 1))
		  (list p1 `#(,(x p1) ,(/ (+ (y p1) (y p2)) 2)) p2))
		 ((and (= (abs (- (y p2) (y p1))) 1) (= (abs (- (y p2) (y p1))) 1))
		  (list p1
			`#(,(x p1) ,(/ (+ (y p1) (y p2)) 2))
			`#(,(/ (+ (x p1) (x p2)) 2) ,(/ (+ (y p1) (y p2)) 2))
			p2))
		 (else (list p1 p2))))
	  (but-last polygon)
	  (cdr polygon)))
	(area (compute-non-simple-polygon-area dense-polygon)))
  (when (< area 0)
   (panic "polygon has negative area, it really shouldn't!"))
  area))

(define (compute-solid-weight2-check chain)
 (unless (every (lambda (p q)
		 (every-vector (lambda (c1 c2) (<= (abs (- c1 c2)) 1)) p q))
		(but-last chain)
		(rest chain))
  (panic "not 8 connected"))
 (compute-non-simple-polygon-area-sparse chain))

(define (compute-w2 u v)
 (area-between-pixels u v))

(define (pgm-dialate n pgm)
 (let* ((height (pnm-height pgm))
	(width (pnm-width pgm))
	(grey (make-matrix height width 0)))
  (for-each-n
   (lambda (y)
    (for-each-n
     (lambda (x)
      (do ((y1 (max (- y n) 0) (+ y1 1)))
	((>= y1 (min (+ y (+ n 1)) height)))
       (do ((x1 (max (- x n) 0) (+ x1 1)))
	 ((>= x1 (min (+ x (+ n 1)) width)))
	(matrix-set!
	 grey y x
	 (max (matrix-ref grey y x) (matrix-ref (pgm-grey pgm) y1 x1))))))
     (pnm-width pgm)))
   (pnm-height pgm))
  (make-pgm (pgm-raw? pgm) 255 grey)))

(define (make-normalized-pgm matrix)
 (let* ((m (maximum-matrix matrix)) (m (if (zero? m) 1 m)))
  (make-pgm
   #f 255 (map-matrix (lambda (v) (exact-round (* 255 (/ v m)))) matrix))))

(define (edge-mask edge n)
 (let ((height (pnm-height edge)) (width (pnm-width edge)))
  (make-normalized-pgm
   (map-n-vector
    (lambda (y)
     (map-n-vector
      (lambda (x)
       (let ((count 0) (sum 0))
	(do ((y1 (max (- y n) 0) (+ y1 1)))
	  ((>= y1 (min (+ y (+ n 1)) height)))
	 (do ((x1 (max (- x n) 0) (+ x1 1)))
	   ((>= x1 (min (+ x (+ n 1)) width)))
	  (set! count (+ count 1))
	  (set! sum
		(+ sum
		   (/ (/ (matrix-ref (pgm-grey edge) y1 x1) 255)
		      (sqr (+ (distance (vector x y) (vector x1 y1)) 1)))))))
	(/ sum count)))
      width))
    height))))

(define (midpoint->point mp)
 ;; needs work: might want to do more than round to nearest pixel
 (vector (inexact->exact (floor (x mp))) (inexact->exact (floor (y mp)))))

;;; Intermediate graph conversions

(define (flip-edge e)
 (let ((u (first (rc-vertex-pixels (solid-edge-superpixel-u e))))
       (v (first (rc-vertex-pixels (solid-edge-superpixel-v e)))))
  (if
   (and
    (= (x u) (x v))
    (> (y v) (y u))) ; for whatever reason, this case happens less often
   (begin
    (make-solid-edge-superpixel
     (solid-edge-superpixel-v e)
     (solid-edge-superpixel-u e)
     (solid-edge-superpixel-w1 e)
     (solid-edge-superpixel-w2 e)
     (solid-edge-superpixel-pixels e)
     (solid-edge-superpixel-id-u e)
     (solid-edge-superpixel-id-v e)))
   e)))

(define (graph->rc-graph g edge-data)
 (let ((solid
	(map flip-edge
	     (map-indexed
	      (lambda (e n)
	       (let* ((w2 (compute-w2 (point (solid-edge-superpixel-u e)) (point (solid-edge-superpixel-v e))))
		      (w1 (compute-w1 e edge-data)))
		(set-rc-vertex-pixels!
		 (solid-edge-superpixel-u e)
		 (list (first (rc-vertex-pixels (solid-edge-superpixel-u e))) (* 2 n)))
		(set-rc-vertex-pixels!
		 (solid-edge-superpixel-v e)
		 (list (first (rc-vertex-pixels (solid-edge-superpixel-v e))) (+ (* 2 n) 1)))
		(make-solid-edge-superpixel
		 (solid-edge-superpixel-u e)
		 (solid-edge-superpixel-v e)
		 (max w1 0)
		 w2
		 (solid-edge-superpixel-pixels e)
		 (solid-edge-superpixel-id-u e)
		 (solid-edge-superpixel-id-v e))))
	      (graph-edges g)))))
  (list (graph-vertices g)
	solid
	(rc-erect-dashed-edges
	 (map (lambda (e) (list (solid-edge-superpixel-u e) (solid-edge-superpixel-v e)))
	      solid)
	 (lambda (u v) 0)
	 (lambda (a b)
	  (let ((area (compute-non-simple-polygon-area (list (point a) (point b)))))
	   (when (< area 0)
	    (panic "area of dashed weight is less than 0"))
	   area))
	 ;; needs work: change rc-erect-dashed-edges to be <= rather than <
	 2.1))))

(define (solid-edges->lines-file! solid output-file)
 (call-with-output-file output-file
  (lambda (port)
   (for-each (lambda (e)
	      (format port "~a ~a ~a ~a~%"
		      (x (first (rc-vertex-pixels (solid-edge-superpixel-u e))))
		      (y (first (rc-vertex-pixels (solid-edge-superpixel-u e))))
		      (x (first (rc-vertex-pixels (solid-edge-superpixel-v e))))
		      (y (first (rc-vertex-pixels (solid-edge-superpixel-v e))))))
	     solid))))

(define (rc-graph->graph-file! g output-file)
 (call-with-output-file output-file
  (lambda (port)
   ;; (display "vertices: ")
   ;; (write (* (length (rc-chains-solid-edges g)) 2))
   ;; (newline)
   (format port "~a~%" (* (length (rc-chains-solid-edges g)) 2))
   (for-each
    (lambda (e)
     (format port "~a ~a~%~a ~a~%"
	     (x (first (rc-vertex-pixels (solid-edge-superpixel-u e))))
	     (y (first (rc-vertex-pixels (solid-edge-superpixel-u e))))
	     (x (first (rc-vertex-pixels (solid-edge-superpixel-v e))))
	     (y (first (rc-vertex-pixels (solid-edge-superpixel-v e))))))
    (rc-chains-solid-edges g))
   ;; (display "solid: ")
   ;; (write (length (rc-chains-solid-edges g)))
   ;; (newline)
   (for-each
    (lambda (e)
     (format port "~a ~a ~a ~a 1~%"
	     (second (rc-vertex-pixels (solid-edge-superpixel-u e)))
	     (second (rc-vertex-pixels (solid-edge-superpixel-v e)))
	     (solid-edge-superpixel-w1 e)
	     (solid-edge-superpixel-w2 e)))
    (rc-chains-solid-edges g))
   ;; (display "dashed: ")
   ;; (write (length (rc-chains-dashed-edges g)))
   ;; (newline)
   (for-each
    (lambda (e)
     (format port "~a ~a ~a ~a 0~%"
	     (second (rc-vertex-pixels (dashed-edge-u e)))
	     (second (rc-vertex-pixels (dashed-edge-v e)))
	     (dashed-edge-w1 e)
	     (dashed-edge-w2 e)))
    (rc-chains-dashed-edges g)))))

;;; Postscript drawing utilities

(define *mrc-postscript-leader*
 "%!PS-Adobe-2.0 EPSF-2.0
%%Pages: 1
%%BoundingBox: 0 0 ~a ~a
%%EndComments
/readstring {
  currentfile exch readhexstring pop
} bind def
/picstr ~a string def
%%EndProlog
%%Page: 1 1
gsave
0 0 translate
~a ~a scale
~a ~a 8
[ ~a 0 0 -~a 0 ~a ]
{ picstr readstring }
image
")

(define *mrc-postscript-trailer*
 "grestore
showpage
%%Trailer
")

(define (draw-line ps w xp yp xq yq r g b)
 (format ps "~a setlinewidth newpath ~a ~a moveto ~a ~a lineto gsave ~a ~a ~a setrgbcolor stroke grestore~%"
	 w xp yp xq yq r g b))

(define (draw-text ps font-size xp yp r g b text)
 (format ps "/Courier findfont ~a scalefont setfont newpath ~a ~a moveto gsave ~a ~a ~a setrgbcolor (~a) show grestore~%" font-size xp yp r g b text))

(define (max-w1-in-chain chain)
 (map-reduce max 0 solid-edge-superpixel-w1 (rc-chains-solid-edges chain)))

(define (write-solid-graph chain pgm pathname)
 (let ((height (pnm-height pgm))
       (width (pnm-width pgm)))
  (call-with-output-file pathname
   (lambda (ps)
    (format ps *mrc-postscript-leader*
	    width height width width height width height width height height)
    (let ((i 0))
     (do ((y 0 (+ y 1))) ((>= y height))
      (do ((x 0 (+ x 1))) ((>= x width))
       (when (= i 30) (newline ps) (set! i 0))
       (let ((string (number->string (matrix-ref (pgm-grey pgm) y x) 16)))
	(when (= (string-length string) 1) (write-char #\0 ps))
	(display string ps)
	(set! i (+ i 1))))))
    (newline ps)
    (format ps "grestore~%gsave~%")
    (for-each
     (lambda (e)
      (let* ((rgb (hsv->rgb
		   (vector
		    (exact-round (* 120 (/ (solid-edge-superpixel-w1 e) (max-w1-in-chain chain))))
		    100
		    100)))
	     (red (/ (x rgb) 255.0))
	     (green (/ (y rgb) 255.0))
	     (blue (/ (z rgb) 255.0))
	     (x1 (x (point (solid-edge-superpixel-u e))))
	     (y1 (- height (y (point (solid-edge-superpixel-u e)))))
	     (x2 (x (point (solid-edge-superpixel-v e))))
	     (y2 (- height (y (point (solid-edge-superpixel-v e)))))
	     (up (vector x1 y1))
	     (vp (vector x2 y2))
	     ;; (mp (midpoint->point (pixel-midpoint up vp)))
	     ;; (mpu (midpoint->point (pixel-midpoint mp up)))
	     ;; (mpv (midpoint->point (pixel-midpoint mp vp)))
	     (mp (pixel-midpoint up vp))
	     (mpu (pixel-midpoint mp up))
	     (mpv (pixel-midpoint mp vp)))
       (draw-line ps 0.0
		  (min (max x1 0) width)
		  (min (max y1 0) height)
		  (min (max x2 0) width)
		  (min (max y2 0) height)
		  red green blue)
       (draw-text ps 1 (x mpu) (y mpu) 1 1 1 (number->string (label (solid-edge-superpixel-u e ))))
       (draw-text ps 1 (x mpv) (y mpv) 1 1 1 (number->string (label (solid-edge-superpixel-v e ))))))
     (rc-chains-solid-edges chain))
    (for-each
     (lambda (e)
      (let* ((rgb (vector 0 0 255))
	     (red (/ (x rgb) 255.0))
	     (green (/ (y rgb) 255.0))
	     (blue (/ (z rgb) 255.0))
	     (x1 (x (point (dashed-edge-u e))))
	     (y1 (y (point (dashed-edge-u e))))
	     (x2 (x (point (dashed-edge-v e))))
	     (y2 (y (point (dashed-edge-v e)))))
       (draw-line ps 0.0
		  (min (max x1 0) width)
		  (min (max (- height y1) 0) height)
		  (min (max x2 0) width)
		  (min (max (- height y2) 0) height)
		  red green blue)))
     (rc-chains-dashed-edges chain))
    (format ps *mrc-postscript-trailer*)))))

;;; Resulting cycle functions

(define (read-cycle-edges chain pathname)
 (let* ((w2 0)
	(w1 0))
  (map
   (lambda (l)
    (let* ((u (inexact->exact (floor (/ (string->number (field-ref l 0)) 2))))
	   (v (inexact->exact (floor (/ (string->number (field-ref l 1)) 2))))
	   (solid (find-if
		   (lambda (s)
		    (or (and (= (second (rc-vertex-pixels (solid-edge-superpixel-u s))) u)
			     (= (second (rc-vertex-pixels (solid-edge-superpixel-v s))) v))
			(and (= (second (rc-vertex-pixels (solid-edge-superpixel-u s))) v)
			     (= (second (rc-vertex-pixels (solid-edge-superpixel-v s))) u))))
		   (rc-chains-solid-edges chain)))
	   (dashed
	    (find-if
	     (lambda (d)
	      (or (and (= (second (rc-vertex-pixels (dashed-edge-u d))) u)
		       (= (second (rc-vertex-pixels (dashed-edge-v d))) v))
		  (and (= (second (rc-vertex-pixels (dashed-edge-u d))) v)
		       (= (second (rc-vertex-pixels (dashed-edge-v d))) u))))
	     (rc-chains-dashed-edges chain))))
     ;; Swap polarity based on returned results
     ;; (when solid
     ;;  (when (< (string->number (field-ref l 2)) 0)
     ;;   (set-solid-edge-superpixel-w2! solid (- (solid-edge-superpixel-w2 solid)))))
     ;; (when dashed
     ;;  (when (< (string->number (field-ref l 2)) 0)
     ;;   (set-dashed-edge-w2! dashed (- (dashed-edge-w2 dashed)))))
     (set! w2 (+ w2 (string->number (field-ref l 2))))
     (set! w1 (+ w1 (string->number (field-ref l 3))))
     (cond (solid solid)
	   (dashed dashed)
	   (else (fuck-up)))))
   (cdr (read-file pathname)))))

(define (draw-cycle-image input-image edges)
 (let* ((image input-image)
	(w1 0)
	(w2 0)
	(w1-lengths (remove-if zero? (map (lambda (e) (if (solid-edge-superpixel? e) (length (solid-edge-superpixel-pixels e)) 0)) edges))))
  (unless (null? edges)
   (define (draw-red y x)
    (matrix-set! (ppm-red image) y x 255)
    (matrix-set! (ppm-green image) y x 0)
    (matrix-set! (ppm-blue image) y x 0))
   (for-each
    (lambda (e)
     (if
      (dashed-edge? e)
      (begin
       (for-each
	(lambda (pt)
	 (draw-red (exact-floor (y pt)) (exact-floor (x pt)))
	 (draw-red (exact-floor (y pt)) (exact-ceiling (x pt)))
	 (draw-red (exact-ceiling (y pt)) (exact-floor (x pt)))
	 (draw-red (exact-ceiling (y pt)) (exact-ceiling (x pt))))
	(line-segment->points
	 (make-line-segment
	  (first (rc-vertex-pixels (dashed-edge-u e)))
	  (first (rc-vertex-pixels (dashed-edge-v e)))))))
      (begin
       (set! w1 (+ w1 (solid-edge-superpixel-w1 e)))
       (set! w2 (+ w2 (solid-edge-superpixel-w2 e)))
       (for-each
	(lambda (mp)
	 (draw-red (exact-floor (y mp)) (exact-floor (x mp)))
	 (draw-red (exact-floor (y mp)) (exact-ceiling (x mp)))
	 (draw-red (exact-ceiling (y mp)) (exact-floor (x mp)))
	 (draw-red (exact-ceiling (y mp)) (exact-ceiling (x mp))))
	(solid-edge-superpixel-pixels e)))))
    edges)
   image)))

(define (draw-contour-image image edges)
 (let* ((blank (pbm-constant (pnm-width image) (pnm-height image) #f))
	(w1 0)
	(w2 0)
	(w1-lengths (remove-if zero? (map (lambda (e) (if (solid-edge-superpixel? e) (length (solid-edge-superpixel-pixels e)) 0)) edges))))
  (when (eq? edges #f) (panic "Cannot draw #f edges"))
  (unless (null? edges)
   (define (draw-pixel y x)
    (matrix-set! (pbm-bitmap blank) y x #t))
   (for-each
    (lambda (e)
     (if
      (dashed-edge? e)
      (begin
       (for-each
	(lambda (pt)
	 (draw-pixel (exact-floor (y pt)) (exact-floor (x pt)))
	 (draw-pixel (exact-floor (y pt)) (exact-ceiling (x pt)))
	 (draw-pixel (exact-ceiling (y pt)) (exact-floor (x pt)))
	 (draw-pixel (exact-ceiling (y pt)) (exact-ceiling (x pt))))
	(line-segment->points
	 (make-line-segment
	  (first (rc-vertex-pixels (dashed-edge-u e)))
	  (first (rc-vertex-pixels (dashed-edge-v e)))))))
      (begin
       (set! w1 (+ w1 (solid-edge-superpixel-w1 e)))
       (set! w2 (+ w2 (solid-edge-superpixel-w2 e)))
       (for-each
	(lambda (mp)
	 (draw-pixel (exact-floor (y mp)) (exact-floor (x mp)))
	 (draw-pixel (exact-floor (y mp)) (exact-ceiling (x mp)))
	 (draw-pixel (exact-ceiling (y mp)) (exact-floor (x mp)))
	 (draw-pixel (exact-ceiling (y mp)) (exact-ceiling (x mp))))
	(solid-edge-superpixel-pixels e)))))
    edges)
   (pbm->pgm
    (fix-contour-image
     (pbm-flood blank
		(quantize-point
		 (centroid
		  (pbm->points blank)))))))))

(define (draw-superpixel-image image superpixels)
 (let* ((blank (pbm-constant (pnm-width image) (pnm-height image) #f)))
  (unless (null? superpixels)
   (define (draw-pixel pixel)
    (matrix-set! (pbm-bitmap blank) (y pixel) (x pixel) #t))
   (for-each
    (lambda (v)
     (for-each
      (lambda (pixel)
       (draw-pixel pixel))
      (superpixel-vertex-region-pixels v)))
    superpixels))
  blank))

(define (fix-contour-image image)
 ;; Make this more intelligent post-haste
 (if (eq? (matrix-ref (pbm-bitmap image) 0 0) #t)
     (pbm-not image)
     image))

(define (draw-cycle-edges! image edges edge-composite-path filled-contour-path)
 (let* ((blank (pbm-constant (pnm-width image) (pnm-height image) #f))
	(w1 0)
	(w2 0)
	(w1-lengths (remove-if zero? (map (lambda (e) (if (solid-edge-superpixel? e) (length (solid-edge-superpixel-pixels e)) 0)) edges))))
  (unless (null? edges)
   (define (draw-red y x)
    (matrix-set! (ppm-red image) y x 255)
    (matrix-set! (ppm-green image) y x 0)
    (matrix-set! (ppm-blue image) y x 0)
    (matrix-set! (pbm-bitmap blank) y x #t))
   (for-each
    (lambda (e)
     (if
      (dashed-edge? e)
      (begin
       ;; (set! w1 (+ w1 (dashed-edge-w1 e)))
       ;; (set! w2 (+ w2 (dashed-edge-w2 e)))
       (for-each
	(lambda (pt)
	 (draw-red (exact-floor (y pt)) (exact-floor (x pt)))
	 (draw-red (exact-floor (y pt)) (exact-ceiling (x pt)))
	 (draw-red (exact-ceiling (y pt)) (exact-floor (x pt)))
	 (draw-red (exact-ceiling (y pt)) (exact-ceiling (x pt))))
	(line-segment->points
	 (make-line-segment
	  (first (rc-vertex-pixels (dashed-edge-u e)))
	  (first (rc-vertex-pixels (dashed-edge-v e)))))))
      (begin
       (set! w1 (+ w1 (solid-edge-superpixel-w1 e)))
       (set! w2 (+ w2 (solid-edge-superpixel-w2 e)))
       (for-each
	(lambda (mp)
	 (draw-red (exact-floor (y mp)) (exact-floor (x mp)))
	 (draw-red (exact-floor (y mp)) (exact-ceiling (x mp)))
	 (draw-red (exact-ceiling (y mp)) (exact-floor (x mp)))
	 (draw-red (exact-ceiling (y mp)) (exact-ceiling (x mp))))
	(solid-edge-superpixel-pixels e)))))
    edges)
   (display "w1    : ")(write w1)(newline)
   (display "w2    : ")(write w2)(newline)
   (display "w1/w2 : ")(write (/ w1 w2))(newline)
   (display "length: ")(write (length edges)) (newline)
   ;; (display "mean length: ")(write (/ (reduce + w1-lengths 0) (count-if solid-edge-superpixel? edges)))(newline)
   ;; (display "var length : ")(write (list-variance w1-lengths))(newline)
   ;; (display "std length : ")(write (sqrt (list-variance w1-lengths)))(newline)
   (write-pnm
    image
    edge-composite-path)
   (write-pnm
    (pbm->pgm
     (pbm-flood blank
		(quantize-point
		 (centroid
		  (pbm->points blank)))))
    filled-contour-path))))

(define (rc-chain->ppm g)
 (let* ((vs (map (lambda (v)
		  (map-vector exact-round (k*v 2 (first (rc-vertex-pixels v)))))
		 (rc-chains-vertices g)))
	(es (map-reduce
	     append
	     '()
	     (lambda (v)
	      (map (lambda (p) (map-vector exact-round (k*v 2 p)))
		   (solid-edge-superpixel-pixels v)))
	     (rc-chains-solid-edges g)))
	(height (+ (max (map-reduce max minus-infinity y vs)
			(map-reduce max minus-infinity y es))
		   1))
	(width (+ (max (map-reduce max minus-infinity x vs)
		       (map-reduce max minus-infinity x es))
		  1))
	(red (make-matrix height width 0))
	(green (make-matrix height width 0))
	(blue (make-matrix height width 0)))
  (when #f (for-each (lambda (p) (matrix-set! red (y p) (x p) 255)) vs))
  (for-each (lambda (p) (matrix-set! green (y p) (x p) 255)) es)
  (for-each (lambda (e)
	     (for-each (lambda (p) (matrix-set! blue (y p) (x p) 255))
		       (map
			(lambda (p) (quantize-point (k*v 2 p)))
			(line-segment->points
			 (make-line-segment
			  (first (rc-vertex-pixels (dashed-edge-u e)))
			  (first (rc-vertex-pixels (dashed-edge-v e))))))))
	    (rc-chains-dashed-edges g))
  (make-ppm #t 255 red green blue)))

;;; Single frame driver

(define-structure rc-contour edges superpixels chains level)

(define (contour->superpixels superpixel-graph contour-image)
 ;; Must be a better way to handle this...
 (remove-if
  (lambda (v)
   (let ((center-point (map-vector exact-round (centroid (superpixel-vertex-region-pixels v)))))
    (= (matrix-ref
	(pgm-grey contour-image)
	(y center-point)
	(x center-point))
       0)))
  (graph-vertices superpixel-graph)))

(define (rc-initial-iteration path image chains superpixel-graph)
 (let* ((contour-edges (first (rc path chains 1)))
	(superpixel-set
	 (contour->superpixels superpixel-graph (draw-contour-image image contour-edges))))
  (make-rc-contour contour-edges superpixel-set chains 0)))

(define (rc-non-recursive path image chains superpixel-graph number-of-contours)
 (map-indexed
  (lambda (contour-edges n)
   (let ((superpixel-set
	 (contour->superpixels superpixel-graph (draw-contour-image image contour-edges))))
    (make-rc-contour contour-edges superpixel-set chains n)))
  (rc path chains number-of-contours)))

(define (rc-recursive path image superpixel-graph prev-contour level last-level)
 (cond
  ((> level last-level) (list prev-contour))
  (else (append
	 (list prev-contour)
	 (rc-recursive
	  path
	  image
	  superpixel-graph
	  (first
	   (sort
	    (removeq
	     #f
	     (map-indexed
	      (lambda (e i)
	       (if (dashed-edge? e)
		   (let* ((chains (rc-contour-chains prev-contour))
			  (contour-chains
			   (make-rc-chains
			    (rc-chains-width chains)
			    (rc-chains-height chains)
			    (rc-chains-vertices chains)
			    (rc-chains-solid-edges chains)
			    (remove-dashed-edge chains
						(label (dashed-edge-u e))
						(label (dashed-edge-v e)))))
			  (contour-edges
			   (first (rc (lambda (type . args)
				       (apply
					generate-pathname
					(append
					 `("" ,(path 'base) ,type ,(number->string level) ,(number->string i))
					 args)))
				      contour-chains
				      1)))
			  (contour
			   (make-rc-contour
			    contour-edges
			    (contour->superpixels superpixel-graph (draw-contour-image image contour-edges))
			    contour-chains
			    level)))
		    (write-object-to-file contour ((lambda (type . args)
						    (apply
						     generate-pathname
						     (append
						      `("" ,(path 'base) ,type ,(number->string level) ,(number->string i))
						      args))) 'contour))
		    (write-pnm (draw-contour-image image contour-edges)
			       ((lambda (type . args)
				 (apply
				  generate-pathname
				  (append
				   `("" ,(path 'base) ,type ,(number->string level) ,(number->string i))
				   args))) 'output-contour))
		    contour)
		   #f))
	      (rc-contour-edges prev-contour)))
	    >
	    (lambda (set)
	     (compare-superpixel-sets set prev-contour))))
	  (+ level 1)
	  last-level)))))

(define (run-rc-superpixels path image superpixels edge-image number-of-contours)
 (display (path 'base))
 (let* ((base-graph (person->graph image superpixels))
	(superpixel-graph (person->superpixel-graph image superpixels))
	(g (graph->rc-graph base-graph edge-image))
	(chains (make-rc-chains
		 (pnm-height image)
		 (pnm-width image)
		 (first g)
		 ;;(map (lambda (a) (list (solid-edge-superpixel-u a) (solid-edge-superpixel-v a))) (second g))
		 (second g)
		 (third g)))
	(graph-background (let ((mat-max (maximum-matrix (first edge-image))))
			   (make-pgm
			    #f
			    255
			    (map-matrix
			     (lambda (value) (exact-round (* (/ value mat-max) 255)))
			     (first edge-image))))))
  (compute-weights! chains edge-image)
  (write-object-to-file chains (path 'chains))
  ;; if controls the recursive (greedy) or non-recursive version of rc
  (let* ((rc-contours (if #t
			  (reverse (rc-recursive
				    path
				    image
				    superpixel-graph
				    (rc-initial-iteration path image chains superpixel-graph)
				    1 number-of-contours))
			  (rc-non-recursive path image chains superpixel-graph number-of-contours))))
   (for-each
    (lambda (contour)
     (write-contour-files! path chains (rc-contour-edges contour) image graph-background (rc-contour-level contour)))
    rc-contours))))

(define (write-contour-files! path chains cycle image graph-background n)
 (write-solid-graph chains graph-background (path 'numbered-graph))
 (write-object-to-file cycle (path 'cycle-sc))
 (write-pnm (draw-cycle-image image cycle) (path 'output-image (number->string n)))
 (write-pnm (draw-contour-image image cycle) (path 'output-contour (number->string n)))
 (write-pnm (rc-chain->ppm
	     (make-rc-chains
	      (rc-chains-width chains)
	      (rc-chains-height chains)
	      (rc-chains-vertices chains)
	      (remove-if-not solid-edge-superpixel? cycle)
	      (remove-if-not dashed-edge? cycle)))
	    (path 'output-graph (number->string n))))

(define (compare-superpixel-sets s1 s2)
 (let* ((set1 (map (lambda (v) (superpixel-vertex-id v)) (rc-contour-superpixels s1)))
	(set2 (map (lambda (v) (superpixel-vertex-id v)) (rc-contour-superpixels s2)))
	(superpixel-intersection
	 (length
	  (intersection set1 set2)))
	(superpixel-union
	 (length
	  (union set1 set2)))
	(superpixel-difference1
	 (length
	  (set-difference set1 set2)))
	(superpixel-difference2
	 (length
	  (set-difference set2 set1)))
	(superpixel-symmetric-difference
	 (length
	  (set-difference (union set1 set2) (intersection set1 set2)))))
  ;; (+ superpixel-intersection superpixel-difference)
  superpixel-symmetric-difference))

(define (rc-clean path n)
 (rm (path 'graph))
 (rm (path 'lines))
 (rm (path 'weights))
 (rm (path 'cycle (number->string n))))

(define (rc-clean-images path n)
 (rm (path 'output-image (number->string n)))
 (rm (path 'output-contour (number->string n)))
 (rm (path 'output-graph))
 (rm (path 'numbered-graph)))

(define (rc-clean-everything path n)
 (rm (path 'pb))
 (rm (path 'superpixels))
 (rm (path 'graph))
 (rm (path 'lines))
 (rm (path 'weights))
 (rm (path 'cycle (number->string n)))
 (rm (path 'output-image (number->string n)))
 (rm (path 'output-contour (number->string n)))
 (rm (path 'output-graph))
 (rm (path 'numbered-graph))
 (rm (path 'chains))
 (rm (path 'cycle-sc)))

(define (create-sequence n)
 (define (sequence n)
  (cond
   ((< n 0) (fuck-up))
   ((zero? n) (list))
   (else (append (list (- n 1)) (sequence (- n 1))))))
 (reverse (sequence n)))

(define (rc path chains number-of-contours)
 (if #t
     (begin
      (rc-graph->graph-file! chains (path 'graph))
      (start-matlab!)
      (display
       (matlab
	(string-append "addpath('../rc/'); convertGraph('" (strip-extension (path 'graph)) "');")))
      (newline))
     (begin
      (newline)
      ;; (write-object-to-file chains (string-append pathname ".chains"))
      ;; (write-w-file (convert-graph (rc-chains->graph chains)) (string-append pathname ".w"))
      ))
 (display (string-append "../rc/RatioContour3 " (strip-extension (path 'weights)) " " (number->string number-of-contours))) (newline)
 (system (string-append "../rc/RatioContour3 " (strip-extension (path 'weights)) " " (number->string number-of-contours)))
 (map
  (lambda (n)
   (let ((cycle (read-cycle-edges chains (path 'cycle (number->string n)))))
   (rc-clean path n)
   cycle))
  (create-sequence number-of-contours)))

(define (compute-weights! chain edge-image)
 (for-each
  (lambda (e)
   (let* ((w2 (compute-dashed-weight2 (solid-edge-superpixel-u e) (solid-edge-superpixel-v e)))
	  (alpha 0)
	  (w1-temp (compute-w1 e edge-image))
	  (w1 (if (< w1-temp alpha)
		  w1-temp
		  ;; (compute-w1 e edge-image)
		  (max 0 (- (compute-w1 e edge-image) alpha))))
	  ;; (w1 (compute-w1 e edge-image))
	  )
    (set-solid-edge-superpixel-w1! e w1)
    (set-solid-edge-superpixel-w2! e w2)))
  (rc-chains-solid-edges chain))
 (for-each
  (lambda (e)
   (let* ((w1 0)
	  (w2 (dashed-edge-w2 e)))
    (set-dashed-edge-w1! e w1)
    (set-dashed-edge-w2! e w2)))
  (rc-chains-dashed-edges chain)))

;;; Top Level

;; Not accurate right now
;;  darpa-wrap ./compute-optical-flow  -darpa Chase2_A1_C1_Act1_PARK1_MC_AFTN_47ce0b23-c5af-11df-a82b-e80688cb869a

(define (generate-berkeley image destination)
 (let ((f (string-append "waggonej-" (number->string (getpid)))))
  (rm (format #f "/tmp/~a.ppm" f))
  (rm (format #f "/tmp/~a-berkeley.pgm" f))
  (rm (format #f "~a-berkeley.pgm_pbs.mat_lat.tmp" f))
  (rm (format #f "~a-berkeley.pgm_pbs.mat_lat.jpg" f))
  (write-pnm image (format #f "/tmp/~a.ppm" f))
  (system (format #f "~~/darpa-collaboration/bin/run-berkeley /tmp/~a.ppm /tmp/~a-berkeley.pgm" f f))
  (system (format #f "mv /tmp/~a-berkeley.pgm ~a" f destination))
  (rm (format #f "/tmp/~a.ppm" f))
  (rm (format #f "/tmp/~a-berkeley.pgm" f))
  (rm (format #f "~a-berkeley.pgm_pbs.mat_lat.tmp" f))
  (rm (format #f "~a-berkeley.pgm_pbs.mat_lat.jpg" f)))
 (read-pnm destination))

;; ../../bin/darpa-wrap ./generate-superpixels-slic -darpa Approach7_A2_C1_Act3_URBAN6_MC_AFTN_4878ba73-c5af-11df-a416-e80688cb869a -file frame-cropped-person-1.ppm -nr-superpixels 1000 -weight 15
(define (generate-slic image destination number-of-superpixels spatial-proximity-weight)
 (let ((f (string-append "waggonej-" (number->string (getpid)))))
  (rm (format #f "/tmp/~a.ppm" f))
  (rm (format #f "/tmp/~a.dat" f))
  (rm (format #f "/tmp/~a_slic.ppm" f))
  (rm (format #f "/tmp/~a_slic.png" f))
  (write-pnm image (format #f "/tmp/~a.ppm" f))
  (system (format #f "cd /tmp;~~/darpa-collaboration/bin/slic ~a.ppm ~s ~s" f number-of-superpixels spatial-proximity-weight))
  (system (format #f "mv /tmp/~a.dat ~a" f destination))
  (rm (format #f "/tmp/~a.ppm" f))
  (rm (format #f "/tmp/~a.dat" f))
  (rm (format #f "/tmp/~a_slic.ppm" f))
  (rm (format #f "/tmp/~a_slic.png" f)))
 (read-slic-file destination (pnm-width image) (pnm-height image)))

(define (fix-voc4-box image voc4)
 (define (bounds-check-x image val)
  (max 0 (min (pnm-width image) val)))
 (define (bounds-check-y image val)
  (max 0 (min (pnm-height image) val)))
 (make-voc4-detection
  (bounds-check-x image (voc4-detection-x1 voc4))
  (bounds-check-y image (voc4-detection-y1 voc4))
  (bounds-check-x image (voc4-detection-x2 voc4))
  (bounds-check-y image (voc4-detection-y2 voc4))
  (voc4-detection-filter voc4)
  (voc4-detection-strength voc4)
  (voc4-detection-delta voc4)
  (voc4-detection-model voc4)))

;; (run-rc-on-frames (string->darpa-video "Approach7_A2_C1_Act3_URBAN6_MC_AFTN_4878ba73-c5af-11df-a416-e80688cb869a"))

(define (dtrace s v) (format #t "~a: ~a~%" s v) v)

;; (run-rc-on-image "../qobi/purdue36-9/fall3-0106")
(define (run-rc-on-image image-name)
 (start-matlab!)
 (let ((path (lambda (type . args)
	      (apply
	       generate-pathname
	       (append
		`("" ,image-name ,type)
		args)))))
  (let* ((image (read-pnm (path 'image)))
	 (globalpb
	  (if (file-exists? (path 'pb))
	      (read-pnm
	       (path 'pb))
	      (generate-berkeley image (path 'pb))))
	 (slic
	  (if (file-exists?
	       (path 'superpixels))
	      (read-slic-file
	       (path 'superpixels)
	       (pnm-width image)
	       (pnm-height image))
	      (generate-slic image (path 'superpixels) 300 15)))
	 (distance-matrix (distance-matrix-pb globalpb )))
   (write-pnm (binary-threshold-optimal globalpb) (string-append image-name "-optimal-threshold.pbm"))
   (run-rc-superpixels path image slic distance-matrix 4))))

(define (run-rc-on-frames video-name)
 (start-matlab!)
 (for-each-frame
  (lambda (frame)
   ;; (format #t "Frame ~a~%" frame)
   (let ((path (lambda (type . args)
		(apply
		 generate-pathname
		 (append
		  `(,(generic-pathname video-name frame "") "frame-cropped" ,type "person" "1")
		  args)))))
    (when (and
	   (file-exists? (ppm-full-pathname video-name frame))
	   (not (file-exists? (path 'output-image)))) ;; Added to skip ones already done
     (let* ((model "person")
	    (model-number 1)
	    (full-image (read-pnm (ppm-full-pathname video-name frame)))
	    (image
	     (if (file-exists? (path 'image))
		 (read-pnm (path 'image))
		 (crop-voc4
		  full-image
		  (fix-voc4-box
		   full-image
		   (voc4-bloat
		    (first (read-voc4-boxes
			    (smooth-tracked-box-pathname video-name frame model (number->string model-number))))
		    0.2)))))
	    (globalpb
	     (if (file-exists? (cropped-berkeley-pathname video-name frame model model-number))
		 (read-pnm (path 'pb))
		 (generate-berkeley image (path 'pb))))
	    (slic
	     (if (file-exists? (path 'superpixels))
		 (read-slic-file (path 'superpixels)
		  (pnm-width image)
		  (pnm-height image))
		 (generate-slic image (path 'superpixels) 300 15)))
	    (distance-matrix (distance-matrix-pb globalpb )))
      (when (not (file-exists? (path 'image)))
       (write-pnm image (path 'image)))
      (run-rc-superpixels path image slic distance-matrix 4)))))
   video-name))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up)))))
  (run-rc-on-frames video-name)))

;; Older

(define (remove-dashed-edge chains ind-u ind-v)
 (remove-if
  (lambda (e)
   (or
    (and
     (eq? (label (dashed-edge-u e)) ind-u)
     (eq? (label (dashed-edge-v e)) ind-v))
    (and
     (eq? (label (dashed-edge-u e)) ind-v)
     (eq? (label (dashed-edge-v e)) ind-u))))
  (rc-chains-dashed-edges chains)))

(define (supress-other-edges! chains preselected-edges)
 (let ((counter 0))
  (for-each
   (lambda (e)
    (set! counter (+ counter 1))
    (when (eq? (member (- counter 1) preselected-edges) #f)
     (set-solid-edge-superpixel-w1! e *a*))
    (when (not (eq? (member (- counter 1) preselected-edges) #f))
     (set-solid-edge-superpixel-w1! e 0)))
   (rc-chains-solid-edges chains))))

(define (outside-crop-point? p p1 p2)
 (or (< (x p) (x p1)) (< (y p) (y p1)) (> (x p) (x p2)) (> (y p) (y p2))))

(define (crop-edges edges p1 p2)
 ;; needs work: ugly code
 (remove-if
  (lambda (e)
   (cond ((solid-edge-superpixel? e)
	  (or (outside-crop-point? (first (rc-vertex-pixels (solid-edge-superpixel-u e))) np1 p2)
	      (outside-crop-point? (first (rc-vertex-pixels (solid-edge-superpixel-v e))) p1 p2)))
	 ((dashed-edge? e)
	  (or
	   (outside-crop-point? (first (rc-vertex-pixels (dashed-edge-u e))) p1 p2)
	   (outside-crop-point? (first (rc-vertex-pixels (dashed-edge-v e))) p1 p2)))
	 (else (fuck-up))))
  edges))

(define (reindex-edges! solid-edge)
 (for-each-indexed
  (lambda (e n)
   (let ((u (solid-edge-superpixel-u e)) (v (solid-edge-superpixel-v e)))
    (format #t "~a,~a ~a,~a~%" (label u) (* 2 n) (label v) (+ (* 2 n) 1))
    (set-rc-vertex-pixels! u (list (point u) (* 2 n)))
    (set-rc-vertex-pixels! v (list (point v) (+ (* 2 n) 1)))))
  solid-edge))

(define (crop-graph g)
 (let ((g (list
	   (first g)
	   (crop-edges (second g) '#(140 171) '#(153 195))
	   (third g)))
       (g (list
	   (first g)
	   (second g)
	   (remove-if-not
	    (lambda (e)
	     (and (find-if (lambda (s)
			    (or (eq? (dashed-edge-u e) (solid-edge-superpixel-u s))
				(eq? (dashed-edge-u e) (solid-edge-superpixel-v s))))
			   (second g))
		  (find-if (lambda (s)
			    (or (eq? (dashed-edge-v e) (solid-edge-superpixel-u s))
				(eq? (dashed-edge-v e) (solid-edge-superpixel-v s))))
			   (second g))))
	    (third g)))))
  (reindex-edges! (second g))
  g))

(define (edge-length e)
 (distance (first (rc-vertex-pixels (solid-edge-superpixel-u e)))
	   (first (rc-vertex-pixels (solid-edge-superpixel-v e)))))

;;; Early testing cases

(define *files*
 '("test-cropped-0001"
   "test-cropped-0002"
   "step3_circle"
   "fall3-0054"
   "fall3-0106"
   "fall3-0124"
   "jump7-0001"
   "jump7-0019"
   "jump7-0034"
   "run1-0002"
   "run1-0008"
   "run1-0044"))

(define *new-files*
 '(;;"test-cropped-0001"
   ;;"test-cropped-0002"
   ;;"step3_circle"
   "../qobi/purdue36-9/fall3-0054"
   "../qobi/purdue36-9/fall3-0106"
   "../qobi/purdue36-9/fall3-0124"
   "../qobi/purdue36-9/jump7-0001"
   "../qobi/purdue36-9/jump7-0019"
   "../qobi/purdue36-9/jump7-0034"
   "../qobi/purdue36-9/run1-0002"
   "../qobi/purdue36-9/run1-0008"
   "../qobi/purdue36-9/run1-0044"))

(define (test1 pathname)
 (display pathname) (newline)		;debugging
 (let ((g (person->superpixel-graph pathname)))
  (for-each (lambda (v)
	     (unless (subset? (superpixel-vertex-boundary-pixels v)
			      (superpixel-vertex-region-pixels v))
	      (fuck-up)))
	    (graph-vertices g))
  (for-each
   (lambda (u)
    (for-each
     (lambda (v)
      (unless (or (eq? u v)
		  (null? (intersection (superpixel-vertex-region-pixels u)
				       (superpixel-vertex-region-pixels v))))
       (fuck-up)))
     (graph-vertices g)))
   (graph-vertices g))))

(define (test1-all) (for-each test1 *files*))

(define (test2 pathname)
 (display pathname) (newline)		;debugging
 (write-pnm (graph->ppm (person->graph pathname))
	    (default-extension
	     (string-append (strip-extension pathname) "-graph") "ppm")))

(define (test2-all) (for-each test2 *files*))

(define (test3 pathname)
 (display pathname) (newline)		;debugging
 (let ((g (person->graph pathname)))
  (draw-cycle-as-contour
   (map (lambda (e) (random-boolean)) (graph-edges g))
   g
   (read-pnm (default-extension pathname "ppm"))
   (string-append (strip-extension pathname) "-graph"))))

(define (test3-all) (for-each test3 *files*))


(define (points->pgm ps)
 (let* ((ps (map (lambda (p) (map-vector exact-round (k*v 2 p))) ps))
	(height (+ (map-reduce max minus-infinity y ps) 1))
	(width (+ (map-reduce max minus-infinity x ps) 1))
	(grey (make-matrix height width 0)))
  (for-each (lambda (p) (matrix-set! grey (y p) (x p) 255)) ps)
  (make-pgm #t 255 grey)))

(define (points->ppm ps qs rs ts)
 (let* ((ps (map (lambda (p) (map-vector exact-round (k*v 2 p))) ps))
	(qs (map (lambda (q) (map-vector exact-round (k*v 2 q))) qs))
	(rs (map (lambda (r) (map-vector exact-round (k*v 2 r))) rs))
	(ts (map (lambda (t) (map-vector exact-round (k*v 2 t))) ts))
	(height (+ (max (map-reduce max minus-infinity y ps)
			(map-reduce max minus-infinity y qs)
			(map-reduce max minus-infinity y rs)
			(map-reduce max minus-infinity y ts))
		   1))
	(width (+ (max (map-reduce max minus-infinity x ps)
		       (map-reduce max minus-infinity x qs)
		       (map-reduce max minus-infinity x rs)
		       (map-reduce max minus-infinity x ts))
		  1))
	(red (make-matrix height width 0))
	(green (make-matrix height width 0))
	(blue (make-matrix height width 0)))
  (for-each (lambda (p) (matrix-set! green (y p) (x p) 255)) ps)
  (for-each (lambda (r) (matrix-set! blue (y r) (x r) 255)) rs)
  (for-each (lambda (t) (matrix-set! red (y t) (x t) 255)) ts)
  (for-each (lambda (q)
	     (matrix-set! red (y q) (x q) 255)
	     (matrix-set! green (y q) (x q) 255)
	     (matrix-set! blue (y q) (x q) 255))
	    qs)
  (make-ppm #t 255 red green blue)))

(define (graph->ppm g)
 (let* ((vs (map (lambda (v)
		  (map-vector exact-round (k*v 2 (first (rc-vertex-pixels v)))))
		 (graph-vertices g)))
	(es (map-reduce
	     append
	     '()
	     (lambda (v)
	      (map (lambda (p) (map-vector exact-round (k*v 2 p)))
		   (solid-edge-superpixel-pixels v)))
	     (graph-edges g)))
	(height (+ (max (map-reduce max minus-infinity y vs)
			(map-reduce max minus-infinity y es))
		   1))
	(width (+ (max (map-reduce max minus-infinity x vs)
		       (map-reduce max minus-infinity x es))
		  1))
	(red (make-matrix height width 0))
	(green (make-matrix height width 0))
	(blue (make-matrix height width 0)))
  (for-each (lambda (p) (matrix-set! red (y p) (x p) 255)) vs)
  (for-each (lambda (p) (matrix-set! green (y p) (x p) 255)) es)
  (make-ppm #t 255 red green blue)))


(define (draw-cycle-as-contour labels graph ppm pathname)
 (let* ((height (pnm-height ppm))
	(width (pnm-width ppm))
	(red (make-matrix height width))
	(green (make-matrix height width))
	(blue (make-matrix height width)))
  (for-each-n
   (lambda (y)
    (for-each-n (lambda (x)
		 (matrix-set! red y x (matrix-ref (ppm-red ppm) y x))
		 (matrix-set! green y x (matrix-ref (ppm-green ppm) y x))
		 (matrix-set! blue y x (matrix-ref (ppm-blue ppm) y x)))
		width))
   height)
  (for-each (lambda (e l)
	     (when l
	      (for-each (lambda (p)
			 (define (draw-red y x)
			  (matrix-set! red y x 255)
			  (matrix-set! green y x 0)
			  (matrix-set! blue y x 0))
			 (draw-red (exact-floor (y p)) (exact-floor (x p)))
			 (draw-red (exact-floor (y p)) (exact-ceiling (x p)))
			 (draw-red (exact-ceiling (y p)) (exact-floor (x p)))
			 (draw-red (exact-ceiling (y p)) (exact-ceiling (x p))))
			(solid-edge-superpixel-pixels e))))
	    (graph-edges graph)
	    labels)
  (write-pnm (make-ppm (ppm-raw? ppm) 255 red green blue)
	     (default-extension pathname "ppm"))))

;; Old routines to read in files

(define (berkeley-file input-pathname)
 (string-append input-pathname "-berkeley.pgm"))

(define (pb input-pathname) (read-pnm (berkeley-file input-pathname)))

(define (thresh-pb input-pathname)
  (let ((thresholded (binary-threshold-optimal (pb input-pathname))))
   (write-pnm thresholded (string-append input-pathname "-thresholded-pb.pgm"))
  thresholded))

(define (distance-pb input-pathname)
 (let* ((d (distance-transform (thresh-pb input-pathname)))
	(mat-max (maximum-matrix (first d))))
  ;; debugging
  (write-pnm (make-pgm
	      #f
	      255
	      (map-matrix
	       (lambda (value) (exact-round (* (/ value mat-max) 255)))
	       (first d)))
	     (string-append input-pathname "-distance-matrix.pgm"))
  d))

(define (edge-distance-pb input-pathname)
 (let* ((d (edge-mask (pb input-pathname) 10)))
  ;; debugging
  (write-pnm d (string-append input-pathname "-edge-distance-matrix.pgm"))
  d))

(define (dialated-berkeley-file input-pathname)
 (string-append input-pathname "-berkeley-dilated.pgm"))

(define (dialated-pb input-pathname)
 (if #t
     (pgm-dialate 3 (pb input-pathname))
     (read-pnm (dialated-berkeley-file input-pathname))))
