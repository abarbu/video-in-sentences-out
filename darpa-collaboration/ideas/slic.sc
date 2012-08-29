(MODULE
 SLIC
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
(include "slic.sch")

(set! *program* "slic")
(set! *panic?* #f)

(define dummy image-ref)

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

(define (superpixel-random-color a)
 (srand a)
 `#(,(random-integer 255)
    ,(random-integer 255)
    ,(random-integer 255)))

(define (uniform-grid s rows columns)
 (remove-if (lambda (p) (or (>= (x p) columns) (>= (y p) rows)))
	    (map-reduce-n
	     append
	     '()
	     (lambda (a) (map-n (lambda (b) (v+ `#(,(/ s 2) ,(/ s 2))
				      (v+ (k*v b `#(,s 0)) (k*v a `#(0 ,s)))))
			   s))
	     s)))

(define (slic-ppm ppm k m) (slic (ppm->data ppm) k m))

;; (define (slic data k m)
;;  ;; k number of superpixels, m compactness
;;  (define (image-gradient x y)
;;   (+ (sqr (magnitude (v- (matrix-ref data y (+ x 1)) (matrix-ref data y (- x 1)))))
;;      (sqr (magnitude (v- (matrix-ref data (+ y 1) x) (matrix-ref data (- y 1) x))))))
;;  (define (image-value data c) `#(,c ,(image-ref data c)))
;;  (define (vector-average l)
;;   ;; todo quantization, this means all centers are at integral points after every step
;;   (vector (quantize-point (list-mean (map x l))) (list-mean (map y l))))
;;  (let* ((n (* (matrix-rows data) (matrix-columns data)))
;; 	(s (exact-round (sqrt (/ n k))))
;; 	(data (map-matrix rgb->l*ab data))
;; 	(centers (map (lambda (v) (map-vector (lambda (e) (inexact->exact (floor e))) v))
;; 		      (uniform-grid s (matrix-rows data) (matrix-columns data))))
;; 	(centers
;; 	 (if #t
;; 	     centers
;; 	     (map
;; 	      (lambda (c)
;; 	       (image-value data (minimump
;; 				  (neighbours-8 data (y c) (x c))
;; 				  (lambda (p) (image-gradient (x p) (y p))))))
;; 	      centers)))
;; 	(centers (map (lambda (c) (image-value data c)) centers)))

;;   (define (distance-metric a b)
;;    (+ (distance (x a) (x b)) (* (/ m s) (distance (y a) (y b)))))

;;   (define (label data centers)
;;    (let ((labels (time-code (make-matrix (matrix-rows data) (matrix-columns data) #f))))
;;     (time-code
;;      (for-each (lambda (c)
;; 		(let ((xc (x (x c))) (yc (y (x c))))
;; 		 (for-each-pixel-in-rectangle
;; 		  xc yc s s
;; 		  (lambda (x y)
;; 		   (when (and (< x (matrix-columns data)) (>= x 0)
;; 			      (< y (matrix-rows data)) (>= y 0))
;; 		    (let ((i (matrix-ref labels y x))
;; 			  (d (image-value data (vector x y))))
;; 		     (when (or (not i)
;; 			       (< (distance-metric c d) (distance-metric i d)))
;; 		      (matrix-set! labels y x c))))))))
;; 	       centers))
;;     labels))

;;   (define (recompute-centers labels centers)
;;    ;; todo deal with centers going away
;;    (let ((centers (time-code (map (lambda (c) (vector c '())) centers))))
;;     (time-code
;;      (for-each-indexed-matrix (lambda (r i j)
;; 			       (let ((c (find-if (lambda (c) (equal? (x c) r)) centers)))
;; 				(vector-set! c 1 (cons (vector j i) (y c)))))
;; 			      labels))
;;     (time-code
;;      (map (lambda (c) (vector-average (map (lambda (p) (image-value data p)) (y c)))) centers))))

;;   (let loop ((n 10) (centers centers) (old-centers #f))
;;    (if (and (> n 0) (not (equal? old-centers (first centers))))
;;        (loop (- n 1)
;; 	     (time-code (recompute-centers (time-code (label data centers)) centers))
;; 	     (map-vector identity (first centers)))
;;        (begin
;; 	(when (equal? old-centers (first centers))
;; 	 (format #t "Stopped at iteration ~a because centers didn't change~%" (- 10 n)))
;; 	(map-matrix (lambda (a) (position a centers)) (label data centers)))))))

(define (slic->superpixels slic)
 (map slic->superpixel (slic-frame->regions slic)))

(define test1
 '#(#(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
    #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
    #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
    #(#(0 0 0) #(0 0 0) #(255 255 255) #(255 255 255) #(255 255 255) #(255 255 255))
    #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))))

(define (labelling->ppm labelling)
 (let ((image (ppm-constant (matrix-columns labelling) (matrix-rows labelling) 0 0 0)))
  (map-indexed-vector
   (lambda (r i)
    (map-indexed-vector
     (lambda (e j)
      (set-ppm-pixel! image j i (superpixel-random-color e)))
     r))
   labelling)
  image))

(define (ppm->data ppm)
 (map-indexed-matrix
  (lambda (r i j)
   (vector r
	   (matrix-ref (ppm-green ppm) i j)
	   (matrix-ref (ppm-blue ppm) i j)))
  (ppm-red ppm)))

(define (data->ppm data)
 (make-ppm #t 255
	   (map-matrix r data)
	   (map-matrix g data)
	   (map-matrix b data)))

(define-command
 (main (at-most-one ("nr-superpixels" nr-superpixels?
		     (nr-superpixels "integer" integer-argument 200)))
       (at-most-one ("affinity" affinity?
		     (affinity "integer" integer-argument 20)))
       (rest (pathnames "pathname" string-argument)))
 (for-each-indexed
  (lambda (pathname i)
   (write-pnm (labelling->ppm (time-code (slic (ppm->data (read-pnm pathname)) nr-superpixels affinity)))
	      (string-append "/tmp/slic-" (number->string i) ".ppm")))
  pathnames))

;; (define-command
;;  (main (at-most-one ("nr-superpixels" nr-superpixels?
;; 		     (nr-superpixels "integer" integer-argument 200)))
;;        (at-most-one ("affinity" affinity?
;; 		     (affinity "integer" integer-argument 20)))
;;        (rest (pathnames "pathname" string-argument)))
;;  (for-each-frame-pair individual-f pair-f video-name)
;;  (optical-flow a b)
;;  (for-each-indexed
;;   (lambda (pathname i)
;;    (write-pnm (labelling->ppm (first (time-code (slic (ppm->data (read-pnm pathname)) nr-superpixels affinity))))
;; 	      (string-append "/tmp/slic-" (number->string i) ".ppm")))
;;   pathnames))

;; (define (superpixel-correspondence ss ss-map)
;;  ;; TODO Check image boundaries
;;  ;; TODO Record magnitudes and pick the max
;;  (image-ref
;;   ss-map
;;   (quantize-point (if (superpixel-velocity ss)
;; 		      (v+ (superpixel-center ss) (superpixel-velocity ss))
;; 		      (superpixel-center ss)))))

(define (superpixels-width superpixels)
 (maximum (map (lambda (a) (maximum (map x (superpixel-pixels a)))) superpixels)))
(define (superpixels-height superpixels)
 (maximum (map (lambda (a) (maximum (map y (superpixel-pixels a)))) superpixels)))

;; (define (temporal-slic-method-1 video-name object k s)
;;  ;; width, height
;;  (for-each-frame-pair
;;   (lambda (frame)
;;    (let ((labelling (slic (ppm->data (read-ppm (ppm-pathname video-name frame))) k s)))
;;     (write-object-to-file superpixels (slic-pathname video-name frame "vanilla"))
;;     (list frame (slic->superpixels labelling))))
;;   (lambda (f1 f2)
;;    (let ((labelling-2
;; 	  (map-matrix
;; 	   (lambda (e) (vector 0 e))
;; 	   (superpixels->map (first f2)
;; 			     (superpixels-width (first f2))
;; 			     (superpixels-height (first f2)))))
;; 	 (flow (optical-flow (ppm-pathname video-name (first f1))
;; 			     (ppm-pathname video-name (first f2)))))
;;     (write-object-to-file flow (optical-flow-pathname video-name frame))
;;     (for-each (lambda (s)
;; 	       (set-superpixel-velocity! s (average-optical-flow-superpixel s flow)))
;; 	      (second f1))
;;     (for-each (lambda (s)
;; 	       (when (not (superpixel-next s))
;; 		(set-superpixel-next! s (list (superpixel-track s (z next))))))
;; 	      (second f1))))
;;   video-name))

(define (slic data k m)
 ;; k number of superpixels, m compactness
 (define (image-gradient x y)
  (+ (sqr (magnitude (v- (matrix-ref data y (+ x 1)) (matrix-ref data y (- x 1)))))
     (sqr (magnitude (v- (matrix-ref data (+ y 1) x) (matrix-ref data (- y 1) x))))))
 (define (image-value data c) `#(,c ,(image-ref data c)))
 (define (slic-n data) (* (matrix-rows data) (matrix-columns data)))
 (define (slic-s data k) (exact-round (sqrt (/ (slic-n data) k))))
 (define (initial-centers data k m)
  (let ((centers (map (lambda (v) (map-vector (lambda (e) (inexact->exact (floor e))) v))
		      (uniform-grid (slic-s data k) (matrix-rows data) (matrix-columns data)))))
   (if #t				; debugging
       centers
       (map
	(lambda (c)
	 (image-value data (minimump
			    (neighbours-8 data (y c) (x c))
			    (lambda (p) (image-gradient (x p) (y p))))))
	centers))))
 (define (vector-average l)
  ;; todo quantization, this means all centers are at integral points after every step
  (vector (list-mean (map x l)) (list-mean (map y l))))
 (let* ((n (* (matrix-rows data) (matrix-columns data)))
	(s (exact-round (sqrt (/ n k))))
	(data (map-matrix rgb->l*ab data))
	(centers (initial-centers data k m))
	(centers (map (lambda (c) (image-value data c)) centers)))

  (define (distance-metric a b)
   (+ (distance (x a) (x b)) (* (/ m s) (distance (y a) (y b)))))

  (define (label data centers)
   (let ((labels (make-matrix (matrix-rows data) (matrix-columns data) #f)))
    (for-each (lambda (c)
	       (let ((xc (x (x c))) (yc (y (x c))))
		(for-each-pixel-in-rectangle
		 (quantize-coordinate xc)
		 (quantize-coordinate yc) (* s 2) (* s 2)
		 (lambda (x y)
		  (when (and (< x (matrix-columns data)) (>= x 0)
			     (< y (matrix-rows data)) (>= y 0))
		   (let ((i (matrix-ref labels y x))
			 (d (image-value data (vector x y))))
		    (when (or (not i)
			      (< (distance-metric c d) (distance-metric i d)))
		     (matrix-set! labels y x c))))))))
	      centers)
    labels))

  (define (recompute-centers labels centers)
   ;; todo deal with centers going away
   (let ((centers (map (lambda (c) (vector c '())) centers)))
    (for-each-indexed-matrix (lambda (r i j)
			      (let ((c (find-if (lambda (c) (equal? (x c) r)) centers)))
			       (unless c
				(format #t "FAILED ~a ~%" r)
				(fuck-up))
			       (vector-set! c 1 (cons (vector j i) (y c)))))
			     labels)
    (map (lambda (c)
	  (if (null? (y c))
	      (x c)
	      (vector-average (map (lambda (p) (image-value data p)) (y c)))))
	 centers)))

  (let loop ((n 10) (centers centers) (old-centers #f))
   (format #t "Loop ~a~%" n)
   (if (and (> n 0) (not (equal? old-centers (first centers))))
       (loop (- n 1)
	     (time-code (recompute-centers (time-code (label data centers)) centers))
	     (map-vector identity (first centers)))
       (begin
	(when (equal? old-centers (first centers))
	 (format #t "Stopped at iteration ~a because centers didn't change~%" (- 10 n)))
	(map-matrix (lambda (a) (position a centers)) (label data centers)))))))

(define (label-connected-components )
 )

;; (pp (slic test1 3 20))

;; (pp (slic test2 10 20))


;; (define data (ppm->data (read-pnm "/home/andrei/darpa-collaboration/qobi/purdue36-9/test-cropped-0001.ppm")))
;; (define labelling (slic data 10 20))
;; (show-image (labelling->ppm (first labelling)))

;; ;; (pp (slic test1 3 20))

;; (pp (slic test1 3 20))
;; (define labelling (slic data 10 20))

;; (show-image
;;  (labelling->ppm
;;   (first
;;    (slic
;;     (ppm->data
;;      (read-pnm "/home/andrei/darpa-collaboration/qobi/purdue36-9/test-cropped-0001.ppm"))
;;     20 20))))

;; (define test2
;;  '#(#(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
;;     #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
;;     #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
;;     #(#(0 0 0) #(0 0 0) #(255 255 255) #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(255 255 255) #(255 255 255) #(255 255 255) #(255 255 255))
;;     #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
;;     #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
;;     #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
;;     #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))
;;     #(#(0 0 0) #(0 0 0) #(255 255 255) #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(255 255 255) #(255 255 255) #(255 255 255) #(255 255 255))
;;     #(#(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255) #(0 0 0) #(0 0 0) #(0 0 0)       #(255 255 255) #(255 255 255) #(255 255 255))))

;; (define (neighbours-8 i j)
;;  (list `#(,(+ i 1) ,j)
;;        `#(,(- i 1) ,j)
;;        `#(,i ,(+ j 1))
;;        `#(,i ,(- j 1))
;;        `#(,(+ i 1) ,(+ j 1))
;;        `#(,(- i 1) ,(+ j 1))
;;        `#(,(+ i 1) ,(- j 1))
;;        `#(,(- i 1) ,(- j 1))))

(define (labelling->names labelling)
 (remove-duplicates (map-reduce
		     append
		     '()
		     vector->list
		     (vector->list labelling))))

(define (superpixel-connectivity labelling)
 (let ((cc-pixels
	(map
	 (lambda (cc) (map vertex-pixels (graph-vertices cc)))
	 (connected-components (labeling->graph labelling 1))))
       (cc-map (regions->frame cc-pixels w h)))
  (for-each (lambda (a)
	     (image-ref cc-map (center a)) ;;;;;
	     )
	    labelling)
 )

(let ((a (slic test2 10 20)))
   (pp (labelling->names a))(newline)
   (postprocessing-connectivity! a (labelling->names a))
   (pp a)(newline))

(define c (connected-components (labeling->graph l 1)))

(define (regions->frame cc-pixels w h)
 (let ((m (make-matrix h w 0)))
  (for-each-indexed
   (lambda (pixels i) (map (lambda (p) (matrix-set! m (y p) (x p) i)) pixels))
   cc-pixels)
  m))
