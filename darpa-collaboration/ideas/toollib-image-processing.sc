(MODULE TOOLLIB-IMAGE-PROCESSING)

(include "QobiScheme.sch")
(include "toollib-c-macros.sch")
(include "toollib-image-processing.sch")

(c-include "idealib-c.h")

;; touch these functions so that sch knows to pick them up, they're
;; only used inside unquoted s-expressions
(define (image-processing:dummy)
 exact-round
 exact-ceiling
 matlab->scheme
 matlab-variable
 matlab-get-variable
 map-indexed-vector)

(define (image-ref i p)
 (cond ((pbm? i) (matrix-ref (pbm-bitmap i) (y p) (x p)))
       ((pgm? i) (matrix-ref (pgm-grey i) (y p) (x p)))
       ((ppm? i)
	`#(,(matrix-ref (ppm-red i) (y p) (x p))
	   ,(matrix-ref (ppm-blue i) (y p) (x p))
	   ,(matrix-ref (ppm-green i) (y p) (x p))))
       ((matrix? i) (matrix-ref i (y p) (x p)))
       (else (fuck-up))))

(define (pnm? m) (or (pbm? m) (pgm? m) (ppm? m)))

(define (rgb->cmyk rgb)
 (let* ((r (vector-ref rgb 0))
	(g (vector-ref rgb 1))
	(b (vector-ref rgb 2))
	(k (max r g b)))
  (if (zero? k)
      '#(0 0 0 0)
      `#(,(exact-round (* 255 (- 1 (/ r k))))
	 ,(exact-round (* 255 (- 1 (/ g k))))
	 ,(exact-round (* 255 (- 1 (/ b k))))
	 ,k))))

(define (rgb->uv-hsv c)
 (let ((hsv (rgb->hsv c)))
  `#(,(exact-round (+ (- (* 0.169 (x c))) (* 0.331 (y c)) (* 0.5 (z c))))
     ,(exact-round (+ (* 0.5 (x c)) (- (* 0.418 (y c))) (- (* 0.082 (z c)))))
     ,(x hsv) ,(y hsv) ,(z hsv))))

;; threshold used to default to 30
(define (line-filter lines min max threshold)
 (remove-if
  (lambda (l) (or (or (> (x (p l)) (x max)) (> (x (q l)) (x max)))
	     (or (< (x (p l)) (x min)) (< (x (q l)) (x min)))
	     (or (> (y (p l)) (y max)) (> (y (q l)) (y max)))
	     (or (< (y (p l)) (y min)) (< (y (q l)) (y min)))
	     (< (line-segment-length l) threshold)))
  lines))

(define (crop-image pnm x y width height)
 (cond ((pbm? pnm) (make-pbm #f (crop (pbm-bitmap pnm) x y width height)))
       ((pgm? pnm) (make-pgm (pgm-raw? pnm)
			     (pgm-maxval pnm)
			     (crop (pgm-grey pnm) x y width height)))
       ((ppm? pnm) (make-ppm (ppm-raw? pnm)
			     (ppm-maxval pnm)
			     (crop (ppm-red pnm) x y width height)
			     (crop (ppm-green pnm) x y width height)
			     (crop (ppm-blue pnm) x y width height)))
       (else (panic "Image must be one of PBM, PGM or PPM"))))

(define (pbm-ascii pbm) (make-pbm #f (pbm-bitmap pbm)))

(define (flatten-ppm ppm colour-transform)
 (map-vector
  (lambda (red-row green-row blue-row)
   (map-vector
    (lambda (r g b)
     (colour-transform `#(,r ,g ,b)))
    red-row green-row blue-row))
  (ppm-red ppm) (ppm-green ppm) (ppm-blue ppm)))

(define (ppm-mean image colour-transform)
 (let ((acc (colour-transform '#(0 0 0))))
  (for-each-vector
   (lambda (red-row green-row blue-row)
    (for-each-vector
     (lambda (r g b)
      (set! acc (v+ acc (colour-transform `#(,r ,g ,b)))))
     red-row green-row blue-row))
   (ppm-red image) (ppm-green image) (ppm-blue image))
  (k*v (/ 1 (* (pnm-height image) (pnm-width image))) acc)))

(define (ppm-covariance image colour-transform)
 (let* ((mu (ppm-mean image colour-transform))
	(acc (make-matrix (vector-length mu) (vector-length mu) 0)))
  (for-each-vector
   (lambda (red-row green-row blue-row)
    (for-each-vector
     (lambda (r g b)
      (set!
       acc
       (m+ acc (self-outer-product * (v- (colour-transform `#(,r ,g ,b)) mu)))))
     red-row green-row blue-row))
   (ppm-red image) (ppm-green image) (ppm-blue image))
  (k*m (/ 1 (* (pnm-height image) (pnm-width image))) acc)))

(define (pgm-mean image)
 (histogram-mean (find-histogram (pgm-grey image) (pgm-maxval image)) 0))

(define (pgm-variance image)
 (let ((hist (find-histogram (pgm-grey image) (pgm-maxval image))))
  (histogram-variance hist (histogram-mean hist 0) 0)))

(define (pgm-and-pbm pgm pbm)
 (map-vector (lambda (eg eb) (map-vector (lambda (eg eb) (if eb eg 0)) eg eb))
	     (pgm-grey pgm) (pbm-bitmap pbm)))

(define (pbm-skeletonize pbm)
 (let ((height (pnm-height pbm)) (width (pnm-width pbm)))
  (let loop ((bitmap (pbm-bitmap pbm)))
   (let ((new-bitmap (map-vector (lambda (row) (map-vector identity row)) bitmap)))
    (for-each-n
     (lambda (y)
      (for-each-n
       (lambda (x)
	(when (and
	       (matrix-ref bitmap y x)
	       (<= 4
		   (+ (if (and (not (zero? y))
			       (not (zero? x))
			       (matrix-ref bitmap (- y 1) (- x 1)))
			  1
			  0)
		      (if (and (not (zero? y))
			       (matrix-ref bitmap (- y 1) x))
			  1
			  0)
		      (if (and (not (zero? y))
			       (not (= x (- width 1)))
			       (matrix-ref bitmap (- y 1) (+ x 1)))
			  1
			  0)
		      (if (and (not (zero? x))
			       (matrix-ref bitmap y (- x 1)))
			  1
			  0)
		      (if (and (not (= x (- width 1)))
			       (matrix-ref bitmap y (+ x 1)))
			  1
			  0)
		      (if (and (not (= y (- height 1)))
			       (not (zero? x))
			       (matrix-ref bitmap (+ y 1) (- x 1)))
			  1
			  0)
		      (if (and (not (= y (- height 1)))
			       (matrix-ref bitmap (+ y 1) x))
			  1
			  0)
		      (if (and (not (= y (- height 1)))
			       (not (= x (- width 1)))
			       (matrix-ref bitmap (+ y 1) (+ x 1)))
			  1
			  0))
		   7))
	 (matrix-set! new-bitmap y x #f)))
       width))
     height)
    (if (equal? new-bitmap bitmap)
	(make-pbm (pbm-raw? pbm) bitmap)
	(loop new-bitmap))))))

(define (pbm-flood pbm point)
 (let ((new-bitmap
	(map-vector (lambda (row) (map-vector identity row)) (pbm-bitmap pbm)))
       (height (pnm-height pbm))
       (width (pnm-width pbm)))
  (let loop ((point point))
   (when (and (<= 0 (y point) (- height 1)) (<= 0 (x point) (- width 1)))
    (unless (matrix-ref new-bitmap (y point) (x point))
     (matrix-set! new-bitmap (y point) (x point) #t)
     (loop (vector (- (x point) 1) (y point)))
     (loop (vector (+ (x point) 1) (y point)))
     (loop (vector (x point) (- (y point) 1)))
     (loop (vector (x point) (+ (y point) 1))))))
  (make-pbm (pbm-raw? pbm) new-bitmap)))

(define (pgm-canny image . args)
 (with-temporary-file
  "/tmp/canny.pgm"
  (lambda (file)
   (write-pnm image file)
   (system
    (list->string
     (remove-if (lambda (c) (or (equal? #\( c) (equal? #\) c)))
		(string->list (format #f "canny ~a ~a ~a" file file args)))))
   (read-pnm file))))

(define (pbm-distance-transform image)
 (with-temporary-file
  "/tmp/distance-transform.pbm"
  (lambda (file)
   (write-pnm image file)
   (system (format #f "distance-transform ~a ~a" file file))
   (read-object-from-file file))))

(define (draw-new-image size i)
 (system (format #f "convert -size ~ax~a xc:white ~a"
		 (first size) (second size) i)))

(define (read-image-as-pbm image)
 (with-temporary-file
  "/tmp/read-image.pbm"
  (lambda (file)
   (system (format #f "convert ~a ~a" image file))
   (read-pnm file))))

(define (interpolate-colour c1 c2 p)
 (k*v (/ 2) (v+ (k*v p c1) (k*v (- p 1) c1))))

(define (number-radix->padded-string-of-length number radix length)
 (when (negative? number) (fuck-up))
 (let ((string (number->string number radix)))
  (string-append (make-string (- length (string-length string)) #\0) string)))

(define (rgb->html c)
 (map-reduce
  string-append
  "#"
  (lambda (a) (number-radix->padded-string-of-length a 16 2))
  (vector->list c)))

(define (draw-interpolated-coloured-lines ls c1 c2 p i)
 (draw-coloured-lines ls (interpolate-colour c1 c2 p) i))

(define (draw-coloured-lines ls c i)
 (system
  (format #f "convert -fill white -stroke ~a ~a ~a ~a"
	  (string-append "'" (rgb->html c) "00" "'")
	  (apply
	   string-append
	   (map
	    (lambda (l)
	     (format #f " -draw ~a line ~a,~a,~a,~a ~a  "
		     #\"
		     (x (p l)) (y (p l)) (x (q l)) (y (q l))
		     #\"))
	    ls))
	  i
	  i)))

(define (draw-lines ls i)
 (system
  (format #f "convert -fill white -stroke black ~a ~a ~a"
	  (apply
	   string-append
	   (map
	    (lambda (l)
	     (format #f " -draw ~a line ~a,~a,~a,~a ~a  "
		     #\"
		     (x (p l)) (y (p l)) (x (q l)) (y (q l))
		     #\"))
	    ls))
	  i
	  i)))

(define (blend-image s1 s2 r)
 (system (format #f "composite -blend 50 ~a ~a -matte ~a" s1 s2 r)))

(define (find-histogram pixmap maxval)
 (let ((bin (make-vector (+ maxval 1) 0)))
  (for-each-vector
   (lambda (row)
    (for-each-vector
     (lambda (col)
      (vector-set! bin col (+ (vector-ref bin col) 1)))
     row))
   pixmap)
  bin))

(define (histogram-mean histogram i)
 (let ((n (reduce-vector + histogram 0)))
  (/ (reduce-vector
      + (map-indexed-vector (lambda (n j) (* n (+ i j))) histogram) 0) n)))

(define (histogram-variance histogram mu i)
 (let ((n (reduce-vector + histogram 0)))
  (/ (reduce-vector
      + (map-indexed-vector (lambda (n j) (* (sqr (- (+ i j) mu)) n)) histogram) 0)
     n)))

(define (normalised-histogram histogram val)
 (map-vector (lambda (v) (/ v val)) histogram))

(define (weighted-histogram histogram)
 (map-indexed-vector (lambda (n i) (* n (+ i 1))) histogram))

(define (cumulative-histogram histogram)
 (let ((h (map-vector (lambda (v) v) histogram)))
  (let loop ((i 1))
   (when (< i (vector-length h))
    (vector-set! h i (+ (vector-ref h (- i 1)) (vector-ref h i)))
    (loop (+ i 1))))
  h))

(define (find-between-class-variances omegas mus mu-total)
 (map-vector
  (lambda (omega mu)
   (if (or (= omega 0) (= omega 1)) 0 (/ (sqr (- (* mu-total omega) mu)) (* omega (- 1 omega)))))
  omegas
  mus))

(define (binary-threshold pgm threshold)
 (unless (pgm? pgm) (panic "Argument is not a PGM"))
 (make-pbm #f (map-vector
	       (lambda (row)
		(map-vector
		 (lambda (col) (> col threshold)) row))
	       (pgm-grey pgm))))

;; Optimal Threshold - Otsu's Method
(define (find-threshold-otsu pgm)
 (let* ((normalised-histogram
	 (map-vector (lambda (v) (/ v (* (pnm-width pgm) (pnm-height pgm))))
		     (find-histogram (pgm-grey pgm) (pgm-maxval pgm))))
	(first-cumulative-moments
	 (cumulative-histogram
	  (map-indexed-vector (lambda (n i) (* n (+ i 1))) normalised-histogram)))
	(between-class-sigmas
	 (find-between-class-variances
	  (cumulative-histogram normalised-histogram)
	  first-cumulative-moments
	  (vector-ref first-cumulative-moments
		      (- (vector-length first-cumulative-moments) 1)))))
  (vector-position between-class-sigmas
		   (reduce-vector max between-class-sigmas minus-infinity))))

(define (binary-threshold-optimal pgm)
 (unless (pgm? pgm) (panic "Argument is not a PGM"))
 (binary-threshold pgm (find-threshold-otsu pgm)))

;;; Optimal Threshold - Method of Successive Means
(define (find-threshold-means pgm)
 (let* ((histogram (find-histogram (pgm-grey pgm) (pgm-maxval pgm)))
	(l (vector-length histogram)))
  (let loop ((t (/ l 2)) (oldt 0))
   (if (<= (abs (- t oldt)) 1)
       t
       (loop (inexact->exact
	      (round (/ (+ (histogram-mean (subvector histogram 0 t) 0)
			   (histogram-mean (subvector histogram t l) t))
			2)))
	     t)))))

(define (binary-threshold-means pgm)
 (unless (pgm? pgm) (panic "Argument is not a PGM"))
 (binary-threshold pgm (find-threshold-means pgm)))

;;; Colour Threshold
(define (sample-image ppm colour-tx window-centre window-size)
 (let* ((halfwin (inexact->exact (/ window-size 2)))
	(tlx (- (x window-centre) halfwin))
	(tly (- (y window-centre) halfwin)))
  (map-vector
   (lambda (r g b) (colour-tx `#(,r ,g ,b)))
   (unshape-matrix (crop (ppm-red ppm) tlx tly window-size window-size))
   (unshape-matrix (crop (ppm-green ppm) tlx tly window-size window-size))
   (unshape-matrix (crop (ppm-blue ppm) tlx tly window-size window-size)))))

(define (binary-threshold-colour ppm colour-tx point threshold)
 (let* ((window-size 21)
	(colour-values (sample-image ppm colour-tx point window-size))
	(mu (vectors-mean colour-values))
	(isigma (invert-matrix (vectors-variance mu colour-values))))
  (make-pbm #f
	    (map-vector
	     (lambda (red-row green-row blue-row)
	      (map-vector
	       (lambda (r g b)
		(< (mahalanobis-distance (colour-tx `#(,r ,g ,b)) mu isigma)
		   threshold))
	       red-row
	       green-row
	       blue-row))
	     (ppm-red ppm)
	     (ppm-green ppm)
	     (ppm-blue ppm)))))

;;; Histogram Equalization
(define (histogram-equalise pgm)
 (let* ((w (pnm-width pgm))
	(h (pnm-height pgm))
	(cdf (cumulative-histogram
	      (find-histogram (pgm-grey pgm) (pgm-maxval pgm))))
	(min-cdf (reduce-vector min cdf infinity)))
  (make-pgm (pgm-raw? pgm)
	    (pgm-maxval pgm)
	    (map-vector
	     (lambda (row)
	      (map-vector
	       (lambda (val)
		(inexact->exact
		 (round (* (- (pgm-maxval pgm) 1)
			   (/ (- (vector-ref cdf val) min-cdf)
			      (- (* w h) 1))))))
	       row))
	     (pgm-grey pgm)))))

(define (colour-threshold ppm colour-tx mu isigma threshold)
 (make-pbm #f
	   (map-vector
	    (lambda (red-row green-row blue-row)
	     (map-vector
	      (lambda (r g b)
	       (< (mahalanobis-distance (colour-tx `#(,r ,g ,b)) mu isigma)
		  threshold))
	      red-row
	      green-row
	      blue-row))
	    (ppm-red ppm)
	    (ppm-green ppm)
	    (ppm-blue ppm))))

;;; Adaptive Thresholding
(define (make-integral-matrix matrix)
 (let ((integral-matrix
	(make-matrix (matrix-rows matrix) (matrix-columns matrix) 0)))
  (map-n
   (lambda (i)
    (map-n
     (lambda (j)
      (let ((current-val (matrix-ref matrix i j)))
       (matrix-set!
	integral-matrix i j
	(cond ((and (zero? i) (zero? j)) current-val)
	      ((zero? i) (+ current-val
			    (matrix-ref integral-matrix i (- j 1))))
	      ((zero? j) (+ current-val
			    (matrix-ref integral-matrix (- i 1) j)))
	      (else (+ current-val
		       (matrix-ref integral-matrix i (- j 1))
		       (matrix-ref integral-matrix (- i 1) j)
		       (- (matrix-ref integral-matrix (- i 1) (- j 1)))))))))
     (matrix-columns matrix)))
   (matrix-rows matrix))
  integral-matrix))

(define (compute-integral-matrix-mu integral-matrix x y w)
 (let ((r (- (matrix-rows integral-matrix) 1))
       (c (- (matrix-columns integral-matrix) 1))
       (del1 (inexact->exact (/ w 2)))
       (del2 (exact-ceiling (/ w 2))))
  (/ (- (+ (matrix-ref integral-matrix (min (+ x del1) r) (min (+ y del1) c))
	   (matrix-ref integral-matrix (max (- x del2) 0) (max (- y del2) 0)))
	(+ (matrix-ref integral-matrix (min (+ x del1) r) (max (- y del2) 0))
	   (matrix-ref integral-matrix (max (- x del2) 0) (min (+ y del1) c))))
     (* w w))))

(define (compute-integral-matrix-sigma squared-integral-matrix mu x y w)
 (sqrt (- (compute-integral-matrix-mu squared-integral-matrix x y w)
	  (* mu mu))))

(define (compute-adaptive-threshold
	 integral-matrix squared-integral-matrix x y w)
 (let ((k 0.2) (R 128)
       (mu (compute-integral-matrix-mu integral-matrix x y w)))
  (* mu (+ 1 (* k (- (/ (compute-integral-matrix-sigma
			squared-integral-matrix mu x y w) R)
		    1))))))

(define (adaptive-threshold pgm winsize)
 (let* ((pixmap (pgm-grey pgm))
	(integral-pixmap (make-integral-matrix pixmap))
	(squared-integral-pixmap
	 (make-integral-matrix (map-matrix sqr pixmap))))
  (make-pbm (pgm-raw? pgm)
	    (map-n-vector
	     (lambda (i)
	      (map-n-vector
	       (lambda (j)
		(> (matrix-ref pixmap i j)
		   (compute-adaptive-threshold
		    integral-pixmap squared-integral-pixmap i j winsize)))
	       (matrix-columns pixmap)))
	     (matrix-rows pixmap)))))

;; Image regions
(define (pnm->bounding-mask pnm space colour-mu colour-sigma threshold show?)
 (with-temporary-file
  "/tmp/bounding-mask.pgm"
  (lambda (bb-file)
   (write-pnm
    (pbm->pgm (colour-threshold pnm space colour-mu colour-sigma threshold))
    bb-file)
   (matlab-eval-strings
    (format #f "I=imread('~a');" bb-file)
    "I1 = imclose(I, strel('disk', 10));"
    "I2 = imclearborder(I1, 8);"
    "I3 = bwareaopen(I2, 5000);"
    "P = regionprops(bwlabel(I3),'Area','Extrema','Centroid');"
    "[v,maxi] = max([P.Area]);"
    "[I4,idx]=bwselect(I3,P(maxi).Extrema(1,1),P(maxi).Extrema(1,2));"
    "I5 = imclose(I4, strel('disk', 15));"
    "I6 = imclose(imdilate(I5,strel('disk',15)), strel('disk', 15));"
    "C = P(maxi).Centroid;"
    (format #f "imwrite(I6,'~a');" bb-file))
   (when show? (matlab-eval-strings "imshow(I6);"))
   `#(,(matlab-get-variable "C")
      ,(pgm->pbm (read-pnm bb-file) 1)))))

;;; Phase Congruency - with Matlab
;;;  (optimal parameters for the lincoln log assembly:
;;; Siddharth's parameters:
;;;   3, 6, 2, 2.1, 0.5, 1.2, 8, 0.5, 10
;;;   -nscale 3 -norient 6 -min-wave-length 2 -mult 2.1
;;;   -sigma-onf 0.5 -d-theta-on-sigma 1.2 -k 8 -cut-off 0.5 -g 10
;;; Andrei's parameters:
;;;   -norient 20 -k 40 -sigma-onf 0.45)
(define (phasecong-orientation l)
 (if (> (orientation l) 90)
     (- 360 (orientation l) 90)
     (- 180 (orientation l) 90)))

(define (find-edges-and-corners-phasecong-1 pgm location . params)
 ;; equivalent of:
 ;; i = imread('a.pgm');
 ;; [e, or, ft] = phasecongmono(i, 5, 3, 2.1, 0.55, 2, -1, 0.5, 10);
 ;; nm = nonmaxsup(e, or, 1.2);
 ;; eimg = hysthresh(nm, 0.3, 0.1);
 ;; imwrite(e,'xe.pgm');
 ;; imwrite(c,'xc.pgm');
 ;; imwrite(nm,'xnm.pgm');
 ;; imwrite(eimg,'xeimg.pgm');
 (matlab-append-to-path "~/imitate/matlab/phasecong/")
 (with-temporary-file
  "/tmp/phase.pgm"
  (lambda (t)
   (write-pnm pgm t)
   (matlab-eval-string (format #f "i = imread('~a');" t))
   (matlab-eval-string
    (format
     #f
     "[e, or, ft] = phasecongmono(i~a);"
     (if (null? params)
	 ""
	 (reduce string-append
		 (map (lambda (p) (format #f ", ~a " p)) params)
		 ""))))
   (matlab-eval-string (format #f "nm = nonmaxsup(e, or, 1.2);"))
   (matlab-eval-string (format #f "eimg = hysthresh(nm, 0.3, 0.1);"))
   (when #f
    ;; debugging
    (matlab-eval-string (format #f "imwrite(c,'/tmp/p-c.pgm');"))
    (matlab-eval-string (format #f "imwrite(e,'/tmp/p-e.pgm');"))
    (matlab-eval-string (format #f "imwrite(or,'/tmp/p-or.pgm');"))
    (matlab-eval-string (format #f "imwrite(nm,'/tmp/p-nm.pgm');"))
    (matlab-eval-string (format #f "imwrite(eimg,'/tmp/p-eigm.pgm');")))
   (matlab-save-variables location "e" "or" "nm"))))

(define (find-edges-and-corners-phasecong-2 pgm location . params)
 ;; equivalent of:
 ;; i = imread('a.pgm');
 ;; [e, c, or] = phasecong2(i, 3, 6, 2, 2.1, 0.5, 1.2, 8, 0.5, 10);
 ;; nm = nonmaxsup(e, or, 1.2);
 ;; eimg = hysthresh(nm, 0.3, 0.1);
 ;; imwrite(e,'xe.pgm');
 ;; imwrite(c,'xc.pgm');
 ;; imwrite(nm,'xnm.pgm');
 ;; imwrite(eimg,'xeimg.pgm');
 (matlab-append-to-path "~/imitate/matlab/phasecong/")
 (with-temporary-file
  "/tmp/phase.pgm"
  (lambda (t)
   (write-pnm pgm t)
   (matlab-eval-string (format #f "i = imread('~a');" t))
   (matlab-eval-string
    (format
     #f
     "[e, c, or, ft, pc, eo, en] = phasecong2(i~a);"
     (if (null? params)
	 ""
	 (reduce string-append
		 (map (lambda (p) (format #f ", ~a " p)) params)
		 ""))))
   (matlab-eval-string (format #f "nm = nonmaxsup(e, or, 1.2);"))
   (matlab-eval-string (format #f "eimg = hysthresh(nm, 0.3, 0.1);"))
   (when #f
    ;; debugging
    (matlab-eval-string (format #f "imwrite(c,'/tmp/p-c.pgm');"))
    (matlab-eval-string (format #f "imwrite(e,'/tmp/p-e.pgm');"))
    (matlab-eval-string (format #f "imwrite(or,'/tmp/p-or.pgm');"))
    (matlab-eval-string (format #f "imwrite(nm,'/tmp/p-nm.pgm');"))
    (matlab-eval-string (format #f "imwrite(eimg,'/tmp/p-eigm.pgm');")))
   (matlab-save-variables location "c" "e" "or" "nm"))))

(define (find-edges-and-corners-phasecong-3 pgm location . params)
 ;; equivalent of:
 ;; i = imread('a.pgm');
 ;; [e, c, or, ft] = phasecong3(i, 4, 6, 3, 2.1, 0.55, 2, 0.5, 10, -1);
 ;; nm = nonmaxsup(e, or, 1.2);
 ;; eimg = hysthresh(nm, 0.3, 0.1);
 ;; imwrite(e,'xe.pgm');
 ;; imwrite(c,'xc.pgm');
 ;; imwrite(nm,'xnm.pgm');
 ;; imwrite(eimg,'xeimg.pgm');
 (matlab-append-to-path "~/imitate/matlab/phasecong/")
 (with-temporary-file
  "/tmp/phase.pgm"
  (lambda (t)
   (write-pnm pgm t)
   (matlab-eval-string (format #f "i = imread('~a');" t))
   (matlab-eval-string
    (format
     #f
     "[e, c, or, ft] = phasecong3(i~a);"
     (if (null? params)
	 ""
	 (reduce string-append
		 (map (lambda (p) (format #f ", ~a " p)) params)
		 ""))))
   (matlab-eval-string (format #f "nm = nonmaxsup(e, or, 1.2);"))
   (matlab-eval-string (format #f "eimg = hysthresh(nm, 0.3, 0.1);"))
   (when #f
    ;; debugging
    (matlab-eval-string (format #f "imwrite(c,'/tmp/p-c.pgm');"))
    (matlab-eval-string (format #f "imwrite(e,'/tmp/p-e.pgm');"))
    (matlab-eval-string (format #f "imwrite(or,'/tmp/p-or.pgm');"))
    (matlab-eval-string (format #f "imwrite(nm,'/tmp/p-nm.pgm');"))
    (matlab-eval-string (format #f "imwrite(eimg,'/tmp/p-eigm.pgm');")))
   (matlab-save-variables location "e" "or" "ft" "nm"))))

(define (load-edges-and-corners-phasecong2 location)
 (matlab-load-variables location)
 `(,(matlab-get-variable "e")
   ,(matlab-get-variable "nm")
   ,(matlab-get-variable "or")))

(define (matlab-edges-phasecong2->pgm location)
 (make-pgm
  #t
  255
  (map-vector
   (lambda (a)
    (map-vector
     (lambda (a)
      (inexact->exact (round (* a 255))))
     a))
   (matlab-get-variable "e"))))

(define (find-edges-and-corners-phasecong pgm . params)
 (matlab-append-to-path "~/imitate/matlab/phasecong/")
 ;; scheme->matlab writes image to disk and uses imread from matlab
 ;; - because issues with size of string
 (scheme->matlab! "img" pgm)
 (matlab-eval-string
  (format #f "[e, c, or] = phasecong2(img~a);"
		  (reduce string-append (map (lambda (p) (format #f ", ~a " p)) params) "")))
 (when #f
  (matlab-eval-string (format #f "nm = nonmaxsup(e, or, 1.2);"))
  (matlab-eval-string (format #f "eimg = hysthresh(nm, 0.3, 0.1);")))
 `#(,(make-pgm (pgm-raw? pgm)
			   (pgm-maxval pgm)
			   (matlab->scheme (matlab-variable "e")))
    ,(make-pgm (pgm-raw? pgm)
			   (pgm-maxval pgm)
			   (matlab->scheme (matlab-variable "c")))))

(define (matlab-edge-orientation-hough
		 e o orientation draw? scaling threshold)
 (matlab-append-to-path "~/imitate/matlab/")
 (let ((e-file
		(format #f "/tmp/matlab_hough_e_~a_~a.dat"
				(let ((u (getenv "USERNAME"))) (if u u "USERNAME"))
				(getpid)))
       (o-file
		(format #f "/tmp/matlab_hough_o_~a_~a.dat"
				(let ((u (getenv "USERNAME"))) (if u u "USERNAME"))
				(getpid))))
  (matlab-matrix-output e e-file)
  (matlab-matrix-output o o-file)
  (matlab-eval-string (format #f "E = importdata('~a', ' ')~%;" e-file))
  (matlab-eval-string (format #f "O = importdata('~a', ' ')~%;" o-file))
  (matlab-eval-string
   (format #f "[p,q,h] = besthough(E,O,~a,~a,~a,~a);~%" orientation
		   (if draw? 1 0)))
  `#(,(matlab-get-variable "h")
     ,(matlab-get-variable "p")
     ,(matlab-get-variable "q"))))

(define (pgm->phase-congruency pgm)
 (with-matlab-default-engine
  (lambda (e)
   (find-edges-and-corners-phasecong e pgm))))

;; no wrapping, #F instead of the element at the edges
(define (slide-window m f size)
 (map-n-vector
  (lambda (x)
   (map-n-vector
    (lambda (y)
     (f (submatrix m x y size size)))
    (matrix-columns m)))
  (matrix-rows m)))

(define (count-pixels a)
 (reduce-vector
  +
  (map-vector
   (lambda (e)
    (reduce-vector (lambda (a b) (+ (if (and a (> a 0)) 1 0)
			       (if (and b (> b 0)) 1 0))) e 0)) a) 0))

(define (ppm->label-closest ppm mu1 sigma1 mu2 sigma2 colour-transform)
 (make-pbm
  #f
  (map-vector
   (lambda (red-row green-row blue-row)
    (map-vector
     (lambda (r g b)
      (< (mahalanobis-distance (colour-transform `#(,r ,g ,b)) mu1 sigma1)
	 (mahalanobis-distance (colour-transform `#(,r ,g ,b)) mu2 sigma2)))
     red-row
     green-row
     blue-row))
   (ppm-red ppm)
   (ppm-green ppm)
   (ppm-blue ppm))))

(define r x)
(define g y)
(define b z)

(define (set-ppm-pixel! ppm x y value)
 (matrix-set! (ppm-red ppm) y x (r value))
 (matrix-set! (ppm-green ppm) y x (g value))
 (matrix-set! (ppm-blue ppm) y x (b value))
 ppm)

(define (pnm-pixel? i x y)
 (and (>= x 0) (< x (pnm-width i))
      (>= y 0) (< y (pnm-height i))))

(define (map-ppm-values ppm f)
 (make-ppm
  (ppm-raw? ppm)
  (ppm-maxval ppm)
  (map-matrix (lambda (a) (inexact->exact (f a))) (ppm-red ppm))
  (map-matrix (lambda (a) (inexact->exact (f a))) (ppm-green ppm))
  (map-matrix (lambda (a) (inexact->exact (f a))) (ppm-blue ppm))))

(define (resize-image w h i)
 (let ((name (format #f "/tmp/resize-~a.ppm" (getpid))))
  (write-pnm i name)
  (system (format #f "mogrify -resize ~ax~a! ~a &> /dev/null" w h name))
  (let ((r (read-pnm name)))
   (rm name)
   r)))

(define (show-image i)
 ;; TODO should use a temporary file
 (cond ((ppm? i) (write-pnm i "/tmp/show.ppm")
	(system (format #f "feh /tmp/show.ppm")))
       ((pgm? i) (write-pnm i "/tmp/show.pgm")
	(system (format #f "feh /tmp/show.pgm")))
       ((pbm? i) (write-pnm (pbm->pgm i) "/tmp/show.pgm")
	(system (format #f "feh /tmp/show.pgm")))
       ;; should be imlib?
       ((imlib-image? i)
	(rm-if-necessary "/tmp/show.png")
	(imlib-context-set-image! i)
	(imlib-save-image "/tmp/show.png")
	(system (format #f "feh /tmp/show.png")))
       (else (system (format #f "feh ~a" i)))))

(define (ppm-burn base mask colour)
 (let ((image (pnm-copy base)))
  (for-each-indexed-vector
   (lambda (row j)
    (for-each-indexed-vector
     (lambda (val i)
      (if val (set-ppm-pixel! image i j colour)))
     row))
   (pbm-bitmap mask))
  image))

(define (show-ppm w h f)
 (let ((ppm (ppm-constant w h 0 0 0)))
  (f ppm)
  (show-image ppm)))

;;; (write-pnm
;;;  (draw-labels (read-pnm "/tmp/fall3-0054_slic.ppm")
;;; 	      (person->graph "fall3-0054")
;;;           '#(0 255 0))
;;;  "/tmp/bar.ppm")
(define (draw-labels pnm graph colour)
 ;; draws led style nums at centroid of (convex)superpixels
 (let ((ht (pnm-height pnm))
       (wd (pnm-width pnm))
       (m (make-matrix (pnm-height pnm) (pnm-width pnm) #f)))
  (for-each
   (lambda (idc) (matrix-set-mat! m (integer->matrix (first idc)) (second idc)))
   (map
    (lambda (v) `(,(superpixel-vertex-id v)
	     ,(map-vector
	       exact-round
	       (vectors-mean (list->vector (superpixel-vertex-region-pixels v))))))
    (graph-vertices graph)))
  (ppm-burn pnm (make-pbm #t m) colour)))

;;; Stacking

(define (pbm-stack-vertical pbm1 pbm2)
 (make-pbm (pbm-raw? pbm1)
	   (list->vector (append (vector->list (pbm-bitmap pbm1))
				 (vector->list (pbm-bitmap pbm2))))))

(define (pbm-stack-horizontal pbm1 pbm2)
 (make-pbm
  (pbm-raw? pbm1)
  (map-vector (lambda (row1 row2)
	       (list->vector (append (vector->list row1) (vector->list row2))))
	      (pbm-bitmap pbm1)
	      (pbm-bitmap pbm2))))

(define (ppm-stack-vertical ppm1 ppm2)
 (make-ppm (ppm-raw? ppm1)
	   (ppm-maxval ppm1)
	   (list->vector (append (vector->list (ppm-red ppm1))
				 (vector->list (ppm-red ppm2))))
	   (list->vector (append (vector->list (ppm-green ppm1))
				 (vector->list (ppm-green ppm2))))
	   (list->vector (append (vector->list (ppm-blue ppm1))
				 (vector->list (ppm-blue ppm2))))))

(define (ppm-stack-horizontal ppm1 ppm2)
 (make-ppm
  (ppm-raw? ppm1)
  (ppm-maxval ppm1)
  (map-vector (lambda (row1 row2)
	       (list->vector (append (vector->list row1) (vector->list row2))))
	      (ppm-red ppm1) (ppm-red ppm2))
  (map-vector (lambda (row1 row2)
	       (list->vector (append (vector->list row1) (vector->list row2))))
	      (ppm-green ppm1) (ppm-green ppm2))
  (map-vector (lambda (row1 row2)
	       (list->vector (append (vector->list row1) (vector->list row2))))
	      (ppm-blue ppm1) (ppm-blue ppm2))))

;;; Points

(define (pbm->points pbm)
 (map-reduce-n
  append
  '()
  (lambda (y)
   (removeq
    #f
    (map-n (lambda (x) (if (matrix-ref (pbm-bitmap pbm) y x) (vector x y) #f))
	   (pnm-width pbm))))
  (pnm-height pbm)))

(define (points->pbm points height width)
 (let ((m (make-matrix height width #f)))
  (for-each (lambda (point)
	     (let ((y (quantize-coordinate (y point)))
		   (x (quantize-coordinate (x point))))
	      (when (and (>= y 0) (>= x 0) (< y height) (< x width))
	       (matrix-set! m y x #t))))
	    points)
  (make-pbm #f m)))

;;; Rendering Line Segments

(define (midpoint l) (k*v 0.5 (v+ (p l) (q l))))

;;; needs work: screen coordinates
(define (orientation v) (atan (y v) (x v)))

(define (line-segment-orientation l) (orientation (v- (q l) (p l))))

(define (line-segment->points l)
 (let ((n (ceiling (line-segment-length l))) (v (v- (q l) (p l))))
  (if (zero? n)
      (list (p l))
      (map-n (lambda (i) (v+ (p l) (k*v (/ i n) v))) (+ n 1)))))

(define (line-segments->points ls)
 (map-reduce append '() line-segment->points ls))

;;; Contours

(define (points->line-segments ps)
 (map make-line-segment (but-last ps) (rest ps)))

(define (points->bounding-box-points points)
 (let* ((xs (map x points))
	(ys (map y points))
	(min-x (minimum xs))
	(max-x (maximum xs))
	(min-y (minimum ys))
	(max-y (maximum ys)))
  (line-segments->points
   (list (make-line-segment (vector min-x min-y) (vector max-x min-y))
	 (make-line-segment (vector min-x min-y) (vector min-x max-y))
	 (make-line-segment (vector max-x max-y) (vector max-x min-y))
	 (make-line-segment (vector max-x max-y) (vector min-x max-y))))))

(define (bb-size bb)
 (v- (vector (vector-ref bb 2) (vector-ref bb 3))
     (vector (vector-ref bb 0) (vector-ref bb 1))))

(define (points-bounding-box points)
 (vector (minimum (map x points)) (minimum (map y points))
	 (maximum (map x points)) (maximum (map y points))))

(define (points->points-bb points bb)
 (map (lambda (p) (vector (- (x p) (x bb)) (- (y p) (y bb)))) points))

(define (normalize-to-bounding-box ps)
 (let* ((bb (points-bounding-box ps))
	(bottom-left (vector (vector-ref bb 0) (vector-ref bb 1)))
	(top-right (vector (vector-ref bb 2) (vector-ref bb 3)))
	(size (v- top-right bottom-left)))
  (map (lambda (p) (let ((p (v- p bottom-left)))
	       (vector (/ (x p) (x size)) (/ (y p) (y size)))))
       ps)))

(define (normalize-to-other-bounding-box points ps)
 (let* ((bb (points-bounding-box ps))
	(bottom-left (vector (vector-ref bb 0) (vector-ref bb 1)))
	(top-right (vector (vector-ref bb 2) (vector-ref bb 3)))
	(size (v- top-right bottom-left)))
  (map (lambda (p) (let ((p (v- p bottom-left)))
	       (vector (/ (x p) (x size)) (/ (y p) (y size)))))
       points)))

(define (points->target-bb points target-bb)
 (let* ((size (bb-size target-bb)))
  (map (lambda (v) (v+ (map-vector * v size)
		  (vector (x target-bb) (y target-bb))))
       (normalize-to-bounding-box points))))

(define (points->other-target-bb points ps target-bb)
 (let* ((size (bb-size target-bb)))
  (map (lambda (v) (v+ (map-vector * v size)
		  (vector (x target-bb) (y target-bb))))
       (normalize-to-other-bounding-box points ps))))

(define (bb-bloat bb p)
 (let ((xl (vector-ref bb 0))
       (yl (vector-ref bb 1))
       (xu (vector-ref bb 2))
       (yu (vector-ref bb 3)))
  (vector (max 0 (quantize-coordinate (- xl (* p (- xu xl)))))
	  (max 0 (quantize-coordinate (- yl (* p (- yu yl)))))
	  (max 0 (quantize-coordinate (+ xu (* p (- xu xl)))))
	  (max 0 (quantize-coordinate (+ yu (* p (- yu yl))))))))

(define (bb-crop bb image)
 (let ((x1 (max 0 (vector-ref bb 0)))
       (x2 (max 0 (min (vector-ref bb 2) (pnm-width image))))
       (y1 (max 0 (vector-ref bb 1)))
       (y2 (max 0 (min (vector-ref bb 3) (pnm-height image)))))
  (crop-image image x1 y1 (- x2 x1) (- y2 y1))))

(define (bb->pb-pgm bb video frame)
 (with-temporary-file
  "bb-in.ppm"
  (lambda (file-in)
   (with-temporary-file
    "bb-out.ppm"
    (lambda (file-out)
     (write-pnm (bb-crop bb (read-pnm (ppm-pathname video frame))) file-in)
     (system (string-append (getenv "HOME") "/darpa-collaboration/bin/run-berkeley "
			    file-in " " file-out))
     (ppm->pgm (read-pnm file-out)))))))

(define (draw-limbs-bb ppm limbs bb r g b)
 (let ((rgb (map-vector quantize-coordinate (vector r g b))))
  (for-each (lambda (l) (for-each (lambda (p) (set-ppm-pixel! ppm (x p) (y p) rgb))
			     (map quantize-point (line-segment->points l))))
	    (limbs->lines-bb limbs bb))))

(define (show-limbs limbs r g b)
 (let* ((ppm (ppm-constant 500 500 0 0 0))
	(rgb (map-vector quantize-coordinate (vector r g b))))
  (for-each (lambda (l)
	     (for-each (lambda (p) (set-ppm-pixel! ppm (x p) (y p) rgb))
		       (map quantize-point (line-segment->points l))))
	    (limbs->lines limbs))
  (show-image ppm)))

;;; Rendering Ellipses

(define (line-segment->ellipse l)
 (let ((m (midpoint l)) (a (line-segment-length l)))
  ;; hardwired
  (make-ellipse (x m) (y m) (line-segment-orientation l) (* 0.5 a) (* 0.15 a))))

(define (ellipse->points e . n)
 (let* ((x0 (ellipse-x0 e))
	(y0 (ellipse-y0 e))
	(t0 (ellipse-t0 e))
	(a (ellipse-a e))
	(b (ellipse-b e))
	(rxx (cos t0))
	(rxy (- (sin t0)))
	(ryx (- rxy))
	(ryy rxx)
	(n (if (null? n)
	       360
	       (first n))))
  (map-n (lambda (i)
	  (let ((ellipse-x (* a (sin (degrees->radians (* i (/ 360.0 n))))))
		(ellipse-y (* b (cos (degrees->radians (* i (/ 360.0 n)))))))
	   (vector (+ (* rxx ellipse-x) (* rxy ellipse-y) x0)
		   (+ (* ryx ellipse-x) (* ryy ellipse-y) y0))))
	 n)))

;;; Subsample pbm

(define (subsample-pbm pbm)
 ;; hardwired to a factor of 2
 (make-pbm
  (pbm-raw? pbm)
  (map-n-vector
   (lambda (y)
    (map-n-vector
     (lambda (x)
      (or (matrix-ref (pbm-bitmap pbm) (* 2 y) (* 2 x))
	  (matrix-ref (pbm-bitmap pbm) (+ (* 2 y) 1) (* 2 x))
	  (matrix-ref (pbm-bitmap pbm) (* 2 y) (+ (* 2 x) 1))
	  (matrix-ref (pbm-bitmap pbm) (+ (* 2 y) 1) (+ (* 2 x) 1))))
     (quotient (pnm-width pbm) 2)))
   (quotient (pnm-height pbm) 2))))

;;; Rendering as avi
;;; This section hardwired to 4-digit frame indices

(define (pbm-files->avi-file
	 input-directory input-filename frame-rate output-pathname)
 (rm (default-extension output-pathname "avi"))
 (system (format #f "ffmpeg 2>/dev/null -r ~s -i ~a ~a"
		 frame-rate
		 (string-append input-directory "/%06d/" input-filename ".pbm")
		 (default-extension output-pathname "avi"))))

(define (pbms->avi-file pbms frame-rate pathname)
 (rm (default-extension pathname "avi"))
 (system "rm -f /tmp/frame[0-9][0-9][0-9][0-9].pgm")
 (for-each-indexed
  (lambda (pbm frame)
   (write-pnm
    (pbm->pgm pbm)
    (format #f "/tmp/frame~a.pgm" (number->padded-string-of-length frame 4))))
  pbms)
 (system (format #f "ffmpeg 2>/dev/null -r ~s -i /tmp/frame%06d.pgm ~a"
		 frame-rate
		 (default-extension pathname "avi")))
 (system "rm -f /tmp/frame[0-9][0-9][0-9][0-9].pgm"))

(define (pgm-files->avi-file
	 input-directory input-filename frame-rate output-pathname)
 (rm (default-extension output-pathname "avi"))
 (system (format #f "ffmpeg 2>/dev/null -r ~s -i ~a ~a"
		 frame-rate
		 (string-append input-directory "/%06d/" input-filename ".pgm")
		 (default-extension output-pathname "avi"))))

(define (pgms->avi-file pgms frame-rate pathname)
 (rm (default-extension pathname "avi"))
 (system "rm -f /tmp/frame[0-9][0-9][0-9][0-9].pgm")
 (for-each-indexed
  (lambda (pgm frame)
   (write-pnm
    pgm
    (format #f "/tmp/frame~a.pgm" (number->padded-string-of-length frame 4))))
  pgms)
 (system (format #f "ffmpeg 2>/dev/null -r ~s -i /tmp/frame%06d.pgm ~a"
		 frame-rate
		 (default-extension pathname "avi")))
 (system "rm -f /tmp/frame[0-9][0-9][0-9][0-9].pgm"))

(define (ppm-files->avi-file
	 input-directory input-filename frame-rate output-pathname)
 (rm (default-extension output-pathname "avi"))
 (system (format #f "ffmpeg 2>/dev/null -r ~s -i ~a ~a"
		 frame-rate
		 (string-append input-directory "/%06d/" input-filename ".ppm")
		 (default-extension output-pathname "avi"))))

(define (ppms->avi-file ppms frame-rate pathname)
 (rm (default-extension pathname "avi"))
 (system "rm -f /tmp/frame[0-9][0-9][0-9][0-9].ppm")
 (for-each-indexed
  (lambda (ppm frame)
   (write-pnm
    ppm
    (format #f "/tmp/frame~a.ppm" (number->padded-string-of-length frame 4))))
  ppms)
 (system (format #f "ffmpeg 2>/dev/null -r ~s -i /tmp/frame%06d.ppm ~a"
		 frame-rate
		 (default-extension pathname "avi")))
 (system "rm -f /tmp/frame[0-9][0-9][0-9][0-9].ppm"))

(define (ffmpeg fps frame destination)
 ;; If this fails it's because bin/install-ffmpeg-presets was not run
 (let ((cmd (format #f "ffmpeg -y -r ~a -i  ~a -vcodec libx264 -vb 3000000 -vpre default ~a ~a" fps frame destination
                    (if (not *quiet-mode?*)
                        "" ; Don't disable output
                        "-loglevel quiet 2> /dev/null"))))
  (system cmd)))

;;; Motion

(define (ppm-absolute-difference ppm1 ppm2)
 ;; maxval hardwired to 255
 (unless (and (ppm? ppm1)
	      (ppm? ppm2)
	      (= (ppm-maxval ppm1) 255)
	      (= (ppm-maxval ppm2) 255)
	      (eq? (ppm-raw? ppm1) (ppm-raw? ppm2))
	      (= (pnm-width ppm1) (pnm-width ppm2))
	      (= (pnm-height ppm1) (pnm-height ppm2)))
  (panic "Arguments to PPM-ABSOLUTE-DIFFERENCE are not matching PPMs"))
 (let ((scale
	(/ 255 (distance (vector 0.0 0.0 0.0) (vector 255.0 255.0 255.0)))))
  (make-pgm (ppm-raw? ppm1)
	    255
	    (map-vector
	     (lambda (red1 green1 blue1 red2 green2 blue2)
	      (map-vector (lambda (red1 green1 blue1 red2 green2 blue2)
			   (inexact->exact
			    (round
			     (* scale
				(distance (vector red1 green1 blue1)
					  (vector red2 green2 blue2))))))
			  red1 green1 blue1 red2 green2 blue2))
	     (ppm-red ppm1)
	     (ppm-green ppm1)
	     (ppm-blue ppm1)
	     (ppm-red ppm2)
	     (ppm-green ppm2)
	     (ppm-blue ppm2)))))

(define (scale-ppm ppm scale)
 (cond ((= scale 1) ppm)
       ((= scale 2)
	(let* ((rows (matrix-rows (ppm-red ppm)))
	       (columns (matrix-columns (ppm-red ppm)))
	       (r (make-matrix (* rows scale) (* columns scale) 0))
	       (g (make-matrix (* rows scale) (* columns scale) 0))
	       (b (make-matrix (* rows scale) (* columns scale) 0))
	       (ir (ppm-red ppm))
	       (ig (ppm-green ppm))
	       (ib (ppm-blue ppm)))
	 (define (fill-4! m i j v)
	  (matrix-set! m i j v)
	  (matrix-set! m i (+ j 1) v)
	  (matrix-set! m (+ i 1) j v)
	  (matrix-set! m (+ i 1) (+ j 1) v))
	 (for-each-n
	  (lambda (i)
	   (let ((i2 (* i 2)))
	    (for-each-n
	     (lambda (j)
	      (let ((j2 (* j 2)))
	       (fill-4! r i2 j2 (matrix-ref ir i j))
	       (fill-4! g i2 j2 (matrix-ref ig i j))
	       (fill-4! b i2 j2 (matrix-ref ib i j))))
	     columns)))
	  rows)
	 (make-ppm (ppm-raw? ppm) (ppm-maxval ppm) r g b)))
       (else (panic "Scale unsupported"))))

;;; Video annotations

(define (annotation-on-ppm annotation image)
 (annotate-image! annotation (map-ppm-values (pnm->ppm image) (lambda (v) v))))

(define (annotation->ppm annotation w h)
 (annotate-image! annotation (ppm-constant w h 0 0 0)))

(define (annotation->colour c)
 (case (annotation-label c)
  ((object1) '#(255 0 0))
  ((object2) '#(200 0 0))
  ((object3) '#(200 100 0))
  ((object4) '#(200 0 100))
  ((object5) '#(255 100 100))
  ((object6) '#(255 50 150))
  ((object7) '#(255 150 50))
  ((arm1) '#(0 0 255))
  ((arm2) '#(0 100 200))
  ((person1) '#(0 255 0))
  ((person2) '#(0 255 100))
  ((none) '#(255 255 255))
  (else (fuck-up))))

(define (label->colour c)
 (case c
  ((ball) '#(255 0 0))
  ((chair) '#(200 0 0))
  ((gun) '#(200 100 0))
  ((vehicle) '#(200 0 100))
  ((shovel) '#(255 100 100))
  ((object6) '#(255 50 150))
  ((object7) '#(255 150 50))
  ((person1) '#(0 255 0))
  ((p1arm1) '#(0 0 255))
  ((p1arm2) '#(0 0 200))
  ((p1leg1) '#(0 255 255))
  ((p1leg2) '#(0 200 255))
  ((person2) '#(0 255 100))
  ((p2arm1) '#(0 100 255))
  ((p2arm2) '#(0 100 200))
  ((p2leg1) '#(100 200 255))
  ((p2leg2) '#(150 200 255))
  ((none) '#(255 255 255))
  (else (fuck-up))))

(define (annotate-image! annotations image)
 (for-each
  (lambda (c)
   (let ((colour (annotation->colour c)))
    (for-each (lambda (p) (set-ppm-pixel! image (x p) (y p) colour))
	      (annotation-pixels c))))
  annotations)
 image)

(define (annotate-image-scaled! annotations image)
 (for-each
  (lambda (c)
   (for-each (lambda (p)
	      (let ((x (* (x p) 2)) (y (* (y p) 2)))
	       (set-ppm-pixel! image x y (annotation->colour c))
	       (set-ppm-pixel! image (+ x 1) y (annotation->colour c))
	       (set-ppm-pixel! image x (+ y 1) (annotation->colour c))
	       (set-ppm-pixel! image (+ x 1) (+ y 1) (annotation->colour c))))
	     (annotation-pixels c)))
  annotations)
 image)

(define (pnm->ppm pnm)
 (cond ((ppm? pnm) pnm)
       ((pgm? pnm) (pgm->ppm pnm))
       ((pbm? pnm) (pbm->ppm pnm))
       (else (fuck-up))))

(define (pnm->pgm pnm)
 (cond ((ppm? pnm) (ppm->pgm pnm))
       ((pgm? pnm) pnm)
       ((pbm? pnm) (pbm->pgm pnm))
       (else (fuck-up))))

(define (pixel-cell uniform-grid point)
 (matrix-ref uniform-grid
	     (inexact->exact (floor (/ (y point) (matrix-rows uniform-grid))))
	     (inexact->exact (floor (/ (x point) (matrix-columns uniform-grid))))))

(define (cell-coordinates uniform-grid point)
 `#(,(inexact->exact (floor (/ (x point) (matrix-columns uniform-grid))))
    ,(inexact->exact (floor (/ (y point) (matrix-rows uniform-grid))))))

(define (cell-ref uniform-grid coord)
 (if (or (< (x coord) 0) (>= (x coord) (matrix-columns uniform-grid))
	 (< (y coord) 0) (>= (y coord) (matrix-rows uniform-grid)))
     (make-cell '())
     (matrix-ref uniform-grid (y coord) (x coord))))

(define (annotations->uniform-grid annotations w h cell-size)
 (let ((matrix (map-matrix (lambda (_) (make-cell '()))
			   (make-matrix (ceiling (/ h cell-size))
					(ceiling (/ w cell-size))
					#f))))
  (for-each (lambda (a)
	     (for-each (lambda (p)
			(let* ((cell (pixel-cell matrix p))
			       (l (cell-list cell)))
			 (when (or (null? l) (not (eq? (car l) a)))
			  (set-cell-list! cell (cons a l)))))
		       (annotation-pixels a)))
	    annotations)
  matrix))

(define (lookup-9-closest-cells grid p)
 (let ((c (cell-coordinates grid p)))
  (map-reduce append
	      '()
	      (lambda (c) (cell-list c))
	      (list (cell-ref grid c)
		    (cell-ref grid (v+ c '#( 1  0)))
		    (cell-ref grid (v+ c '#( 0  1)))
		    (cell-ref grid (v+ c '#( 1  1)))
		    (cell-ref grid (v+ c '#( 1 -1)))
		    (cell-ref grid (v+ c '#(-1  0)))
		    (cell-ref grid (v+ c '#(0  -1)))
		    (cell-ref grid (v+ c '#(-1  1)))
		    (cell-ref grid (v+ c '#(-1 -1)))))))

(define (annotation-point-distance annotation point)
 (minimum (map (lambda (p) (distance point p)) (annotation-pixels annotation))))

(define (closest-annotation annotations point)
 (minimump annotations (lambda (c) (annotation-point-distance c point))))

(define (frame-absolute-difference v frame)
 (pgm-absolute-difference
  (read-pnm (pgm-pathname v frame))
  (read-pnm (pgm-pathname v (+ frame 1)))))

;;; Essa

(define (submit-essa email video)
 (let* ((id (uuid))
	(c1 (curl "Mozilla Firefox/3.0 (Linux)"
		  `(("email" ,email)
		    ("optical_flow" "1")
		    ("file" ,(string-append "@" video))
		    ("submit" "submit")
		    ("progress_key" ,id)
		    ("APC_UPLOAD_PROGRESS" ,id))
		  "http://neumann.cc.gt.atl.ga.us/segmentation/upload_file.php"))
	(c2 (curl "Mozilla Firefox/3.0 (Linux)"
		  `(("submit" "submit")
		    ("session_id" ,id))
		  "http://neumann.cc.gt.atl.ga.us/segmentation/segment_file.php")))
  (list id c1 c2)))

;; (submit-essa "andrei@0xab.com" "/home/andrei/video-datasets/C-D1a/SINGLE_VERB/Approach2_A1_C1_Act1_2_Park3_MC_AFTN_47ff4ac2-c5af-11df-a99b-e80688cb869a.mov")

(define (essa-results name id)
 (let ((f1 (format #f "http://neumann.cc.gt.atl.ga.us/seg_results/~a/~a_resized_h264_out1.mp4"
		   id (strip-extension (strip-directory name))))
       (f2 (format #f "http://neumann.cc.gt.atl.ga.us/seg_results/~a/~a_resized_h264_out2.mp4"
		   id (strip-extension (strip-directory name))))
       (f3 (format #f "http://neumann.cc.gt.atl.ga.us/seg_results/~a/~a_resized_h264_out3.mp4"
		   id (strip-extension (strip-directory name))))
       (p  (format #f "http://neumann.cc.gt.atl.ga.us/seg_results/~a/~a_resized_segment.pb.zip"
		   id (strip-extension (strip-directory name)))))
  (system (format #f "wget ~a" f1))
  (system (format #f "wget ~a" f2))
  (system (format #f "wget ~a" f3))
  (system (format #f "wget ~a" p))
  (list (format #f "~a_resized_h264_out1.mp4" (strip-extension (strip-directory name)))
	(format #f "~a_resized_h264_out2.mp4" (strip-extension (strip-directory name)))
	(format #f "~a_resized_h264_out3.mp4" (strip-extension (strip-directory name)))
	(format #f "~a_resized_segment.pb.zip" (strip-extension (strip-directory name))))))

(define (r-scanline r) (R-Ss r))

(define (essa-width e) (D-width (F-Ds e)))
(define (essa-height e) (D-height (F-Ds e)))

(define (essa-hierarchies f) (D-Hs (F-Ds f)))
(define (essa-hierarchies-levels f) (map-vector H-level (D-Hs (F-Ds f))))
(define (essa-hierarchy f level)
 (find-if (lambda (a) (= (H-level a) level)) (vector->list (D-Hs (F-Ds f)))))

(define (region-parent r)
 (cond ((cr? r) (cr-parent r))
       ((r? r) (r-parent r))
       (else (fuck-up))))

(define (essa-region-id r f l)
 (cond ((= 0 l) (r-id r))
       ((= 1 l) (region-parent r))
       (else
 	(let loop ((r r) (i 1))
 	 (if (< i l)
 	     (loop (vector-ref (H-CRs (vector-ref (essa-hierarchies f) (- i 1)))
 			       (region-parent r))
 		   (+ i 1))
 	     (region-parent r))))))

(define (scalines->pixels top-y ss)
 (join
  (vector->list
   (map-indexed-vector
    (lambda (s i)
     (join
      (vector->list
       (map-vector
	(lambda (s)
	 (map-m-n
	  (lambda (x) (vector x (+ i top-y)))
	  (I-left s)
	  (I-right s)))
	s))))
    ss))))

(define (essa-frame-hierarchy->slice f level)
 (map-vector
  (lambda (r) (vector (essa-region-id r f level) r))
  (D-Rs (F-Ds f))))

(define (essa->regions f level)
 (map-vector (lambda (v)
	      (vector
	       (x v)
	       (scalines->pixels (r-top-y (y v))
				 (r-scanline (y v)))))
	     (essa-frame-hierarchy->slice f level)))

(define (essa->region-pixels f level)
 (map
  (lambda (c) (list (first (first c)) (foldl append (map second c) '())))
  (equivalence-classesp
   (lambda (a b) (= (first a) (first b)))
   (vector->list
    (map-vector (lambda (v)
		 (list
		  (x v)
		  (scalines->pixels (r-top-y (y v))
				    (r-scanline (y v)))))
		(essa-frame-hierarchy->slice f level))))))

(define (essa->region-outline f level)
 (map (lambda (r) (list (first r) (region->boundary (second r))))
      (essa->region-pixels f level)))

(define (essa-blit-region! matrix id region)
 (for-each-indexed-vector
  (lambda (ss i)
   (for-each-vector
    (lambda (l)
     (for-each-m-n
      (lambda (x)
       (matrix-set! matrix (+ i (r-top-y region)) x id))
      (I-left l) (I-right l)))
    ss))
  (R-Ss region)))

(define (essa-blit-region-pixels! matrix id region)
 (let ((p '()))
  (for-each-indexed-vector
   (lambda (ss i)
    (for-each-vector
     (lambda (l)
      (for-each-m-n
       (lambda (x)
	(set! p (cons (vector x (+ i (r-top-y region))) p))
	(matrix-set! matrix (+ i (r-top-y region)) x id))
       (I-left l) (I-right l)))
     ss))
   (R-Ss region))
  p))

(define (essa-rasterize-regions! matrix regions)
 (for-each-vector (lambda (r) (essa-blit-region! matrix (x r) (y r))) regions)
 matrix)

(define (essa-rasterize-slice f level)
 (let ((slice (essa-frame-hierarchy->slice f level)))
  (vector
   (x (maximump (vector->list slice) x))
   (essa-rasterize-regions! (make-matrix (D-height (f-Ds f)) (D-width (f-Ds f)) #f)
			    slice))))

(define (essa-rasterized->pgm f level)
 (let ((r (essa-rasterize-slice f level)))
  (make-pgm
   #t
   255
   (map-matrix (lambda (e)
		(inexact->exact (* (/ e (x r)) 255))) (y r)))))

;;; Create region matrix for fast lookup
(define (essa->region-map f level)
 (let* ((region-map (make-matrix (essa-height f) (essa-width f) 0)))
  (for-each
   (lambda (r)
    (for-each
     (lambda (p)
      (matrix-set! region-map  (vector-ref p 1) (vector-ref p 0) (first r))
      ) (second r)))
   (essa->region-pixels f level))
  region-map))

(define (annotation-region->annotation-label annotations region level)
 (cond
  ((null? annotations) #f)
  ((and
    (= (essa-annotation-region (car annotations)) region )
    ;; could probably get rid of this check (and argument
    (= (essa-annotation-level (car annotations)) level))
   (essa-annotation-label (car annotations)))
  (else (annotation-region->annotation-label (cdr annotations) region level))))

;;; Turbopixels

(define (turbopixels-load video-name)
 (matlab-eval-string (format #f "load('~a')" (turbopixel-mat-pathname video-name))))

(define (turbopixels-for-frame f)
 (matlab (format #f "superpixels = video.superpixels(:,:,~a);" f))
 (map-matrix inexact->exact (matlab-get-variable "superpixels")))

(define (turobpixels-maximum-label t)
 (inexact->exact (maximum (map (lambda (e) (maximum (vector->list e))) (vector->list t)))))

(define (turbopixels->regions t)
 (let ((regions (make-vector (+ (turobpixels-maximum-label t) 1))))
  (for-each-indexed-vector
   (lambda (v y)
    (for-each-indexed-vector
     (lambda (e x)
      (vector-set! regions e (cons `#(,x ,y) (vector-ref regions e))))
     v))
   t)
  (remove-if (lambda (e) (null? (y e)))
	     (cdr (vector->list (map-indexed-vector (lambda (e i) `#(,i ,e)) regions))))))

(define (moving-turbopixels turbopixels motion thresh)
 (remove-if-not (lambda (t)
		 (> (list-mean (map (lambda (p) (image-ref motion p)) (y t)))
		    thresh))
		turbopixels))

(define (turbopixel-regions->pbm r w h)
 (points->pbm (map-concat y r) h w))

(define (video-first-frame video)
 ;; Since we can't depend on per-frame directories anymore there is no
 ;; first frame detection, it's hardcoded to 1
 1)

(define (turbopixels-motion thresh frame)
 (matlab
  (format #f "M=double(video.Vx(:,:,~a)>~a | video.Vx(:,:,~a)<-~a | video.Vy(:,:,~a)>~a | video.Vy(:,:,~a)<-~a);"
	  frame thresh frame thresh frame thresh frame thresh))
 (matlab-get-variable "M"))

(define (turbopixels-names)
 (matlab "M=video.sup_idx_range;")
 (map-vector (lambda (a) (vector (inexact->exact (x a))
				 (inexact->exact (y a))))
	     (matlab-get-variable "M")))

(define (turbopixels-area)
 (matlab "M=video.sup_area;")
 (map-vector x (matlab-get-variable "M")))

(define (turbopixels-speed)
 (matlab "M=video.supSpeed;")
 (matlab-get-variable "M"))

(define (turbopixels-centers)
 (matlab "M=video.sup_centers;")
 (matlab-get-variable "M"))

(define (turbopixels-speed-reliable?)
 (matlab "M=double(video.supSpeedReliab);")
 (map-vector (lambda (a) (= (x a) 1)) (matlab-get-variable "M")))

(define (turbopixel-index t-names frame index)
 ;; -2 since both index and the name correction are off by 1 due to
 ;; the first index in matlab being 1 not 0
 (+ (x (vector-ref t-names (- frame 1))) -2 index))

(define (turbopixel-random-color t)
 (srand (turbopixel-track t))
 `#(,(random-integer 255)
    ,(random-integer 255)
    ,(random-integer 255)))

(define (turbopixel-correspondence t f2)
 (let ((c (image-ref
	   f2
	   (quantize-point (if (turbopixel-speed-reliable? t)
			       (v+ (turbopixel-center t) (turbopixel-speed t))
			       (turbopixel-center t))))))
  (if (= c 0)
      (image-ref f2 (quantize-point (turbopixel-center t)))
      c)))

(define (update-turbopixel-tracks! new-turbopixels new-frame old-turbopixels)
 (for-each (lambda (lt)
	    (let* ((c (turbopixel-correspondence lt new-frame))
		   (t (find-if (lambda (t) (= (turbopixel-name t) c)) new-turbopixels)))
	     (unless (= c 0)
	      (set-turbopixel-track! t (turbopixel-track lt)))))
	   old-turbopixels)
 #f)

(define (update-turbopixel-next! new-turbopixels new-frame old-turbopixels)
 (for-each (lambda (lt)
	    (let* ((c (turbopixel-correspondence lt new-frame))
		   (t (find-if (lambda (t) (= (turbopixel-name t) c)) new-turbopixels)))
	     (unless (= c 0)
	      (set-turbopixel-track! t (turbopixel-global-name lt)))))
	   old-turbopixels)
 #f)

(define (turbopixels->pbm ts w h)
 (points->pbm (map-concat turbopixel-pixels ts) h w))

(define (external-turbopixels->turbopixels t frame t-names t-centers t-speed t-speed-reliable? )
 (let ((turbopixels (make-vector (+ (turobpixels-maximum-label t) 1))))
  (for-each-indexed-vector
   (lambda (v y)
    (for-each-indexed-vector
     (lambda (e x)
      (vector-set! turbopixels e (cons `#(,x ,y) (vector-ref turbopixels e))))
     v))
   t)
  (remove-if (lambda (e) (null? (y e)))
	     (cdr (vector->list (map-indexed-vector
				 (lambda (e i)
				  (unless (= i 0)
				   (let ((g-i (turbopixel-index t-names frame i)))
				    (make-turbopixel
				     i
				     g-i
				     #f
				     (v+ (vector-ref t-centers g-i) '#(-1 -1))
				     (vector-ref t-speed g-i)
				     (vector-ref t-speed-reliable? g-i)
				     e))))
				 turbopixels))))))

;;; SLIC

(define (read-slic-file filename w h)
 (call-with-input-file filename
  (lambda (port)
   (map-matrix (lambda (a)
		(if (eof-object? port)
		    (fuck-up)
		    (read-32-bit-integer port)))
	       (make-matrix h w 0)))))

(define (slic-frame->regions t)
 (map (lambda (a) (make-slic (x a) #f '#(0 0) (y a))) (frame->regions t)))

(define (run-slic video-name frame file nr-superpixels spatial-proxmity-weight)
 (system (format #f "cd ~a;slic ~a ~a ~a"
		 (frame-pathname video-name frame) file
		 nr-superpixels spatial-proxmity-weight))
 (system (format #f "cd ~a; mv ~a ~a"
		 (frame-pathname video-name frame)
		 (replace-whole-extension file ".dat")
		 (slic-dat-pathname video-name frame file)))
 (system (format #f "cd ~a; mv ~a ~a"
		 (frame-pathname video-name frame)
		 (replace-whole-extension file "_slic.png")
		 (replace-whole-extension file "-slic.png")))
 #f)

;;; Superpixels

(define (frame->regions t)
 (let ((regions (make-vector (+ (maximum-matrix t) 1))))
  (for-each-indexed-vector
   (lambda (v y)
    (for-each-indexed-vector
     (lambda (e x)
      (vector-set! regions e (cons `#(,x ,y) (vector-ref regions e))))
     v))
   t)
  (remove-if (lambda (e) (null? (y e)))
	     (vector->list (map-indexed-vector (lambda (e i) `#(,i ,e)) regions)))))

(define (frame->regions-neighbours-lr t)
 (let ((regions (make-vector (+ (maximum-matrix t) 1)))
       (neighbours (make-vector (+ (maximum-matrix t) 1))))
  (for-each-indexed-vector
   (lambda (v y)
    (for-each-indexed-vector
     (lambda (e x)
      (let ((under (matrix-ref t (min (+ y 1) (- (matrix-rows t) 1)) x))
	    (right (matrix-ref t y (min (+ x 1) (- (matrix-columns t) 1)))))
       (vector-set! regions e (cons `#(,x ,y) (vector-ref regions e)))
       (when (not (or (find under (vector-ref neighbours e)) (equal? e under)))
	(vector-set! neighbours e (cons under (vector-ref neighbours e))))
       (when (not (or (find right (vector-ref neighbours e)) (equal? e right)))
	(vector-set! neighbours e (cons right (vector-ref neighbours e))))))
     v))
   t)
  (map-vector
   vector
   (list->vector (enumerate (+ (maximum-matrix t) 1))) regions neighbours)))

(define (frame->regions-neighbours t)
 (let ((regions (frame->regions-neighbours-lr t))
       (neighbour-pairs '()))
  (for-each-vector (lambda (r)
		    (for-each
		     (lambda (pair) (unless (find (reverse pair) neighbour-pairs)
				     (set! neighbour-pairs (cons pair neighbour-pairs))))
		     (map (lambda (e) `(,(x r) ,e)) (z r)))) regions)
  (list regions neighbour-pairs)))

(define (optical-flow-region region flow)
 (map (lambda (p) (image-ref flow p)) (y region)))

(define (average-optical-flow-region region flow)
 (list-mean (map (lambda (p) (image-ref flow p)) (y region))))

(define (optical-flow-region-raw-corresponence region-prev frame-map-next flow)
 (map
  (lambda (r) (let ((f (average-optical-flow-region r flow)))
	       (list r (image-ref frame-map-next f) f)))
  region-prev))

(define (optical-flow-region-corresponence region-prev frame-map-next flow)
 (map
  (lambda (c) (maximump c third))
  (transitive-equivalence-classesp
   (lambda (a b) (equal? (second a) (second b)))
   (map
    (lambda (r) (let ((f (average-optical-flow-region r flow)))
		 (list r (image-ref frame-map-next f) f)))
    region-prev))))

(define (turbopixel->superpixel t)
 (make-superpixel (turbopixel-global-name t)
		  (if (turbopixel-track t) (list (turbopixel-track t)) #f)
		  #f
		  #f
		  (if (turbopixel-speed-reliable? t) (turbopixel-speed t) #f)
		  (turbopixel-pixels t)))

(define (slic->superpixel s)
 (make-superpixel (slic-name s)
		  (if (slic-track s) (list (slic-track s)) '())
		  #f
		  '()
		  (slic-flow s)
		  (slic-pixels s)))

(define (essa->superpixel e level)
 (cond ((R? e) (make-superpixel `#(,level ,(R-id e))
				`(#(,level ,(R-id e)))
				`#(,(+ level 1) ,(R-parent e))
				'()
				'#(0 0)
				(scalines->pixels (R-top-y e) (R-Ss e))))
       ((CR? e)
	(make-superpixel `#(,level ,(CR-id e))
			 `(#(,level ,(CR-id e)))
			 `#(,(+ level 1) ,(CR-parent e))
			 (vector->list
			  (map-vector (lambda (c) `#(,(- level 1) ,c)) (CR-children e)))
			 '#(0 0)
			 '()))
       (else (fuck-up))))

(define (essa->superpixel-none e)
 (cond ((R? e) (make-superpixel (R-id e)
				`(,(R-id e))
				#f
				'()
				'#(0 0)
				(scalines->pixels (R-top-y e) (R-Ss e))))
       (else (fuck-up))))

(define (rasterize ps)
 (map
  (lambda (e) (sort e < x))
  (sort
   (transitive-equivalence-classesp (lambda (a b) (equal? (y a) (y b))) ps)
   <
   (lambda (e) (y (first e))))))

(define (fill-raster-region-gaps r)
 (let ((ht (y (first r))) (end (x (last r))) (l (length r)))
  (if (even? l)
      (if (= l 2)
	  (let loop ((i (x (first r))) (ps '()))
	   (if (> i end) (reverse ps) (loop (+ i 1) (cons `#(,i ,ht) ps))))
	  (map-reduce
	   append
	   '()
	   fill-raster-region-gaps
	   (map (lambda (i) (list (list-ref r i) (list-ref r (+ i 1))))
		(every-other (enumerate (length r))))))
      (let ((l/2 (exact-floor (/ l 2))))
       (if (even? l/2)
	   (cons (list-ref r l/2)
		 (fill-raster-region-gaps (list-remove r l/2)))
	   (append
	    (fill-raster-region-gaps (sublist r 0 (exact-ceiling (/ l 2))))
	    (rest (fill-raster-region-gaps (sublist r l/2 l)))))))))

(define (boundary->region b)
 (map-reduce append '() fill-raster-region-gaps (rasterize b)))

(define (region->boundary r)
 (let* ((m (make-matrix (+ (maximum (map y r)) 1)
			(+ (maximum (map x r)) 1)
			#f)))
  (for-each (lambda (p) (matrix-set! m (y p) (x p) #t)) r)
  (define (safe-matrix-ref m y x)
   (if (or (>= x (matrix-columns m)) (< x 0)
	   (>= y (matrix-rows m)) (< y 0))
       #f
       (matrix-ref m y x)))
  (remove-if
   (lambda (p)
    (and (safe-matrix-ref m (- (y p) 1) (x p))
	 (safe-matrix-ref m (+ (y p) 1) (x p))
	 (safe-matrix-ref m (y p) (- (x p) 1))
	 (safe-matrix-ref m (y p) (+ (x p) 1))))
   r)))

(define (connected-boundary ps1 ps2)
 (let ((result '()))
  (for-each
   (lambda (p1)
    (for-each
     (lambda (p2) (when (four-connected? p1 p2)
		   (set! result (cons `(,p1 ,p2) result))))
     ps2))
   ps1)
  result))

(define (edge-weight-edge u v edge-map)
 ;; assigns edge weight based on an edge-map
 (let* ((alpha 1)
	(u-boundary (superpixel-vertex-boundary-pixels u))
	(v-boundary (superpixel-vertex-boundary-pixels v))
	(maxval (maximum-matrix (pgm-grey edge-map)))
	(connected-boundary (connected-boundary u-boundary v-boundary)))
  (+ (* alpha (/ (map-reduce +
			     0
			     (lambda (p) (+ (image-ref edge-map (first p))
					    (image-ref edge-map (second p))))
			     (zip (map first connected-boundary)
				  (map second connected-boundary)))
		 (* maxval (length connected-boundary))))
     (* (- 1 alpha) (/ (length connected-boundary)
		       (+ (length u-boundary) (length v-boundary)))))))

(define (edge-weight-image-pixel u v image-map)
 ;; assigns edge weight from adjacent pixel intensity difference
 (let* ((alpha 1)
	(u-boundary (superpixel-vertex-boundary-pixels u))
	(v-boundary (superpixel-vertex-boundary-pixels v))
	(maxval (maximum-matrix (pgm-grey image-map)))
	(connected-boundary (connected-boundary u-boundary v-boundary)))
  (+ (* alpha (/ (map-reduce +
			     0
			     (lambda (c) (abs (- (image-ref image-map (first c))
						 (image-ref image-map (second c)))))
			     (zip (map first connected-boundary)
				  (map second connected-boundary)))
		 (* maxval (length connected-boundary))))
     (* (- 1 alpha) (/ (length connected-boundary)
		       (+ (length u-boundary) (length v-boundary)))))))

(define (edge-weight-image-region u v image-map)
 ;; assigns edge weight based on superpixel-region colour/intensity
 (cond
  ((pgm? image-map)
   (/ (abs (- (list-mean (map (lambda (p) (image-ref image-map p))
			      (superpixel-vertex-region-pixels u)))
	      (list-mean (map (lambda (p) (image-ref image-map p))
			      (superpixel-vertex-region-pixels v)))))
      (maximum-matrix (pgm-grey image-map))))
  ((ppm? image-map)
   (let* ((u-region (list->vector
		     (map (lambda (p) (rgb->hsv (image-ref image-map p)))
			  (superpixel-vertex-region-pixels u))))
	  (v-region (list->vector
		     (map (lambda (p) (rgb->hsv (image-ref image-map p)))
			  (superpixel-vertex-region-pixels v))))
	  (u-mu (vectors-mean u-region))
	  (v-mu (vectors-mean v-region))
	  (u-isigma (invert-matrix (vectors-variance u-mu u-region)))
	  (v-isigma (invert-matrix (vectors-variance v-mu v-region))))
    (cond ((and u-isigma v-isigma)
	   (* 0.5
	      (+ (mahalanobis-distance u-mu v-mu v-isigma)
		 (mahalanobis-distance v-mu u-mu u-isigma))))
	  (u-isigma
	   (* 0.5 (+ (distance u-mu v-mu)
		     (mahalanobis-distance v-mu u-mu u-isigma))))
	  (v-isigma
	   (* 0.5 (+ (mahalanobis-distance u-mu v-mu v-isigma)
		     (distance v-mu u-mu))))
	  (else (* 0.5 (+ (distance u-mu v-mu) (distance v-mu u-mu)))))))
  (else (fuck-up))))

(define (perimeter-superpixel? boundary-pixels height width)
 (some (lambda (p) (or (= (x p) 0) (= (x p) (- width 1))
		       (= (y p) 0) (= (y p) (- height 1))))
       boundary-pixels))

(define (weight-normalised-graph g)
 (let ((weights (map superpixel-edge-weight (graph-edges g))))
  (if (every (lambda (w) (<= w 1)) weights)
      g
      (make-graph
       (graph-vertices g)
       (map (lambda (e) (make-superpixel-edge
			 (superpixel-edge-u e)
			 (superpixel-edge-v e)
			 (/ (superpixel-edge-weight e) (maximum weights))))
	    (graph-edges g))))))

(define (superpixel-data->graph data weight-fn weight-map)
 (let*
   ((regions (frame->regions-neighbours-lr data))
    (neighbour-pairs '())
    (vertices
     (map-vector
      (lambda (r)
       (let ((boundary-pixels (region->boundary (y r))))
	(for-each
	 (lambda (pair) (unless (find (reverse pair) neighbour-pairs)
			 (set! neighbour-pairs (cons pair neighbour-pairs))))
	 (map (lambda (e) `(,(x r) ,e)) (z r)))
	(make-superpixel-vertex
	 (x r)
	 boundary-pixels
	 (y r)
	 (remove
	  (x r)
	  (reduce append
		  (remove-if-not
		   (lambda (p) (or (equal? (x r) (first p))
				   (equal? (x r) (second p))))
		   neighbour-pairs)
		  '()))
	 (perimeter-superpixel? boundary-pixels
				(pnm-height weight-map)
				(pnm-width weight-map))
	 0.5)))
      regions)))
  (weight-normalised-graph
   (make-graph (vector->list vertices)
	       (map (lambda (p) (let ((u (vector-ref vertices (first p)))
				      (v (vector-ref vertices (second p))))
				 (make-superpixel-edge u v (weight-fn u v weight-map))))
		    neighbour-pairs)))))

(define (superpixels->map ss w h)
 (let ((m (make-matrix h w 0)))
  (for-each (lambda (s)
	     (map (lambda (p) (matrix-set! m (y p) (x p) (superpixel-name s)))
		  (superpixel-pixels s)))
	    ss)
  m))

(define (split-superpixel-regions! s h w)
 (let ((regions (map-indexed
		 (lambda (ps i)
		  (make-superpixel `(,i ,(superpixel-name s))
				   '()
				   (superpixel-name s)
				   '()
				   #f
				   (pbm->points (graph->pbm ps h w))))
		 (connected-components (pbm->graph (points->pbm (superpixel-pixels s) h w) 2)))))
  (set-superpixel-children! s (map superpixel-name regions))
  regions))

(define (update-superpixel-split-tracking! prev next)
 (for-each
  (lambda (p)
   (let ((ss (remove-if-not (lambda (n) (equal? (second (superpixel-name n))
						(second (superpixel-name p))))
			    next)))
    (when ss
     (set-superpixel-next!
      p
      (list
       (minimump
	ss
	(lambda (s) (distance (superpixel-center p) (superpixel-center s)))))))))
  prev)
 prev)

(define (superpixel-center s)
 `#(,(list-mean (map x (superpixel-pixels s)))
    ,(list-mean (map y (superpixel-pixels s)))))

(define (superpixel-correspondence ss ss-map)
 ;; TODO Check image boundaries
 ;; TODO Record magnitudes and pick the max
 (image-ref
  ss-map
  (quantize-point (if (superpixel-velocity ss)
		      (v+ (superpixel-center ss) (superpixel-velocity ss))
		      (superpixel-center ss)))))

(define (optical-flow-superpixel superpixel flow)
 (map (lambda (p) (vector (image-ref (first flow) p)
			  (image-ref (second flow) p)))
      (superpixel-pixels superpixel)))

(define (average-optical-flow-superpixel superpixel flow)
 (list-mean (map (lambda (p) (vector (image-ref (first flow) p)
				     (image-ref (second flow) p)))
		 (superpixel-pixels superpixel))))

(define (superpixel-track superpixel superpixel-map)
 (let* ((center (superpixel-center superpixel))
	(new (v+ center (superpixel-velocity superpixel))))
  (safe-matrix-ref superpixel-map (quantize-coordinate (y new)) (quantize-coordinate (x new))
		   (matrix-ref superpixel-map
			       (quantize-coordinate (y center)) (quantize-coordinate (x center))))))

(define (safe-superpixel-velocity s)
 (if (superpixel-velocity s)
     (superpixel-velocity s)
     '#(0 0)))

(define (superpixel->outline s) (region->boundary (superpixel-pixels s)))
(define (superpixel->region s) (boundary->region (superpixel-pixels s)))

(define (superpixel-annotate-blank-ppm annotations blank-ppm)
 (for-each
  (lambda (c)
   (unless (equal? (superpixel-annotation-label c) 'none)
    (let ((colour (superpixel-anotation->colour c)))
     (for-each (lambda (p) (set-ppm-pixel! blank-ppm (x p) (y p) colour))
	       (superpixel-pixels (superpixel-annotation-superpixel c))))))
  annotations)
 blank-ppm)

(define (superpixel-annotation-on-ppm annotations image)
 (for-each
  (lambda (c)
   (let ((colour (superpixel-anotation->colour c)))
    (for-each (lambda (p) (set-ppm-pixel! image (x p) (y p) colour))
	      (superpixel->outline (superpixel-annotation-superpixel c)))))
  annotations)
 image)

(define (superpixel-annotation->pixels a)
 (superpixel-pixels (superpixel-annotation-superpixel a)))

(define (superpixel-anotation->colour a)
 (case (superpixel-annotation-label a)
  ((none) '#(255 0 0))
  ((person1) '#(0 0 255))
  ((person2) '#(0 255 0))
  (else (fuck-up))))

(define (frame->empty-annotations superpixels)
 (map (lambda (s) (make-superpixel-annotation 'none s)) superpixels))

;;; Binary superpixels

(define (write-superpixel-id id port)
 (write-char (cond ((number? id) #\0)
		   ((vector? id) #\1)
		   ((list? id) #\2)
		   ((boolean? id) #\3)
		   (else (fuck-up)))
	     port)
 (cond ((number? id) (write-32-bit-integer id port))
       ((vector? id)
	(write-16-bit-integer (vector-length id) port)
	(for-each-vector (lambda (e) (write-superpixel-id e port)) id))
       ((list? id)
	(write-16-bit-integer (length id) port)
	(for-each (lambda (e) (write-superpixel-id e port)) id))
       ((boolean? id) (write-char (if id #\1 #\0) port))
       (else (fuck-up))))

(define (read-superpixel-id port)
 (let ((type (read-char port)))
  (cond ((char=? type #\0) (read-32-bit-integer port))
	((char=? type #\1)
	 (map-n-vector (lambda (n) (read-superpixel-id port)) (read-16-bit-integer port)))
	((char=? type #\2)
	 (map-n (lambda (n) (read-superpixel-id port)) (read-16-bit-integer port)))
	((char=? type #\3) (char=? #\1 (read-char port)))
	(else (fuck-up)))))

(define (write-binary-superpixels superpixels filename)
 (define (write-float float port)
  (write-32-bit-integer (inexact->exact (* 1000 float)) port))
 (call-with-output-file
   filename
  (lambda (port)
   (write-16-bit-integer (length superpixels) port)
   (for-each
    (lambda (s)
     (write-superpixel-id (superpixel-name s) port)
     (write-16-bit-integer (length (superpixel-next s)) port)
     (for-each (lambda (n) (write-superpixel-id n port)) (superpixel-next s))
     (write-superpixel-id (superpixel-parent s) port)
     (if (not (superpixel-children s))
	 (write-16-bit-integer 0 port)
	 (begin (write-16-bit-integer (length (superpixel-children s)) port)
		(for-each (lambda (n) (write-superpixel-id n port)) (superpixel-children s))))
     (write-float (x (superpixel-velocity s)) port)
     (write-float (y (superpixel-velocity s)) port)
     (write-16-bit-integer (length (superpixel-pixels s)) port)
     (for-each (lambda (p) (write-16-bit-integer (x p) port) (write-16-bit-integer (y p) port))
	       (superpixel-pixels s)))
    superpixels))))

(define (read-binary-superpixels filename)
 (define (read-float port) (/ (read-32-bit-integer port) 1000))
 (call-with-input-file
   filename
  (lambda (port)
   (map-n
    (lambda (i)
     (make-superpixel (read-superpixel-id port) ;; name
		      (map-n (lambda (_) (read-superpixel-id port)) (read-16-bit-integer port)) ;; track
		      (read-superpixel-id port)
		      (map-n (lambda (_) (read-superpixel-id port)) (read-16-bit-integer port))
		      (let* ((x (read-float port))
			     (y (read-float port)))
		       (vector x y))
		      (map-n (lambda (_) (let* ((x (read-16-bit-integer port))
						(y (read-16-bit-integer port)))
					  (vector x y)))
			     (read-16-bit-integer port))))
    (read-16-bit-integer port)))))

(define (write-binary-int-matrix matrix filename)
 (call-with-output-file
   filename
  (lambda (port)
   (write-32-bit-integer (matrix-rows matrix) port)
   (write-32-bit-integer (matrix-columns matrix) port)
   (for-each-vector
    (lambda (v) (for-each-vector (lambda (e) (write-32-bit-integer e port)) v))
    matrix)
   #f)))

(define (read-binary-int-matrix filename)
 (call-with-input-file
   filename
  (lambda (port)
   (let* ((rows (read-32-bit-integer port))
	  (columns (read-32-bit-integer port))
	  (matrix (make-matrix rows columns)))
    (for-each-n
     (lambda (i) (for-each-n (lambda (j) (matrix-set! matrix i j (read-32-bit-integer port))) columns))
     rows)
    matrix))))

(define c-read-binary-superpixel-map (c-function pointer ("read_binary_superpixel_map" string)))
(define c-superpixel-map (c-function pointer ("superpixel_map" pointer)))
(define c-superpixel-map-ref (c-function int ("superpixel_map_ref" pointer int int int)))

;;; Image closure

(define (superpixel->matlab-closure-map s output w h)
 (scheme->matlab! "superpixels" (superpixels->map s w h))
 (matlab-eval-strings "video.superpixels = superpixels")
 (matlab-save-variables output "video")
 #f)

;;; Connected Components

(define (pbm->graph pbm delta)
 ;; This uses max(delta-y, delta-x) distance metric.
 ;; needs work: a variant that uses Euclidean distance.
 (let* ((bitmap (pbm-bitmap pbm))
	(height (pnm-height pbm))
	(width (pnm-width pbm))
	(grid (map-n-vector
	       (lambda (i)
		(map-n-vector
		 (lambda (j)
		  (if (matrix-ref bitmap i j)
		      (make-vertex (list (vector j i)) #f '())
		      #f))
		 width))
	       height))
	(vertices '())
	(edges '()))
  (for-each-vector
   (lambda (row)
    (for-each-vector
     (lambda (element) (when element (set! vertices (cons element vertices))))
     row))
   grid)
  (for-each-n
   (lambda (i)
    (for-each-n
     (lambda (j)
      (when (matrix-ref grid i j)
       (for-each-m-n
	(lambda (i1)
	 (for-each-m-n
	  (lambda (j1)
	   (when (matrix-ref grid i1 j1)
	    (unless (and (= i i1) (= j j1))
	     (set! edges
		   (cons (make-edge (matrix-ref grid i j)
				    (matrix-ref grid i1 j1))
			 edges)))))
	  (max (- j delta) 0)
	  (min (+ j delta) (- width 1))))
	(max (- i delta) 0)
	(min (+ i delta) (- height 1)))))
     width))
   height)
  (make-graph vertices edges)))

(define (labeling->graph labeling delta)
 ;; This uses a Manhattan distance metric, but scans all matches within
 ;; a max(dx, dy) = delta distance
 ;; needs work: a variant that uses Euclidean distance.
 (let* ((height (matrix-rows labeling))
	(width (matrix-columns labeling))
	(grid (map-n-vector
	       (lambda (i)
		(map-n-vector
		 (lambda (j)
		  (make-vertex (list (vector j i)) #f '()))
		 width))
	       height))
	(vertices '())
	(edges '()))
  (for-each-vector
   (lambda (row)
    (for-each-vector
     (lambda (element) (set! vertices (cons element vertices)))
     row))
   grid)
  (for-each-n
   (lambda (i)
    (for-each-n
     (lambda (j)
      (when (matrix-ref grid i j)
       (for-each-m-n
	(lambda (i1)
	 (for-each-m-n
	  (lambda (j1)
	   (when (= (matrix-ref labeling i1 j1) (matrix-ref labeling i j))
	    (unless (or (and (= i i1) (= j j1))
			(> (+ (abs (- i i1)) (abs (- j j1))) delta))
	     (set! edges
		   (cons (make-edge (matrix-ref grid i j)
				    (matrix-ref grid i1 j1))
			 edges)))))
	  (max (- j delta) 0)
	  (min (+ j delta) (- width 1))))
	(max (- i delta) 0)
	(min (+ i delta) (- height 1)))))
     width))
   height)
  (make-graph vertices edges)))

(define (dereference-vertex u)
 (if (vertex-vertex u)
     (let ((v (dereference-vertex (vertex-vertex u)))) (set-vertex-vertex! u v))
     u))

(define (connected-components g)
 (for-each (lambda (u)
	    (set-vertex-vertex! u #f)
	    (set-vertex-edges! u '()))
	   (graph-vertices g))
 (for-each (lambda (e)
	    (let ((u (dereference-vertex (edge-u e)))
		  (v (dereference-vertex (edge-v e))))
	     (unless (eq? u v) (set-vertex-vertex! u v))))
	   (graph-edges g))
 (let ((classes
	(transitive-equivalence-classesp
	 (lambda (u v) (eq? (dereference-vertex u) (dereference-vertex v)))
	 (graph-vertices g))))
  (for-each
   (lambda (e)
    (when (eq? (dereference-vertex (edge-u e)) (dereference-vertex (edge-v e)))
     (set-vertex-edges!
      (dereference-vertex (edge-u e))
      (cons e (vertex-edges (dereference-vertex (edge-u e)))))))
   (graph-edges g))
  (map (lambda (class)
	(make-graph class (vertex-edges (dereference-vertex (first class)))))
       classes)))

(define (vertices->pbm vertices height width)
 (let ((bitmap (make-matrix height width #f)))
  (for-each (lambda (u)
	     (for-each (lambda (pixel)
			(let ((y (quantize-coordinate (y pixel)))
			      (x (quantize-coordinate (x pixel))))
			 (when (and (>= y 0) (>= x 0) (< y height) (< x width))
			  (matrix-set! bitmap y x #t))))
		       (vertex-pixels u)))
	    vertices)
  (make-pbm #t bitmap)))

(define (graph->pbm g height width)
 (vertices->pbm (graph-vertices g) height width))

;;; Chains

(define (pbm->chains pbm)
 ;; Pixels at junctions can be in more than one chain.
 (let* ((m (pbm-bitmap pbm))
	(l (make-matrix (matrix-rows m) (matrix-columns m) '()))
	(chains '()))
  (define (trace-it i j)
   (let ((i-next (y (first (matrix-ref l i j))))
	 (j-next (x (first (matrix-ref l i j)))))
    (matrix-set! l i j (remove (vector j-next i-next) (matrix-ref l i j)))
    (cons
     (vector j i)
     (let loop ((i i-next) (j j-next) (i-previous i) (j-previous j))
      (unless (member (vector j-previous i-previous) (matrix-ref l i j))
       (fuck-up))
      (case (length (matrix-ref l i j))
       ((0) (fuck-up))
       ((2) (let ((entry (remove (vector j-previous i-previous)
				 (matrix-ref l i j))))
	     (matrix-set! l i j '())
	     (cons (vector j i)
		   (loop (y (first entry)) (x (first entry)) i j))))
       (else (matrix-set! l i j
			  (remove (vector j-previous i-previous)
				  (matrix-ref l i j)))
	     (list (vector j i))))))))
  (for-each-n
   (lambda (i)
    (for-each-n
     (lambda (j)
      (when (matrix-ref m i j)
       (when (and (> i 0) (> j 0) (matrix-ref m (- i 1) (- j 1)))
	(matrix-set! l i j (cons (vector (- j 1) (- i 1)) (matrix-ref l i j))))
       (when (and (> i 0)
		  (< j (- (matrix-columns m) 1))
		  (matrix-ref m (- i 1) (+ j 1)))
	(matrix-set! l i j (cons (vector (+ j 1) (- i 1)) (matrix-ref l i j))))
       (when (and (< i (- (matrix-rows m) 1))
		  (> j 0)
		  (matrix-ref m (+ i 1) (- j 1)))
	(matrix-set! l i j (cons (vector (- j 1) (+ i 1)) (matrix-ref l i j))))
       (when (and (< i (- (matrix-rows m) 1))
		  (< j (- (matrix-columns m) 1))
		  (matrix-ref m (+ i 1) (+ j 1)))
	(matrix-set! l i j (cons (vector (+ j 1) (+ i 1)) (matrix-ref l i j))))
       (when (and (> i 0) (matrix-ref m (- i 1) j))
	(matrix-set! l i j (cons (vector j (- i 1)) (matrix-ref l i j))))
       (when (and (> j 0) (matrix-ref m i (- j 1)))
	(matrix-set! l i j (cons (vector (- j 1) i) (matrix-ref l i j))))
       (when (and (< i (- (matrix-rows m) 1)) (matrix-ref m (+ i 1) j))
	(matrix-set! l i j (cons (vector j (+ i 1)) (matrix-ref l i j))))
       (when (and (< j (- (matrix-columns m) 1)) (matrix-ref m i (+ j 1)))
	(matrix-set! l i j (cons (vector (+ j 1) i) (matrix-ref l i j))))))
     (matrix-columns m)))
   (matrix-rows m))
  (let loop ()
   (cond
    ((some-n (lambda (i)
	      (some-n (lambda (j) (= (length (matrix-ref l i j)) 1))
		      (matrix-columns l)))
	     (matrix-rows l))
     (for-each-n (lambda (i)
		  (for-each-n (lambda (j)
			       (when (= (length (matrix-ref l i j)) 1)
				(set! chains (cons (trace-it i j) chains))))
			      (matrix-columns l)))
		 (matrix-rows l))
     (loop))
    ((some-n (lambda (i)
	      (some-n (lambda (j) (not (null? (matrix-ref l i j))))
		      (matrix-columns l)))
	     (matrix-rows l))
     (let* ((i (find-if (lambda (i)
			 (some-n (lambda (j) (not (null? (matrix-ref l i j))))
				 (matrix-columns l)))
			(enumerate (matrix-rows l))))
	    (j (find-if (lambda (j) (not (null? (matrix-ref l i j))))
			(enumerate (matrix-columns l)))))
      (set! chains (cons (trace-it i j) chains))
      (loop)))))
  (append
   ;; This is because with the above method, singleton pixels, either initial
   ;; ones or one left after chain removal, will not be included in chains.
   (let ((m (pbm-bitmap
	     (pbm-and2
	      pbm
	      (pbm-not
	       (chains->pbm chains (pnm-height pbm) (pnm-width pbm)))))))
    (reduce
     append
     (map-n (lambda (i)
	     (reduce
	      append
	      (map-n (lambda (j)
		      (if (matrix-ref m i j) (list (list (vector j i))) '()))
		     (matrix-columns m))
	      '()))
	    (matrix-rows m))
     '()))
   chains)))

(define (chains->pbm chains height width)
 (let ((m (make-matrix height width #f)))
  (for-each (lambda (chain)
	     (for-each (lambda (point)
			(let ((y (quantize-coordinate (y point)))
			      (x (quantize-coordinate (x point))))
			 (when (and (>= y 0) (>= x 0) (< y height) (< x width))
			  (matrix-set! m y x #t))))
		       chain))
	    chains)
  (make-pbm #t m)))

(define (chain-filter pbm threshold)
 (chains->pbm (remove-if (lambda (chain) (< (length chain) threshold))
			 (pbm->chains pbm))
	      (pnm-height pbm)
	      (pnm-width pbm)))

(define (break-chain chain l)
 (let loop ((chain chain) (chains '()))
  (if (< (length chain) l)
      (if (null? chain) chains (cons chain chains))
      (loop (sublist chain l (length chain))
	    (cons (sublist chain 0 l) chains)))))

(define (break-chains chains l)
 (map-reduce append '() (lambda (chain) (break-chain chain l)) chains))

(define (pbm-and2 pbm1 pbm2)
 (unless (and (pbm? pbm1)
	      (pbm? pbm2)
	      ;; no need to confirm that they are both raw/ascii pbms
	      ;; (eq? (pbm-raw? pbm1) (pbm-raw? pbm2))
	      (= (pnm-width pbm1) (pnm-width pbm2))
	      (= (pnm-height pbm1) (pnm-height pbm2)))
  (display (list (pbm? pbm1)
		 (pbm? pbm2)
		 (eq? (pbm-raw? pbm1) (pbm-raw? pbm2))
		 (= (pnm-width pbm1) (pnm-width pbm2))
		 (= (pnm-height pbm1) (pnm-height pbm2))))
  (newline)
  (panic "Arguments to PBM-AND are not matching PBMs"))
 (make-pbm (pbm-raw? pbm1)
	   (map-vector
	    (lambda (row1 row2)
	     (map-vector (lambda (bit1 bit2) (and bit1 bit2)) row1 row2))
	    (pbm-bitmap pbm1)
	    (pbm-bitmap pbm2))))

;;; Conjuring

(define (connected-component-filter pbm delta threshold)
 (map-reduce
  pbm-or
  (pbm-constant (pnm-width pbm) (pnm-height pbm) #f)
  (lambda (points) (points->pbm points (pnm-height pbm) (pnm-width pbm)))
  (remove-if
   (lambda (points) (< (length points) threshold))
   (map (lambda (g)
	 (pbm->points (graph->pbm g (pnm-height pbm) (pnm-width pbm))))
	(connected-components (pbm->graph pbm delta))))))

(define (conjure pbms delta span threshold1 threshold2)
 (let* ((new-pbms (list->vector pbms))
	(pbms (list->vector pbms))
	(height (pnm-height (vector-ref pbms 0)))
	(width (pnm-width (vector-ref pbms 0))))
  (for-each-n
   (lambda (i)
    (format #t "Frame ~s~%" i)
    (for-each
     (lambda (g)
      (let ((points (pbm->points (graph->pbm g height width))))
       (format #t "~s point~a~%"
	       (length points)
	       (if (= (length points) 1) "" "s"))
       (when (> (length points) threshold1)
	(let ((pbm (points->pbm points height width)))
	 (do ((j 1 (+ j 1)))
	   ((or (= (+ i j) (vector-length pbms))
		(= j span)
		(> (count-if
		    (lambda (point)
		     (matrix-ref
		      (pbm-bitmap (vector-ref pbms j)) (y point) (x point)))
		    points)
		   threshold2))
	    (if (or (= (+ i j) (vector-length pbms)) (= j span))
		(begin
		 (if (= (+ i j) (vector-length pbms))
		     (format #t "End of movie~%")
		     (format #t "Span too long~%"))
		 #f)
		(begin
		 (cond
		  ((positive? (- j 2))
		   (format #t "Conjuring ~s point~a from frame ~s ~s frame~a~%"
			   (length points)
			   (if (= (length points) 1) "" "s")
			   i
			   (- j 2)
			   (if (= (- j 2) 1) "" "s")))
		  ((zero? (- j 2)) (format #t "zero~%"))
		  (else (format #t "negative~%")))
		 (for-each-n
		  (lambda (k)
		   (vector-set! new-pbms
				(+ i k 1)
				(pbm-or (vector-ref new-pbms (+ i k 1)) pbm)))
		  (- j 2))))))))))
     (connected-components (pbm->graph (vector-ref pbms i) delta))))
   (vector-length pbms))
  (vector->list new-pbms)))

;;; Ratio contour

(define (pgm->rc-graph pgm alpha delta)
 (pgm-weights->rc-graph pgm compute-solid-weight1 alpha delta))

(define (pgm-weights->rc-graph pgm solid-w1 alpha delta)
 (chains-weights->rc-graph (remove-if (lambda (c) (< (length c) 2))
				      (pbm->chains (pgm->pbm pgm 1)))
			   (lambda (a) (solid-w1 a pgm))
			   compute-solid-weight2
			   (compute-dashed-weight1 alpha)
			   compute-dashed-weight2
			   delta))

(define (chains-weights->rc-graph chains solid-w1 solid-w2 dashed-w1 dashed-w2 delta)
 (let* ((solid-edges
	 (map-indexed (lambda (chain i)
		       (make-solid-edge
			(make-rc-vertex (list (first chain) (* 2 i)) #f #f)
			(make-rc-vertex (list (last chain) (+ (* 2 i) 1)) #f #f)
			(solid-w1 chain)
			(solid-w2 chain)
			chain))
		      chains))
	(vertices (map (lambda (a) (list (solid-edge-u a) (solid-edge-v a)))
		       solid-edges))
	(dashed-edges (rc-erect-dashed-edges vertices dashed-w1 dashed-w2 delta)))
  (list vertices solid-edges dashed-edges)))

(define (rc-erect-dashed-edges vertices dashed-w1 dashed-w2 delta)
 (let ((dashed-edges '()))
  (do ((vertices1 vertices (rest vertices1))) ((null? vertices1))
   (let ((u (first vertices1)))
    (for-each
     (lambda (v)
      (for-each
       (lambda (u)
	(for-each
	 (lambda (v)
	  (when (< (distance (first (rc-vertex-pixels u))
			     (first (rc-vertex-pixels v)))
		   delta)
	   (set! dashed-edges
		 ;; needs work: to guarantee v is further right than u
		 (cons (make-dashed-edge u
					 v
					 (dashed-w1 u v)
					 (dashed-w2 u v))
		       dashed-edges))))
	 v))
       u))
     (rest vertices1))))
  dashed-edges))

(define (write-rc-output file vertices solid-edges dashed-edges)
 (call-with-output-file file
  (lambda (port)
   (format port "~a~%" (* 2 (length vertices)))
   (for-each
    (lambda (v)
     (format port "~a ~a~%"
	     (x (car (rc-vertex-pixels (car v))))
	     (y (car (rc-vertex-pixels (car v)))))
     (format port "~a ~a~%"
	     (x (car (rc-vertex-pixels (cadr v))))
	     (y (car (rc-vertex-pixels (cadr v))))))
    vertices)
   (for-each
    (lambda (e)
     (format port "~a ~a ~a ~a 1~%"
	     (second (rc-vertex-pixels (solid-edge-u e)))
	     (second (rc-vertex-pixels (solid-edge-v e)))
	     (solid-edge-w1 e)
	     (solid-edge-w2 e)))
    solid-edges)
   (for-each
    (lambda (e)
     (format port "~a ~a ~a ~a 0~%"
	     (second (rc-vertex-pixels (dashed-edge-u e)))
	     (second (rc-vertex-pixels (dashed-edge-v e)))
	     (dashed-edge-w1 e)
	     (dashed-edge-w2 e)))
    dashed-edges))))

(define (write-solid-lines file solid)
 (call-with-output-file file
  (lambda (port)
   (for-each
    (lambda (solid)
     (format port "~a ~a ~a ~a~%"
	     (x (first (solid-edge-pixels solid)))
	     (y (first (solid-edge-pixels solid)))
	     (x (last (solid-edge-pixels solid)))
	     (y (last (solid-edge-pixels solid)))))
    solid))))

(define (remove-closed-chains chains)
 (remove-if (lambda (c) (equal? (first c) (last c))) chains))

(define (compute-solid-weight1 chain pgm) 0)

(define (compute-solid-weight2 chain)
 (compute-non-simple-polygon-area chain))

(define (compute-dashed-weight1 alpha)
 (lambda (u v)
  (let ((u (first (rc-vertex-pixels u))) (v (first (rc-vertex-pixels v))))
   (expt (distance u v) alpha))))

(define (compute-dashed-weight2 u v)
 (let ((u (first (rc-vertex-pixels u))) (v (first (rc-vertex-pixels v))))
  (polygon-area `(,`#(,(x u) 0)
		  ,u
		  ,v
		  ,`#(,(x v) 0)
		  ,`#(,(x u) 0)))))

(define (rc-cycle-solid-dashed->ppm cycle)
 (let ((image (ppm-constant 320 240 0 0 0)))
  (for-each
   (lambda (p) (matrix-set! (ppm-red image) (inexact->exact (y p)) (inexact->exact (x p)) 255))
   (map-reduce append '() solid-edge-pixels
	       (remove-if-not solid-edge? cycle)))
  (for-each
   (lambda (p) (matrix-set! (ppm-green image) (inexact->exact (y p)) (inexact->exact (x p)) 255))
   (map-reduce
    append
    '()
    (lambda (e)
     (line-segment->points
      (make-line-segment (car (rc-vertex-pixels (any-edge-u e)))
			 (car (rc-vertex-pixels (any-edge-v e))))))
    (remove-if-not dashed-edge? cycle)))
  image))

(define (rc-cycle-solid->ppm cycle)
 (let ((image (ppm-constant 320 240 0 0 0)))
  (for-each
   (lambda (p) (matrix-set! (ppm-red image) (y p) (x p) 255))
   (map-reduce append '() solid-edge-pixels
	       (remove-if-not solid-edge? cycle)))
  image))

(define (rc-cycle-cost cycle s-w1 s-w2 d-w1 d-w2)
 (/
  (map-reduce
   +
   0
   (lambda (e)
    (cond ((solid-edge? e) (s-w1 (solid-edge-pixels e)))
	  ((dashed-edge? e) (d-w1 (car (rc-vertex-pixels (dashed-edge-u e)))
				  (car (rc-vertex-pixels (dashed-edge-v e)))))
	  (else (fuck-up))))
   cycle)
  (map-reduce
   +
   0
   (lambda (e)
    (cond ((solid-edge? e) (s-w2 (solid-edge-pixels e)))
	  ((dashed-edge? e) (d-w2 (car (rc-vertex-pixels (dashed-edge-u e)))
				  (car (rc-vertex-pixels (dashed-edge-v e)))))
	  (else (fuck-up))))
   cycle)))

(define (find-edge-with-point solid-edges point)
 (find-if (lambda (e) (member point (solid-edge-pixels e))) solid-edges))

(define (find-edge-between-vertices edges v1 v2)
 (find-if
  (lambda (e)
   (or
    (equal? (list v1 v2) (list (second (rc-vertex-pixels (any-edge-u e)))
			       (second (rc-vertex-pixels (any-edge-v e)))))
    (equal? (list v2 v1) (list (second (rc-vertex-pixels (any-edge-u e)))
			       (second (rc-vertex-pixels (any-edge-v e)))))))
  edges))

(define (remove-edge-from-cycle cycle v1 v2)
 (remove-if
  (lambda (e)
   (or
    (equal? (list v1 v2) (list (second (rc-vertex-pixels (any-edge-u e)))
			       (second (rc-vertex-pixels (any-edge-v e)))))
    (equal? (list v2 v1) (list (second (rc-vertex-pixels (any-edge-u e)))
			       (second (rc-vertex-pixels (any-edge-v e)))))))
  cycle))

(define (debug-rc-cycle-cost cycle s-w1 s-w2 d-w1 d-w2)
 (list
  (map-reduce
   +
   0
   (lambda (e)
    (cond ((solid-edge? e) (s-w1 (solid-edge-pixels e)))
	  ((dashed-edge? e) (d-w1 (car (rc-vertex-pixels (dashed-edge-u e)))
				  (car (rc-vertex-pixels (dashed-edge-v e)))))
	  (else (fuck-up))))
   cycle)
  (map-reduce
   +
   0
   (lambda (e)
    (cond ((solid-edge? e) (s-w2 (solid-edge-pixels e)))
	  ((dashed-edge? e) (d-w2 (car (rc-vertex-pixels (dashed-edge-u e)))
				  (car (rc-vertex-pixels (dashed-edge-v e)))))
	  (else (fuck-up))))
   cycle)))

(define (write-rc-cycle file cycle)
 (let* ((solid-edges (remove-if-not solid-edge? cycle))
	(dashed-edges (remove-if-not dashed-edge? cycle))
	(vertices (map (lambda (a) (list (solid-edge-u a) (solid-edge-v a))) solid-edges)))
  (call-with-output-file file
   (lambda (port)
    (format port "~a~%" (* 2 (length vertices)))
    (for-each
     (lambda (v)
      (format port "~a ~a~%"
	      (x (car (rc-vertex-pixels (car v))))
	      (y (car (rc-vertex-pixels (car v)))))
      (format port "~a ~a~%"
	      (x (car (rc-vertex-pixels (cadr v))))
	      (y (car (rc-vertex-pixels (cadr v))))))
     vertices)
    (for-each
     (lambda (e)
      (format port "~a ~a ~a ~a 1~%"
	      (second (rc-vertex-pixels (solid-edge-u e)))
	      (second (rc-vertex-pixels (solid-edge-v e)))
	      (solid-edge-w1 e)
	      (solid-edge-w2 e)))
     solid-edges)
    (for-each
     (lambda (e)
      (format port "~a ~a ~a ~a 0~%"
	      (second (rc-vertex-pixels (dashed-edge-u e)))
	      (second (rc-vertex-pixels (dashed-edge-v e)))
	      (dashed-edge-w1 e)
	      (dashed-edge-w2 e)))
     dashed-edges)))))

(define (update-vertices l)
 (let* ((n 0)
	(vertex-map (memoize (lambda (a)
			      (set! n (+ n 1))
			      (- n 1)))))
  (map
   (lambda (e)
    (cond ((solid-edge? e)
	   (let ((u (vertex-map (second (rc-vertex-pixels (solid-edge-u e)))))
		 (v (vertex-map (second (rc-vertex-pixels (solid-edge-v e))))))
	    (make-solid-edge
	     (make-rc-vertex (list (car (rc-vertex-pixels (solid-edge-u e))) u) #f #f)
	     (make-rc-vertex (list (car (rc-vertex-pixels (solid-edge-v e))) v) #f #f)
	     (solid-edge-w1 e)
	     (solid-edge-w2 e)
	     (solid-edge-pixels e))))
	  ((dashed-edge? e)
	   (let ((u (vertex-map (second (rc-vertex-pixels (dashed-edge-u e)))))
		 (v (vertex-map (second (rc-vertex-pixels (dashed-edge-v e))))))
	    (make-dashed-edge
	     (make-rc-vertex (list (car (rc-vertex-pixels (dashed-edge-u e))) u) #f #f)
	     (make-rc-vertex (list (car (rc-vertex-pixels (dashed-edge-v e))) v) #f #f)
	     (dashed-edge-w1 e)
	     (dashed-edge-w2 e))))
	  (else (fuck-up))))
   l)))

(define (any-edge-u e)
 (cond ((solid-edge? e) (solid-edge-u e))
       ((dashed-edge? e) (dashed-edge-u e))
       (else (fuck-up))))

(define (any-edge-v e)
 (cond ((solid-edge? e) (solid-edge-v e))
       ((dashed-edge? e) (dashed-edge-v e))
       (else (fuck-up))))

(define (any-edge-pixels e)
 (cond ((solid-edge? e) (solid-edge-pixels e))
       ((dashed-edge? e) '())
       (else (fuck-up))))

(define (read-cycle-chains-from-file chain filename)
 (map
  (lambda (l)
   (let* ((u (inexact->exact (floor (/ (string->number (field-ref l 0)) 2))))
	  (v (inexact->exact (floor (/ (string->number (field-ref l 1)) 2))))
	  (solid (find-if
		  (lambda (s)
		   (and (= (second (rc-vertex-pixels (solid-edge-u s))) u)
			(= (second (rc-vertex-pixels (solid-edge-v s))) v)))
		  (rc-chains-solid-edges chain)))
	  (dashed
	   (find-if
	    (lambda (s)
	     (and (= (second (rc-vertex-pixels (dashed-edge-u s))) u)
		  (= (second (rc-vertex-pixels (dashed-edge-v s))) v)))
	    (rc-chains-dashed-edges chain))))
    (cond (solid solid)
	  (dashed dashed)
	  (else (fuck-up)))))
  (cdr (read-file filename))))

(define (read-cycle-chains chain prefix n)
 (read-cycle-chains-from-file chain (string-append prefix "-" (number->string n) ".cycle")))

(define (point vertex) (first (rc-vertex-pixels vertex)))

(define (label vertex) (second (rc-vertex-pixels vertex)))

(define (other-point u)
 (let ((e (rc-vertex-solid-edge u)))
  (if (v= (point u) (point (solid-edge-u e)))
      (point (solid-edge-v e))
      (point (solid-edge-u e)))))

(define (forward-point? point other-point)
 (or (> (x point) (x other-point))
     (and (= (x point) (x other-point)) (> (y point) (y other-point)))))

(define (update-edge-u! e u)
 (if (dashed-edge? e)
     (set-dashed-edge-u! e u)
     (set-solid-edge-u! e u)))
(define (update-edge-v! e v)
 (if (dashed-edge? e)
     (set-dashed-edge-v! e v)
     (set-solid-edge-v! e v)))

(define (update-edge-vertex-pointers! vertices edges)
 (map
  (lambda (e)
   (update-edge-u! e (find (any-edge-u e) vertices))
   (update-edge-v! e (find (any-edge-v e) vertices)))
  edges))

(define (rc-chains->graph chains)
 (let* ((vertices (reduce append (rc-chains-vertices chains) '()))
	(edges (append (rc-chains-solid-edges chains)
		       (rc-chains-dashed-edges chains))))
  (update-edge-vertex-pointers! vertices edges)
  (make-graph vertices edges)))

(define (rc-graph-pgm->rc-chains pgm rc-graph)
 (make-rc-chains (pnm-height pgm) (pnm-width pgm)
		 (first rc-graph) (second rc-graph) (third rc-graph)))

(define (convert-graph g)
 (let* ((vertices (graph-vertices g))
	(i 0)
	(next-i! (lambda () (set! i (+ 1 i)) (- i 1)))
	(new-vertices (map (lambda (vertex)
			    (set-rc-vertex-pixels! vertex (list (point vertex) (next-i!)))
			    (make-rc-vertex
			     (list (point vertex) (next-i!)) #f #f))
			   vertices)))
  ;; A fast bijection between old and new vertices.
  (for-each (lambda (vertex new-vertex)
	     (set-rc-vertex-vertex! vertex new-vertex)
	     (set-rc-vertex-vertex! new-vertex vertex))
	    vertices new-vertices)
  (let ((solid-edges
	 (map-reduce
	  append
	  '()
	  (lambda (solid-edge)
	   (let* ((u (solid-edge-u solid-edge))
		  (v (solid-edge-v solid-edge))
		  (w1 (solid-edge-w1 solid-edge))
		  (w2 (solid-edge-w2 solid-edge))
		  (pixels (solid-edge-pixels solid-edge))
		  (new-u (rc-vertex-vertex u))
		  (new-v (rc-vertex-vertex v)))
	    (list (make-solid-edge u v (abs w1) (abs w2) pixels)
		  (make-solid-edge new-u new-v (abs w1) (- (abs w2)) pixels))))
	  (remove-if-not solid-edge? (graph-edges g)))))
   ;; A fast map from vertices to solid edges.
   (for-each (lambda (solid-edge)
	      (set-rc-vertex-solid-edge! (solid-edge-u solid-edge) solid-edge)
	      (set-rc-vertex-solid-edge! (solid-edge-v solid-edge) solid-edge))
	     solid-edges)
   (make-graph
    (append vertices new-vertices)
    (append
     solid-edges
     (map-reduce2
      (lambda (l1 l2) (append l2 l1))
      '()
      (lambda (dashed-edge)
       (let* ((w1 (abs (dashed-edge-w1 dashed-edge)))
	      (w2 (abs (dashed-edge-w2 dashed-edge)))
	      (u (dashed-edge-u dashed-edge))
	      (v (dashed-edge-v dashed-edge))
	      (new-u (rc-vertex-vertex u))
	      (new-v (rc-vertex-vertex v)))
	(when (eq? u v) (panic "Nonunique solid edges!"))
	(cond
	 ((eq? (rc-vertex-solid-edge u) (rc-vertex-solid-edge v))
	  (panic "Nonunique vertices!"))
	 (else
	  (let ((point-u (point u))
		(point-v (point v))
		(other-point-u (other-point u))
		(other-point-v (other-point v)))
	   (cond
	    ((or (v= point-u other-point-u)
		 (v= point-v other-point-v)
		 (v= point-u other-point-v)
		 (v= point-v other-point-u))
	     (display "Endpoint overlap (zero-length solid edge)")
	     (newline)
	     '())
	    (else
	     (let ((w2 (if (> (x point-u) (x point-v)) (- w2) w2)))
	      (if (forward-point? point-u other-point-u)
		  (if (forward-point? point-v other-point-v)
		      (list (make-dashed-edge u new-v w1 w2)
			    (make-dashed-edge new-u v w1 (- w2)))
		      (list (make-dashed-edge u v w1 w2)
			    (make-dashed-edge new-u new-v w1 (- w2))))
		  (if (forward-point? point-v other-point-v)
		      (list (make-dashed-edge new-u new-v w1 w2)
			    (make-dashed-edge u v w1 (- w2)))
		      (list (make-dashed-edge new-u v w1 w2)
			    (make-dashed-edge u new-v w1 (- w2)))))))))))))
      (remove-if-not dashed-edge? (graph-edges g))))))))

(define (write-w-file g pathname)
 (define (map-to-int w) (inexact->exact (floor (* 1000 w))))
 (call-with-output-file (default-extension pathname "w")
  (lambda (port)
   (format port "~s ~s~%"
	   ;; (map-reduce
	   ;;  max
	   ;;  minus-infinity
	   ;;  (lambda (e) (max (label (solid-edge-u e)) (label (solid-edge-v e))))
	   ;;  (remove-if-not solid-edge? (graph-edges g)))
	   (* 2 (length (remove-if-not solid-edge? (graph-edges g))))
	   (length (graph-edges g)))
   (for-each (lambda (e)
	      (if (solid-edge? e)
		  (format port "~s ~s ~s ~s 1~%"
			  (label (solid-edge-u e))
			  (label (solid-edge-v e))
			  (map-to-int (solid-edge-w2 e))
			  (map-to-int (solid-edge-w1 e)))
		  (format port "~s ~s ~s ~s 0~%"
			  (label (dashed-edge-u e))
			  (label (dashed-edge-v e))
			  (map-to-int (dashed-edge-w2 e))
			  (map-to-int (dashed-edge-w1 e)))))
	     (graph-edges g)))))

(define (rc-draw-cycle-edges prefix cycle)
 (let ((output-file (string-append prefix "-cycle.png")))
  (system (format #f "convert ~a ~a"
		  (string-append prefix ".pgm")
		  output-file))
  (draw-coloured-lines
   (map (lambda (e)
	 (make-line-segment (car (rc-vertex-pixels (any-edge-u e)))
			    (car (rc-vertex-pixels (any-edge-v e)))))
	(remove-if-not dashed-edge? cycle))
   '#(0 255 0)
   output-file)
  (draw-coloured-lines
   (map (lambda (e)
	 (make-line-segment (car (rc-vertex-pixels (any-edge-u e)))
			    (car (rc-vertex-pixels (any-edge-v e)))))
	(remove-if-not solid-edge? cycle))
   '#(255 0 0)
   output-file)))

(define (rc-draw-cycle prefix cycle)
 (let ((output-file (string-append prefix "-cycle.ppm")))
  (system (format #f "convert ~a -depth 8 ~a" (string-append prefix ".pgm") output-file))
  (let ((image (read-pnm output-file)))
   (for-each
    (lambda (p)
     (matrix-set! (ppm-red image) (y p) (x p) 255)
     (matrix-set! (ppm-green image) (y p) (x p) 0)
     (matrix-set! (ppm-blue image) (y p) (x p) 0))
    (map-reduce append '() solid-edge-pixels
		(remove-if-not solid-edge? cycle)))
   (write-pnm image (string-append prefix "-cycle.ppm"))
   (draw-coloured-lines
    (map (lambda (e)
	  (make-line-segment (car (rc-vertex-pixels (any-edge-u e)))
			     (car (rc-vertex-pixels (any-edge-v e)))))
	 (remove-if-not dashed-edge? cycle))
    '#(0 255 0)
    (string-append prefix "-cycle.ppm")))))

(define (rc-draw-cycle-chains prefix cycle)
 (let ((image (ppm-constant 320 240 0 0 0)))
  (for-each
   (lambda (p) (matrix-set! (ppm-red image) (y p) (x p) 255))
   (map-reduce append '() solid-edge-pixels
	       (remove-if-not solid-edge? cycle)))
  (write-pnm image (string-append prefix "-cycle-chains.ppm"))))

;;; Distance Transform

(define (distance-transform pbm)
 ;; This does Manhattan distance.
 (let ((height (pnm-height pbm))
       (width (pnm-width pbm))
       (bitmap (pbm-bitmap pbm)))
  (let loop ((distance
	      (map-vector
	       (lambda (row)
		(map-vector (lambda (pixel) (if pixel 0 infinity)) row))
	       bitmap))
	     (closest
	      (map-n-vector
	       (lambda (i)
		(map-n-vector
		 (lambda (j) (if (matrix-ref bitmap i j) (vector j i) #f))
		 width))
	       height)))
   (let ((new-distance
	  (map-n-vector
	   (lambda (i)
	    (map-n-vector
	     (lambda (j)
	      (min (if (> i 0)
		       (+ (matrix-ref distance (- i 1) j) 1)
		       infinity)
		   (if (> j 0)
		       (+ (matrix-ref distance i (- j 1)) 1)
		       infinity)
		   (matrix-ref distance i j)
		   (if (< j (- width 1))
		       (+ (matrix-ref distance i (+ j 1)) 1)
		       infinity)
		   (if (< i (- height 1))
		       (+ (matrix-ref distance (+ i 1) j) 1)
		       infinity)))
	     width))
	   height))
	 (new-closest
	  (map-n-vector
	   (lambda (i)
	    (map-n-vector
	     (lambda (j)
	      (list-ref
	       (list (if (> i 0)
			 (matrix-ref closest (- i 1) j)
			 infinity)
		     (if (> j 0)
			 (matrix-ref closest i (- j 1))
			 infinity)
		     (matrix-ref closest i j)
		     (if (< j (- width 1))
			 (matrix-ref closest i (+ j 1))
			 infinity)
		     (if (< i (- height 1))
			 (matrix-ref closest (+ i 1) j)
			 infinity))
	       (position (min (if (> i 0)
				  (+ (matrix-ref distance (- i 1) j) 1)
				  infinity)
			      (if (> j 0)
				  (+ (matrix-ref distance i (- j 1)) 1)
				  infinity)
			      (matrix-ref distance i j)
			      (if (< j (- width 1))
				  (+ (matrix-ref distance i (+ j 1)) 1)
				  infinity)
			      (if (< i (- height 1))
				  (+ (matrix-ref distance (+ i 1) j) 1)
				  infinity))
			 (list (if (> i 0)
				   (+ (matrix-ref distance (- i 1) j) 1)
				   infinity)
			       (if (> j 0)
				   (+ (matrix-ref distance i (- j 1)) 1)
				   infinity)
			       (matrix-ref distance i j)
			       (if (< j (- width 1))
				   (+ (matrix-ref distance i (+ j 1)) 1)
				   infinity)
			       (if (< i (- height 1))
				   (+ (matrix-ref distance (+ i 1) j) 1)
				   infinity)))))
	     width))
	   height)))
    (if (every-vector v= distance new-distance)
	(list distance closest)
	(loop new-distance new-closest))))))

(define (closest-transform-ref closest-transform p)
 (matrix-ref closest-transform (y p) (x p)))

(define (euclidean-1d-dt v)
 (let* ((n (vector-length v))
	(c-v (malloc (* c-sizeof-double n)))
	(c-v (vector->c-inexact-array c-v v c-sizeof-double #t))
	(c-dt ((c-function pointer ("euclidean_1d_dt" pointer unsigned)) c-v n))
	(dt (c-inexact-array->vector c-dt c-sizeof-double (* 2 n) #t)))
  (free c-dt)
  (free c-v)
  (shape-matrix dt n)))

(define (euclidean-2d-dt m)
 (let* ((row-dt (map-vector (lambda (row) (euclidean-1d-dt row)) m))
	(row-dt-val (map-vector x row-dt))
	(row-dt-pos (map-matrix inexact->exact (map-vector y row-dt)))
	(column-dt (map-vector (lambda (col) (euclidean-1d-dt col)) (transpose row-dt-val)))
	(column-dt-val (transpose (map-vector x column-dt)))
	(column-dt-pos (map-matrix inexact->exact (map-vector y column-dt))))
  `#(,column-dt-val
     ,(transpose
       (map-indexed-vector
	(lambda (r j) (map-vector (lambda (i) `#(,i ,(vector-ref (vector-ref row-dt-pos i) j))) r))
	column-dt-pos)))))

(define (euclidean-3d-dt m)
 (define (along-time m) (map-vector transpose (transpose m)))
 (define (unalong-time m) (transpose (map-vector transpose m)))
 (let* ((ts (map-vector euclidean-2d-dt m))
	(s-dist (map-vector x ts))
	(s-pos (map-vector y ts))
	(dt-3d (map-matrix euclidean-1d-dt (along-time s-dist))))
  (make-voc4-dt
   (unalong-time (map-matrix x dt-3d))
   (map-vector
    (lambda (t) (map-indexed-matrix
	    (lambda (v i j)
	     (map-vector
	      inexact->exact
	      (append-vector
	       `#(,v) (matrix-ref (vector-ref s-pos (inexact->exact v)) i j))))
	    t))
    (unalong-time (map-matrix y dt-3d)))
   '())))

(define-structure voc4-dt distance position ref)

(define (get-voc4-dt boxes costs transformation scale)
 (let ((m (make-matrix 720 1280 1e20))
       (ref '()))
  (for-each
   (lambda (box cost)
    (let ((c (map-vector
	      exact-round
	      (voc4-detection-center
               (forward-project-box-scaled box transformation 0
                                           scale)))))
     (matrix-set! m (y c) (x c) cost)
     (set! ref (cons (list c box) ref))))
   boxes costs)
  (let ((dt (euclidean-2d-dt m)))
   (make-voc4-dt (x dt) (y dt) ref))))

;;; Color space conversions

(define (rgb->xyz c)
 (define (adjust c)
  (let ((c (/ c 255)))
   (* 100 (if (> c 0.04045)
	      (expt (/ (+ c 0.055) 1.055) 2.4)
	      (/ c 12.92)))))
 (let ((r (adjust (r c))) (g (adjust (g c))) (b (adjust (b c))))
  ;; Observer. = 2 degrees, Illuminant = D65
  `#(,(+ (* r 0.4124) (* g 0.3576) (* b 0.1805))
     ,(+ (* r 0.2126) (* g 0.7152) (* b 0.0722))
     ,(+ (* r 0.0193) (* g 0.1192) (* b 0.9505)))))

(define (xyz->rgb c)
 (let ((c (map-vector (lambda (c) (/ c 100)) c)))
  (let ((r (+ (* (x c)  3.2406) (* (y c) -1.5372) (* (z c) -0.4986)))
	(g (+ (* (x c) -0.9689) (* (y c)  1.8758) (* (z c)  0.0415)))
	(b (+ (* (x c)  0.0557) (* (y c) -0.2040) (* (z c)  1.0570))))
   (define (adjust c)
    (exact-round
     (* 255 (if (> c 0.0031308)
		(+ (* 1.055 (expt c (/ 2.4))) -0.055)
		(* 12.92 c)))))
   ;; Observer. = 2 degrees, Illuminant = D65
   (map-vector adjust `#(,r ,g ,b)))))

(define (xyz->l*ab c)
 (define (adjust c)
  (if (> c 0.008856)
      (expt c (/ 3))
      (+ (* 7.787 c) (/ 16 116))))
 (let ((x (adjust (/ (x c)  95.047)))
       (y (adjust (/ (y c) 100.000)))
       (z (adjust (/ (z c) 108.883))))
  ;; Observer. = 2 degrees, Illuminant = D65
  `#(,(- (* 116 y) 16)
     ,(* 500 (- x y))
     ,(* 200 (- y z)))))

(define (l*ab->xyz c)
 (define (adjust c)
  (if (> (expt c 3) 0.008856)
      (expt c 3)
      (/ (- c (/ 16 116)) 7.787)))
 (let* ((cy (/ (+ 16 (x c)) 116))
	(cx (+ (/ (y c) 500) cy))
	(cz (- cy (/ (z c) 200))))
  ;; Observer. = 2 degrees, Illuminant = D65
  `#(,(* (adjust cx)  95.047)
     ,(* (adjust cy) 100.000)
     ,(* (adjust cz) 108.883))))

(define (rgb->l*ab c) (xyz->l*ab (rgb->xyz c)))
(define (l*ab->rgb c) (xyz->rgb (l*ab->xyz c)))
