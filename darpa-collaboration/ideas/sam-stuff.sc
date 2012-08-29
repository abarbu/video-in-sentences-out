(MODULE
  SAM-STUFF
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


(include "QobiScheme.sch")
(include "sam-stuff.sch")

(set! *program* "sam-stuff")
(set! *panic?* #f)





(define (compare-superpixels ppm1 labeling1 ppm2 labeling2 output-dir)
 (for-each-n
  (lambda (n)
   (let ((tmp-ppm1 (pnm-copy ppm1))
	 (tmp-ppm2 (pnm-copy ppm2)))
    (for-each-n-matrix
     (lambda (yy xx)
      (when (= (matrix-ref labeling1 yy xx) n)
       (matrix-set! (ppm-blue tmp-ppm1) yy xx 255)
       (matrix-set! (ppm-red tmp-ppm1) yy xx 0)
       (matrix-set! (ppm-green tmp-ppm1) yy xx 0))
      (when (= (matrix-ref labeling2 yy xx) n)
       (matrix-set! (ppm-blue tmp-ppm2) yy xx 255)
       (matrix-set! (ppm-red tmp-ppm2) yy xx 0)
       (matrix-set! (ppm-green tmp-ppm2) yy xx 0)))
     (matrix-rows labeling1) (matrix-columns labeling1))
    (write-pnm (ppm-stack-horizontal tmp-ppm1 tmp-ppm2)
	       (format #f "~a/sp-match~a.ppm" output-dir n))
    (display (format #f "superpixel ~a" n))
    (newline)))
  (+ (maximum-matrix labeling1) 1)))


(define (random-rgb)
 (vector
  (random-integer *max-red*)
  (random-integer *max-green*)
  (random-integer *max-blue*)))

(define (superpixel-centers labeling)
 (let ((centers (map-n-vector (lambda (n) '()) (+ (maximum-matrix labeling) 1))))
  (for-each-indexed-matrix
   (lambda (l yy xx)
    (vector-set! centers l (cons (vector xx yy) (vector-ref centers l))))
   labeling)
  (map-vector
   (lambda (pixels)
    (map-vector
     (lambda (coord)
      (/ coord (length pixels)))
     (reduce v+ pixels (vector 0 0))))
   centers)))


(define (superpixel-velocities labeling1 labeling2)
 (unless (= (maximum-matrix labeling1) (maximum-matrix labeling2))
  (fuck-up))
 (map-vector
  (lambda (c1 c2)
   (v- c2 c1))
  (superpixel-centers labeling1)
  (superpixel-centers labeling2)))

(define-command
 (main (exactly-one ("first" frame1? (frame1 "first-frame-pathname" string-argument "")))
       (exactly-one ("second" frame2? (frame2 "second-frame-pathname" string-argument "")))
       (exactly-one ("mapping" mapping? (mapping-filename "sp# to color mapping" string-argument ""))))
 (let* ((first-frame (read-pnm frame1))
	(first-labeling (read-object-from-file (replace-extension frame1 "dat")))
	(second-frame (read-pnm frame2))
	(second-labeling (read-object-from-file (replace-extension frame2 "dat")))
	(width (pnm-width first-frame))
	(height (pnm-height first-frame))
	(mapping (if (file-exists? mapping-filename)
		     (read-object-from-file mapping-filename)
		     (map-n
		      (lambda (n) (vector n (random-rgb)))
		      (+ (maximum-matrix first-labeling) 1))))
	(first-centers (superpixel-centers first-labeling))
	(velocities (superpixel-velocities first-labeling second-labeling))
        (image (make-ppm #t 255
						   (make-matrix height width 0)
						   (make-matrix height width 0)
						   (make-matrix height width 0)))
        (image2 (make-ppm #t 255
						   (make-matrix height width 0)
						   (make-matrix height width 0)
						   (make-matrix height width 0))))
	(for-each-n-matrix
	 (lambda (yy xx)
	  (set-ppm-pixel! image xx yy (y (find-if (lambda (n) (= (matrix-ref first-labeling yy xx) (x n)))
					       mapping)))
	  (set-ppm-pixel! image2 xx yy (y (find-if (lambda (n) (= (matrix-ref second-labeling yy xx) (x n)))
					       mapping))))
	 height width)
       (for-each-n
	(lambda (n)
	 (let ((c (vector-ref first-centers n)))
	 (for-each (lambda (p) (set-ppm-pixel! image (x p) (y p) '#(255  255 255)))
		   (quantize-points
		    (line-segment->points (make-line-segment c (v+ c (vector-ref velocities n))))))
	 (set-ppm-pixel! image (quantize-coordinate (x c)) (quantize-coordinate (y c)) '#(0 0 0))))
	(+ (maximum-matrix first-labeling) 1))
  (write-pnm image (replace-extension frame1 "_slic-colored.ppm"))
  (write-pnm image2 (replace-extension frame2 "_slic-colored.ppm"))
  (write-object-to-file mapping mapping-filename)))
