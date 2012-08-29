(MODULE
  MOTION-WEIGH
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

(include "QobiScheme-AD.sch")
(include "motion-weigh.sch")

(set! *program* "motion-weigh")
(set! *panic?* #t)

(define (matrix-sum-elements m)
 (map-reduce-vector + 0 (lambda (r) (reduce-vector + r 0)) m))

(define (weight-function mat)
 (let ((centre `#(,(floor (/ (matrix-rows mat) 2)) ,(floor (/ (matrix-columns mat) 2)))))
  (/ (matrix-sum-elements
      (map-indexed-vector
       (lambda (r i)
	(map-indexed-vector
	 (lambda (v j)
	  (let ((dist (distance centre `#(,i ,j)))) (if (zero? dist) v (/ v dist))))
	 r))
       mat))
     (* (matrix-rows mat) (matrix-columns mat)))))

(define (motion-weigh edge motion half-win)
 (let ((chains
	(remove-if
	 (lambda (c) (< (length c) 2))
	 (map-reduce append '() (lambda (c) (break-chain c 20)) (pbm->chains (pgm->pbm edge 1)))))
       (motion-pixmap (pgm-grey motion)))
  (map
   (lambda (chain)
    `(,chain
      ,(/ (map-reduce
	   +
	   0
	   (lambda (p)
	    (if (or (< (- (x p) half-win 1) 0) (< (- (y p) half-win 1) 0)
		    (>= (+ (x p) half-win) (pnm-width motion))
		    (>= (+ (y p) half-win) (pnm-height motion)))
		0
		(weight-function
		 (map-vector
		  (lambda (v) (subvector v (- (x p) half-win 1) (+ (x p) half-win)))
		  (subvector motion-pixmap (- (y p) half-win 1) (+ (y p) half-win))))))
	   chain)
	  (length chain)))
    )
   chains)))

(define (weighted-chains->pgm wchains height width)
 (let ((m (make-matrix height width 0))
       (maxval (first (sort (map second wchains) > identity))))
  (for-each
   (lambda (wchain)
    (for-each
     (lambda (p)
      (matrix-set!
       m
       (y p)
       (x p)
       (exact-round (* (if (= maxval 0) 0 (/ (second wchain) maxval)) 255))))
     (first wchain)))
   wchains)
  (make-pgm #t 255 m)))

(define-command (main (exactly-one ("standard" standard?
				    (corpus "corpus" string-argument "")
				    (sequence "sequence" string-argument "")
				    (person "person" string-argument "")
				    (location "location" string-argument "")
				    (n "n" integer-argument 0))
				   ("darpa" darpa? (name "name" string-argument "")))
		      (at-most-one ("image" image?))
		      (optional (break-size "chain break-size" integer-argument 20))
		      (optional (half-win-size "half-win-size" integer-argument 5))
		      (optional (start "start" integer-argument #f))
		      (optional (end "end" integer-argument #f)))
 (let* ((video-name (cond (standard?
			   (standard-corpus-video corpus sequence person location n))
			  (darpa? (string->darpa-video name))
			  (else (fuck-up))))
	(start (if start start (video-first-frame video-name)))
	(end (if end end (- (video-length video-name) (if darpa? 1 0)))))
  (for-each-m-n
   (lambda (frame)
    (let* ((berkeley-image (read-pnm (berkeley-pathname video-name frame)))
	   (weighted-chains
	    (motion-weigh berkeley-image
			  (read-pnm (motion-pathname video-name frame))
			  half-win-size)))
     (write-object-to-file
      weighted-chains
      (motion-weighted-berkeley-chains-pathname video-name frame))
     (when image?
      (write-pnm
       (weighted-chains->pgm weighted-chains (pnm-height berkeley-image) (pnm-width berkeley-image))
       (motion-weighted-berkeley-image-pathname video-name frame)))
     (format #t "~a~%" (motion-weighted-berkeley-chains-pathname video-name frame))))
   start
   end)))
