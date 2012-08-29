(MODULE
  IDEA8
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
(include "idea8.sch")

(set! *program* "idea8")
(set! *panic?* #f)

(define (ppm-absolute-difference1 ppm1 ppm2)
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

(define (ppm-absolute-difference2 ppm1 ppm2)
 (pgm-absolute-difference (ppm->pgm ppm1) (ppm->pgm ppm2)))

;; (pbm-or
;;  (pgm->pbm (ppm-absolute-difference1 previous this) threshold1)
;;  (pgm->pbm (ppm-absolute-difference1 this next) threshold1))

;; (format #f
;; 	"/tmp/motion-masks/~a/frame-~a.ppm"
;; 	(darpa-video->name v)
;; 	(number->padded-string-of-length f 4))

(define (select-best-hypothesis-mask v area-thresh)
 (for-each-frame-pair
  (lambda (f)
   (vector f (read-pnm (format #f
			       "/tmp/darpa/~a/frame-cropped-~a.ppm"
			       (darpa-video->name v)
			       (number->padded-string-of-length f 4)))))
  (lambda (prev this)
   (format #t "Frame ~a->~a~%" (x prev) (x this))
   (let* ((motion-mask
	   (points->pbm
	    (concat
	     (remove-if
	      (lambda (points) (< (length points) area-thresh))
	      (map (lambda (g) (pbm->points (graph->pbm g (pnm-height (y prev)) (pnm-width (y prev)))))
		   (connected-components
		    (pbm->graph (pgm->pbm (ppm-absolute-difference2 (y prev) (y this)) 50) 5)))))
	    (pnm-height (y prev))
	    (pnm-width (y prev))))
	  (hyp-scores
	   (map
	    (lambda (fn)
	     (let ((pbm1 (ppm->pbm (x (video-file->pnm-movie fn "Jpeg")) 50)))
	      (format #t "pbm1 ~a ~a~%mm ~a ~a~%r1 ~a~%r2 ~a~%"
		      (pnm-height pbm1) (pnm-width pbm1) (pnm-height motion-mask) (pnm-width motion-mask)
		      (pbm-raw? pbm1) (pbm-raw? motion-mask)
		      )
	      `(,fn ,(length (pbm->points
			      (pbm-xor (ppm->pbm (x (video-file->pnm-movie fn "Jpeg")) 50)
				       motion-mask))))))
	    (dtrace "l"(directory-list (format #f "/tmp/darpa/~a/ResultsClosurett/frame-cropped-~a_solution_*.jpg"
					       (darpa-video->name v)
					       (number->padded-string-of-length (x this) 4)))))))
    (format #t "~a~%" (first (first (sort hyp-scores < second))))
    (write-pnm
     (pbm->pgm motion-mask)
     (format #f "/tmp/motion-masks/~a/frame-~a.pgm" (darpa-video->name v) (number->padded-string-of-length (x this) 4)))))
  v))

(define (line->boxes line)
 (let loop ((l (map string->number (fields line))))
  (if (null? l)
      '()
      (cons (take 6 l) (loop (drop 6 l))))))

(define (box->points box)
 (line-segments->points
  (list (make-line-segment `#(,(second box) ,(first box))
			   `#(,(fourth box) ,(first box)))
	(make-line-segment `#(,(fourth box) ,(first box))
			   `#(,(fourth box) ,(third box)))
	(make-line-segment `#(,(fourth box) ,(third box))
			   `#(,(second box) ,(third box)))
	(make-line-segment `#(,(second box) ,(third box))
			   `#(,(second box) ,(first box))))))

(define (safe-read-boxes video-name name frame ext)
 (let ((filename (generic-full-pathname
		  *video-pathname* video-name
		  (format #f "/~a/voc4-~a.~a"
			  (number->padded-string-of-length frame 4)
			  name
			  ext))))
  (if (file-exists? filename)
      (read-file filename)
      (begin (format #t "~a cannot be read!~%" filename) '()))))

(define (box-area box)
 (* (abs (- (first box) (third box))) (abs (- (second box) (fourth box)))))

(define (box-centre box)
 (map-vector
  exact-round
  (k*v 0.5 (v+ `#(,(first box) ,(second box)) `#(,(third box) ,(fourth box))))))

(define (crop-extrapolate m x y w h)
 (crop (map-vector
	(lambda (row)
	 (vector-append (if (< x 0)
			  (make-vector (- 0 x) (vector-ref row 0))
			 '#())
			row
			(if (>= (+ x w) (vector-length row))
			  (make-vector (- (+ x w) (- (vector-length row) 1))
				       (vector-ref row (- (vector-length row) 1)))
			 '#())))
	(vector-append (if (< y 0)
			 (make-vector (- 0 y) (vector-ref m 0))
			'#())
		       m
		       (if (>= (+ y h) (vector-length m))
			 (make-vector (- (+ y h) (- (vector-length m) 1))
				      (vector-ref m (- (vector-length m) 1)))
			'#())))
       (if (< x 0) 0 x) (if (< y 0) 0 y) w h))

(define (crop-extrapolate-image pnm x y width height)
 (cond ((pbm? pnm) (make-pbm #f (crop-extrapolate (pbm-bitmap pnm) x y width height)))
       ((pgm? pnm) (make-pgm (pgm-raw? pnm)
			     (pgm-maxval pnm)
			     (crop-extrapolate (pgm-grey pnm) x y width height)))
       ((ppm? pnm) (make-ppm (ppm-raw? pnm)
			     (ppm-maxval pnm)
			     (crop-extrapolate (ppm-red pnm) x y width height)
			     (crop-extrapolate (ppm-green pnm) x y width height)
			     (crop-extrapolate (ppm-blue pnm) x y width height)))
       (else (panic "Image must be one of PBM, PGM or PPM"))))

(define (bloat-box box p)
 (let ((del-h (exact-round (* p (- (third box) (first box)))))
       (del-w (exact-round (* p (- (fourth box) (second box))))))
  `(,(- (first box) del-h) ,(- (second box) del-w)
    ,(+ (third box) del-h) ,(+ (fourth box) del-w))))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (exactly-one ("box-label" box-label? (box-label "name" string-argument "person")))
       (at-most-one ("cc-threshold" cc-threshold?
		     (cc-threshold "n" integer-argument 100))))
 (let* ((video-name (cond (standard?
			   (standard-corpus-video corpus sequence person location n))
			  (darpa? (string->darpa-video name))
			  (else (fuck-up)))) 
	(boxes (map-m-n
		(lambda (frame)
		 (format #t "~a~%" frame)
		 (first
		  (map-concat line->boxes
			      (read-file (string-append "/tmp/boxes/voc4-"
							box-label
							"-1-"
							(number->padded-string-of-length frame 4)
							".smooth_tracked_box")))))
		1
		19))
	(largest-box (dtrace "a"(first (sort boxes > box-area))))
	(largest-box-centre (dtrace "bc"(box-centre largest-box))))
  (system (format #f "mkdir -p /tmp/motion-masks/~a" name))
  (for-each-m-n
   (lambda (frame)
    (let* ((bc (box-centre (list-ref boxes (- frame 1))))
	   (disp (v- bc largest-box-centre)))
     (write-pnm
      (crop-extrapolate-image
       (read-pnm (ppm-pathname video-name frame))
       (exact-round (+ (first largest-box) (x disp)))
       (exact-round (+ (second largest-box) (y disp)))
       (exact-round (- (third largest-box) (first largest-box)))
       (exact-round (- (fourth largest-box) (second largest-box))))
      (format #f "/tmp/motion-masks/~a/frame-~a.ppm" name (number->padded-string-of-length frame 4)))))
   1
   19)
  (select-best-hypothesis-mask video-name cc-threshold)))
