;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "generate-cropped-images.sch")

(set! *program* "generate-cropped-images")
(set! *panic?* #f)

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
       (exactly-one ("box-extension" box-extension? (box-extension "name" string-argument "smooth_tracked_box")))
       (at-most-one ("bloat" bloat? (bloat "multiple" real-argument 0.0))))
 (let* ((video-name (cond (standard?
			   (standard-corpus-video corpus sequence person location n))
			  (darpa? (string->darpa-video name))
			  (else (fuck-up)))) 
	(boxes (map-m-n
		(lambda (frame)
		 (format #f "~a~%" frame)
		 (first
		  (map-concat line->boxes
			      (safe-read-boxes video-name (string-append box-label "-1") frame box-extension))))
		1
		(video-last-frame video-name)))
	(largest-box (bloat-box (first (sort boxes > box-area)) bloat))
	(largest-box-centre (box-centre largest-box)))
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
      (generic-full-pathname
       *video-pathname* video-name
       (format #f "/~a/frame-cropped-~a-1.ppm"
	       (number->padded-string-of-length frame 4)
	       box-label)))))
   1
   (video-last-frame video-name))))
