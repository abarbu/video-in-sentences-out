;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "idea9.sch")

(set! *program* "idea9")
(set! *panic?* #f)

(define *previous-motion-box* #f)

(define (line->boxes line)
 (let loop ((l (map string->number (fields line))))
  (if (null? l)
      '()
      (cons (take 6 l) (loop (drop 6 l)))))) 

(define (safe-read-boxes video-name frame type label ext)
 (let ((filename (generic-full-pathname
		  *video-pathname* video-name
		  (format #f "/~a/~a-~a.~a"
			  (number->padded-string-of-length frame 4)
			  type
			  label
			  ext))))
  (if (file-exists? filename)
      (read-file filename)
      (begin (format #t "~a cannot be read!~%" filename) ""))))

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

(define (write-boxes boxes pathname)
 (write-file
  (map
   (lambda (f)
    (map-reduce string-append
		""
		(lambda (e) (string-append (number->string e) " "))
		f))
   boxes)
  pathname))

(define (smooth-boxes boxes)
 (scheme->matlab! "boxes" (map list->vector boxes))
 (matlab-eval-string
  (string-append "addpath('~/darpa-collaboration/splines');"
		 "box_centre = @(x) [(x(1,1)+x(1,3))/2 (x(1,2)+x(1,4))/2]; "
		 "centres = cell2mat(cellfun(box_centre,num2cell(boxes,2),'UniformOutput',false)); "
		 "ts = [1:5:length(boxes)*5]; "
		 "ppx = splinefit(ts,centres(:,1),5,4); "
		 "ppy = splinefit(ts,centres(:,2),5,4); "
		 "smx = ppval(ppx,ts); "
		 "smy = ppval(ppy,ts); "
		 "ppw = splinefit(ts,abs(boxes(:,1)-boxes(:,3)),10,4); "
		 "pph = splinefit(ts,abs(boxes(:,2)-boxes(:,4)),10,4); "
		 "smw = ppval(ppw,ts); "
		 "smh = ppval(pph,ts); "))
 (let ((xs (x (matlab-get-variable "smx")))
       (ys (x (matlab-get-variable "smy")))
       (ws (x (matlab-get-variable "smw")))
       (hs (x (matlab-get-variable "smh"))))
  (vector->list
   (map-vector
    ;; fill up filter and confidence with dummies
    (lambda (x y w h) `(,(exact-round (- x (/ w 2)))
		   ,(exact-round (- y (/ h 2)))
		   ,(exact-round (+ x (/ w 2)))
		   ,(exact-round (+ y (/ h 2)))
		   -1
		   -1000))
    xs ys ws hs))))

(define (dtracef f s v) (format #t "~a: ~a~%" s (f v)) v)
(define i 1)

(define (find-connected-components ppm1 ppm2 dilation threshold area-factor)
 (scheme->matlab! "img1" (ppm->pgm ppm1))
 (scheme->matlab! "img2" (ppm->pgm ppm2))
 (matlab-eval-string
  (string-append
   "img = imabsdiff(img1,img2); "
   ;; (format #f "i = imdilate(im2bw(img, ~a), strel('disk', ~a)); "
   ;; 	   (/ threshold (ppm-maxval ppm1)) dilation)
   (format #f "i = im2bw(img, ~a); "
	   (/ threshold (ppm-maxval ppm1))) 
   (format #f "i2 = bwareaopen(i , ~a, 8); "
	   (exact-round (* area-factor (pnm-height ppm1) (pnm-width ppm1))))
   (format #f "imwrite(i2,'/tmp/img-~a.pgm'); " i)
   "stats = regionprops(bwlabel(i2),'all'); "
   "pixels = [stats.PixelList]; "
   "len = length(pixels); "
   "bboxes = [stats.BoundingBox]; "))
 (set! i (+ i 1))
 (matlab-show-variable "len")
 (matlab-show-variable "bboxes")
 (let* ((points (vector->list (x (matlab-get-variable "pixels"))))
	(box (vector->list
	      (map-vector
	       vector->list
	       (shape-matrix (x (matlab-get-variable "bboxes")) 4))))
	(points-box (zip (vector->list (x (matlab-get-variable "pixels")))
			(vector->list
			 (map-vector
			  vector->list
			  (shape-matrix (x (matlab-get-variable "bboxes")) 4))))))
  (first points-box)))

(define (find-motion-box1 prev this threshold dilation area-factor)
 (append (second (find-connected-components prev this dilation threshold area-factor))
	 '(-1 -1000)))

(define (find-motion-box prev this threshold dilation area-factor)
 (let* ((connected-comps
	 (remove-if
	  (lambda (points) (< (length points) (* area-factor (pnm-height prev) (pnm-width prev))))
	  (map (lambda (g) (pbm->points (graph->pbm g (pnm-height prev) (pnm-width prev))))
	       (connected-components
		(pbm->graph
		 (dtracef
		  (lambda (e) (write-pnm (pbm->pgm e)
				    (format #f "/tmp/img-~a.pgm" (number->padded-string-of-length i 4)))
		     (set! i (+ i 1)))
		  "write"
		  (pbm-bloat (pgm->pbm (ppm-absolute-difference prev this) threshold) dilation))
		 2))))))
  (unless (null? connected-comps)
   (format #t "cc: ~a~%" (length connected-comps))
   (let* ((motion-pixels (concat connected-comps))
	  (min-x (x (first (sort motion-pixels < x))))
	  (min-y (y (first (sort motion-pixels < y))))
	  (max-x (x (first (sort motion-pixels > x))))
	  (max-y (y (first (sort motion-pixels > y)))))
    (set! *previous-motion-box* `(,min-x ,min-y ,max-x ,max-y -1 -1000))))
  *previous-motion-box*))


(define (find-motion-boxes boxes largest-box video-name box-bloat threshold dilation area-factor)
 (let* ((largest-box-centre (box-centre largest-box)))
  (for-each-frame-pair
   (lambda (frame) 
    (let* ((bc (box-centre (list-ref boxes (- frame 1))))
	   (disp (v- bc largest-box-centre)))
     (vector
      frame
      (read-pnm (ppm-pathname video-name frame)))))
   (lambda (prev this)
    (format #t "frame ~a~%" (x prev))
    (dtrace "b"(find-motion-box (y prev) (y this) threshold dilation area-factor)))
   video-name)))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (exactly-one ("box-type" box-type? (box-type "name" string-argument "voc4")))
       (exactly-one ("box-label" box-label? (box-label "name" string-argument "person")))
       (exactly-one ("box-extension" box-extension? (box-extension "name" string-argument "smooth_tracked_box")))
       (at-most-one ("box-bloat" box-bloat? (box-bloat "r" real-argument 0.1)))
       (at-most-one ("threshold" threshold? (threshold "n" integer-argument 50)))
       (at-most-one ("dilation" dilation? (dilation "n" integer-argument 10)))
       (at-most-one ("area-factor" area-factor? (area-factor "r" real-argument 0.5))))
 (set! *engine* (matlab-start *default-matlab-engine-command*))
 (set-maximum-heap! (* 512 1024 1024))
 (let* ((video-name (cond (standard?
			   (standard-corpus-video corpus sequence person location n))
			  (darpa? (string->darpa-video name))
			  (else (fuck-up))))
	(boxes (map-frame
		(lambda (frame)
		 (first
		  (map-concat
		   line->boxes
		   (safe-read-boxes
		    video-name frame box-type (string-append box-label "-1") box-extension))))
		video-name))
	(dummy (set! *previous-motion-box* (first boxes)))
	(largest-box (bloat-box (first (sort boxes > box-area)) box-bloat))
	(motion-boxes (find-motion-boxes boxes largest-box video-name box-bloat threshold dilation area-factor))
	(smoothed-boxes (smooth-boxes motion-boxes)))
  (for-each-frame
   (lambda (frame)
    (write-boxes (list (list-ref motion-boxes (- frame 1)))
		 (generic-full-pathname
		  *video-pathname* video-name
		  (format #f "/~a/~a-~a.~a"
			  (number->padded-string-of-length frame 4)
			  "motion"
			  (string-append box-label "-1")
			  "tracked_box")))
    (write-boxes (list (list-ref smoothed-boxes (- frame 1)))
		 (generic-full-pathname
		  *video-pathname* video-name
		  (format #f "/~a/~a-~a.~a"
			  (number->padded-string-of-length frame 4)
			  "motion"
			  (string-append box-label "-1")
			  "smooth_tracked_box"))))
   video-name)))
