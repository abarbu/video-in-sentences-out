(MODULE IDEALIB-TRACKS)

;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2012 Purdue University. All rights reserved.

(include "QobiScheme-AD.sch")
(include "idealib-tracks.sch")
(include "toollib-c-macros.sch")

(c-include "idealib-c.h")

(c-define-struct-field "box_line_t" "values" pointer)
(c-define-struct-field "box_line_t" "nr_values" int)
(c-define-struct-field "box_line_t" "name" pointer)
(c-define-struct-field "box_line_t" "next" pointer)

;;; Voc4

(define (nondropped-box? box)
 (not (= (voc4-detection-strength box) minus-infinity)))

(define (dropped-box? box)
 (= (voc4-detection-strength box) minus-infinity))

(define (number->string-c-infinity number)
 (cond ((= infinity number) "infinity")
       ((= (- infinity) number) "-infinity")
       (else (number->string number))))

(define (voc4-box->string box)
 (if (nondropped-box? box)
     (format #f "~s ~s ~s ~s ~a ~s ~a ~s ~a ~a"
	     (voc4-detection-x1 box)
	     (voc4-detection-y1 box)
	     (voc4-detection-x2 box)
	     (voc4-detection-y2 box)
	     (string-join " "
			  (map (lambda (l) (string-join " " (map number->string l)))
			       (voc4-detection-parts box)))
	     (voc4-detection-filter box)
	     (number->string-c-infinity (voc4-detection-strength box))
	     (voc4-detection-delta box)
	     (number->string-c-infinity (voc4-detection-ringing box))
	     (if (voc4-detection-model box)
		 (voc4-detection-model box)
		 "padding"))
     (format #f "~s ~s ~s ~s ~s ~a ~s ~a ~a"
	     -1 -1 -1 -1
	     (voc4-detection-filter box)
	     (number->string-c-infinity (voc4-detection-strength box))
	     (voc4-detection-delta box)
	     (number->string-c-infinity minus-infinity)
	     (if (voc4-detection-model box)
		 (voc4-detection-model box)
		 "padding"))))

(define (crop-track-from-video video-name model number destination)
 (crop-box-movie-from-video
  video-name
  (read-voc4-tracked-boxes video-name model number)
  destination))

(define (crop-box-movie-from-video video-name box-movie destination)
 (with-ffmpeg-video
  video-name
  (lambda (ffmpeg-video)
   (for-each-indexed
    (lambda (box frame-number)
     (write-pnm
      (crop-voc4 (ffmpeg-video-frame-data ffmpeg-video)
		 (exact-round-voc4-detection box))
      (string-append destination
		     (number->padded-string-of-length
		      (+ frame-number (video-first-frame video-name))
		      6)
		     ".ppm"))
     (ffmpeg-next-frame! ffmpeg-video))
    box-movie))))

(define (write-boxes-movie boxes-movie video-name type label num ext)
 (let ((zip-filename (per-video-box-pathname video-name type label num ext)))
  ;; TODO This is a hack for now, need better handling of errors in
  ;; zip:* functions
  (rm-if-necessary zip-filename)
  (with-zip-file
   (lambda (zip2)
    (for-each-frame-indexed
     (lambda (frame offset)
      (zip:add-directory zip2 (number->padded-string-of-length
			       frame
			       6))
      (zip:add-file zip2
		    (per-frame-boxes-in-zip-pathname frame type label num ext)
		    (map voc4-box->string (list-ref boxes-movie offset))))
     video-name))
   zip-filename *zip:mode-create-new*)))

(define (write-box-movie box-movie video-name type label num ext)
 (write-boxes-movie (map list box-movie) video-name type label num ext))

(define (write-voc4-boxes-movie boxes-movie video-name model)
 (write-boxes-movie boxes-movie video-name "voc4" model #f "boxes"))

(define (write-voc4-irobot-boxes-movie boxes-movie video-name model)
 (write-boxes-movie boxes-movie video-name "voc4_irobot" model #f "boxes"))

(define (write-voc4-predicted-boxes-movie boxes-movie video-name model)
 (write-boxes-movie boxes-movie video-name "voc4" model #f "predicted-boxes"))

(define (write-voc4-tracked-box-movie box-movie video-name model number)
 (write-box-movie box-movie video-name "voc4" model number "tracked-box"))

(define (write-voc4-smooth-tracked-box-movie box-movie video-name model number)
 (write-box-movie box-movie video-name "voc4" model number "smooth-tracked-box"))

(define (write-voc4-overgenerated-boxes-movie boxes-movie video-name model)
 (write-boxes-movie boxes-movie video-name "voc4_overgenerated" model #f "boxes"))

(define (write-voc4-overgenerated-predicted-boxes-movie boxes-movie video-name model)
 (write-boxes-movie boxes-movie video-name "voc4_overgenerated" model #f "predicted-boxes"))

(define (write-voc4-overgenerated-tracked-box-movie box-movie video-name model number)
 (write-box-movie box-movie video-name "voc4_overgenerated" model number "tracked-box"))

(define (write-voc4-overgenerated-smooth-tracked-box-movie box-movie video-name model number)
 (write-box-movie box-movie video-name "voc4_overgenerated" model number "smooth-tracked-box"))

(define (write-voc4-event-boxes-movie boxes-movie video-name model)
 (write-boxes-movie boxes-movie video-name "voc4_event" model #f "boxes"))

(define (write-voc4-event-predicted-boxes-movie boxes-movie video-name model)
 (write-boxes-movie boxes-movie video-name "voc4_event" model #f "predicted-boxes"))

(define (write-voc4-event-tracked-box-movie box-movie video-name model number)
 (write-box-movie box-movie video-name "voc4_event" model number "tracked-box"))

(define (write-voc4-event-smooth-tracked-box-movie box-movie video-name model number)
 (write-box-movie box-movie video-name "voc4_event" model number "smooth-tracked-box"))

(define (split-n n l)
 (if (null? l) l (cons (take n l) (split-n n (drop n l)))))

(define (quantize-voc4-detection detection)
 (make-voc4-detection
  (quantize-coordinate (voc4-detection-x1 detection))
  (quantize-coordinate (voc4-detection-y1 detection))
  (quantize-coordinate (voc4-detection-x2 detection))
  (quantize-coordinate (voc4-detection-y2 detection))
  (voc4-detection-parts detection)
  (voc4-detection-filter detection)
  (voc4-detection-strength detection)
  (voc4-detection-delta detection)
  (voc4-detection-ringing detection)
  (voc4-detection-model detection)))

(define (unpack-consolidated-frames data)
 (let loop ((data data) (out '()))
  (cond ((null? data) (reverse out))
	((equal? "" (first data)) (loop (cdr data) out))
	(else (loop (drop (string->number (first data)) (cdr data))
		    (cons (take (string->number (first data)) (cdr data))
			  out))))))

(define (vector-string->voc4-inexact-box e string . default-model)
 (make-voc4-detection
  (vector-ref e 0) (vector-ref e 1)
  (vector-ref e 2) (vector-ref e 3)
  (let loop ((i 4))
   (if (<= (- (vector-length e) i) 4)
       '()
       (cons (list (vector-ref e i) (vector-ref e (+ i 1))
		   (vector-ref e (+ i 2)) (vector-ref e (+ i 3)))
	     (loop (+ i 4)))))
  (exact-round
   (let ((a (vector-ref e (- (vector-length e) 4))))
    (if (or (= a infinity) (= a minus-infinity)) -1 a)))
  (vector-ref e (- (vector-length e) 3))
  (exact-round
   (let ((a (vector-ref e (- (vector-length e) 2))))
    (if (or (= a infinity) (= a minus-infinity)) -1 a)))
  (vector-ref e (- (vector-length e) 1))
  (if (equal? "" string)
      (if (null? default-model)
	  (fuck-up)
	  (first default-model))
      (field-ref string 0))))

(define (exact-round-voc4-detection box)
 (make-voc4-detection
  (exact-round (voc4-detection-x1 box))
  (exact-round (voc4-detection-y1 box))
  (exact-round (voc4-detection-x2 box))
  (exact-round (voc4-detection-y2 box))
  (map (lambda (parts) (map exact-round parts))
       (voc4-detection-parts box))
  (voc4-detection-filter box)
  (voc4-detection-strength box)
  (voc4-detection-delta box)
  (voc4-detection-ringing box)
  (voc4-detection-model box)))

(define (fast-read-boxes-from-zip zip filename)
 (let* ((buffer (zip:read-file-to-buffer zip filename)))
  (fast-read-boxes-from-buffer (x buffer) "")))

(define (padding-detections)
 (list (make-voc4-detection -1 -1 -1 -1 '() -1 minus-infinity 0 minus-infinity #f)))

(define (fast-read-zip-boxes video-name type label num ext expect-last?)
 (with-zip-file
  (lambda (zip)
   ((if expect-last?
	identity (lambda (a) (append a (list (padding-detections)))))
    ((if expect-last? map-frame map-frame-but-last)
     (lambda (frame)
      (let ((boxes
	     (fast-read-boxes-from-zip
	      zip (per-frame-boxes-in-zip-pathname frame type label num ext))))
       ;; TODO this should be cleaned up when fast-read-boxes-* loses
       ;; the ability to read per-video files

       ;;;;;;;;;;;;;;; Temporary fix for box model-name with "." ;;;;;;;;;;;;;;;;
       (for-each (lambda (boxes-per-frame)
       		  (for-each (lambda (box)
			     (when (not (find (voc4-detection-model box) *objects*))
			      (set-voc4-detection-model! box label)))
       			    boxes-per-frame)) boxes)
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (when (> (length boxes) 1)
	(panic "Boxes file with ~a frames in a per-frame file at frame ~a" (length boxes) frame))
       (if (null? boxes) '() (first boxes))))
     video-name)))
  (per-video-box-pathname video-name type label num ext)
  *zip:mode-open*))

(define (pre-zip-fast-read-boxes filename)
 (let loop ((p ((c-function pointer ("read_boxes_file" string)) filename))
	    (l '()))
  (if (= (box-line-t-next p) 0)
      (begin (free p) (reverse (map reverse l)))
      (let ((v (c-inexact-array->vector
		(box-line-t-values p) c-sizeof-double (box-line-t-nr-values p) #t))
	    (s (c-string->string (box-line-t-name p)))
	    (n (box-line-t-next p))
	    (nr (box-line-t-nr-values p)))
       (free (box-line-t-values p)) (free (box-line-t-name p)) (free p)
       (loop (box-line-t-next p)
	     (cond ((< nr 0) (fuck-up))
		   ((= nr 0) l)
		   ((= nr 1) (cons '() l))
		   (else (cons (cons (vector-string->voc4-inexact-box v s) (first l))
			       (cdr l)))))))))

(define (fast-read-boxes-from-buffer buffer . default-model)
 ;; Will destroy the contents of buffer in the process and so frees it
 ;; TODO This is messy because it supports multiple formats, clean up after upgrade
 (let loop ((p ((c-function pointer ("read_boxes_from_buffer" string)) buffer))
	    (l '()))
  (if (= (box-line-t-next p) 0)
      (begin (free p) (free buffer) (reverse (map reverse l)))
      (let ((v (c-inexact-array->vector
		(box-line-t-values p) c-sizeof-double (box-line-t-nr-values p) #t))
	    (s (c-string->string (box-line-t-name p)))
	    (n (box-line-t-next p))
	    (nr (box-line-t-nr-values p)))
       (free (box-line-t-values p)) (free (box-line-t-name p)) (free p)
       (loop (box-line-t-next p)
	     (cond ((< nr 0) (fuck-up))
		   ((= nr 0) l)		   ; skip empty lines
		   ((= nr 1) (cons '() l)) ; frame separator
		   (else
		    (cons (cons (vector-string->voc4-inexact-box v s (first default-model))
				(if (null? l) '() (first l)))
			  (if (null? l) l (cdr l))))))))))

(define (read-detector-boxes video-name type model)
 (fast-read-zip-boxes video-name type model #f "boxes" #t))

(define (read-predicted-boxes video-name type model)
 (fast-read-zip-boxes video-name type model #f "predicted-boxes" #t))

(define (read-tracked-boxes video-name type model number)
 (map first (fast-read-zip-boxes
	     video-name type model number "tracked-box" #t)))

(define (read-smooth-tracked-boxes video-name type model number)
 (map first (fast-read-zip-boxes
	     video-name type model number "smooth-tracked-box" #t)))

(define (read-voc4-detector-boxes video-name model)
 (read-detector-boxes video-name "voc4" model))

(define (read-voc4-predicted-boxes video-name model)
 (read-predicted-boxes video-name "voc4" model))

(define (read-voc4-smooth-tracked-boxes video-name model number)
 (read-smooth-tracked-boxes video-name "voc4" model number))

(define (read-voc4-tracked-boxes video-name model number)
 (read-tracked-boxes video-name "voc4" model number))

(define (read-voc4-overgenerated-detector-boxes video-name model)
 (read-detector-boxes video-name "voc4_overgenerated" model))

(define (read-voc4-overgenerated-predicted-boxes video-name model)
 (read-predicted-boxes video-name "voc4_overgenerated" model))

(define (read-voc4-overgenerated-smooth-tracked-boxes video-name model number)
 (read-smooth-tracked-boxes video-name "voc4_overgenerated" model number))

(define (read-voc4-overgenerated-tracked-boxes video-name model number)
 (read-tracked-boxes video-name "voc4_overgenerated" model number))

(define (read-voc4-event-detector-boxes video-name model)
 (read-detector-boxes video-name "voc4_event" model))

(define (read-voc4-event-predicted-boxes video-name model)
 (read-predicted-boxes video-name "voc4_event" model))

(define (read-voc4-event-smooth-tracked-boxes video-name model number)
 (read-smooth-tracked-boxes video-name "voc4_event" model number))

(define (read-voc4-event-tracked-boxes video-name model number)
 (read-tracked-boxes video-name "voc4_event" model number))

(define (write-voc4-boxes boxes file)
 (write-file (map (lambda (e)
		   (string-join
		    " "
		    (map number->string
			 (list (voc4-detection-x1 e)
			       (voc4-detection-y1 e)
			       (voc4-detection-x2 e)
			       (voc4-detection-y2 e)
			       (voc4-detection-filter e)
			       (voc4-detection-strength e)))))
		  boxes)
	     file))

(define (crop-voc4 image voc4)
 (crop-image image
	     (voc4-detection-x1 voc4)
	     (voc4-detection-y1 voc4)
	     (min (voc4-detection-width voc4)
		  (- (pnm-width image)
		     (voc4-detection-x1 voc4)
		     1))
	     (min (voc4-detection-height voc4)
		  (- (pnm-height image)
		     (voc4-detection-y1 voc4)
		     1))))

(define (voc4-scale voc4 scale)
 ;; TODO Image boundaries
 (let ((w (voc4-detection-width voc4))
       (h (voc4-detection-height voc4))
       (xc (/ (+ (voc4-detection-x2 voc4) (voc4-detection-x1 voc4)) 2))
       (yc (/ (+ (voc4-detection-y2 voc4) (voc4-detection-y1 voc4)) 2)))
  (make-voc4-detection
   (quantize-coordinate (- xc (/ (* w scale) 2)))
   (quantize-coordinate (- yc (/ (* h scale) 2)))
   (quantize-coordinate (+ xc (/ (* w scale) 2)))
   (quantize-coordinate (+ yc (/ (* h scale) 2)))
   '()
   (voc4-detection-filter voc4)
   (voc4-detection-strength voc4)
   (voc4-detection-delta voc4)
   (voc4-detection-ringing voc4)
   (voc4-detection-model voc4))))

(define (voc4-bloat voc4 p) (voc4-scale voc4 (+ 1 p)))

(define (voc4-shrink voc4 p) (voc4-scale voc4 (- 1 p)))

(define (voc4->bb voc4)
 (vector (voc4-detection-x1 voc4) (voc4-detection-y1 voc4)
	 (voc4-detection-x2 voc4) (voc4-detection-y2 voc4)))

(define (video-boxes-available video type kind)
 (map
  (lambda (pathname)
   (let* ((parts (pregexp-split "-" (strip-extension
				     (strip-extension
				      (strip-directory pathname)))))
	  (reverse-parts (reverse parts)))
    (list (first parts)
	  (if (string->number (first  reverse-parts))
	      (string-join "-" (cdr (reverse (cdr reverse-parts))))
	      (string-join "-" (cdr parts)))
	  (if (string->number (first  reverse-parts))
	      (last parts)
	      #f)
	  (extension (strip-extension pathname)))))
  (directory-list (generic-root-pathname
		   video (string-append "/" type "-*." kind ".zip")))))

(define (video-detector-boxes-available video type)
 (video-boxes-available video type "boxes"))

(define (video-predicted-boxes-available video type)
 (video-boxes-available video type "predicted-boxes"))

(define (video-tracked-boxes-available video type)
 (video-boxes-available video type "tracked-box"))

(define (video-smooth-tracked-boxes-available video type)
 (video-boxes-available video type "smooth-tracked-box"))

(define (video-voc4-detector-boxes-available video)
 (video-boxes-available video "voc4" "boxes"))

(define (video-voc4-predicted-boxes-available video)
 (video-boxes-available video "voc4" "predicted-boxes"))

(define (video-voc4-tracked-boxes-available video)
 (video-boxes-available video "voc4" "tracked-box"))

(define (video-voc4-smooth-tracked-boxes-available video)
 (video-boxes-available video "voc4" "smooth-tracked-box"))

(define (video-voc4-overgenerated-tracked-boxes-available video)
 (video-boxes-available video "voc4_overgenerated" "tracked-box"))

(define (video-voc4-overgenerated-smooth-tracked-boxes-available video)
 (video-boxes-available video "voc4_overgenerated" "smooth-tracked-box"))

;;; (map-with-lookahead list 2 '(1 2 3 4) '(a b c d))
;;;     ==> (((1 2) (A B)) ((2 3) (B C)) ((3 4) (C D)) ((4) (D)))
(define (map-with-lookahead f n . lists)
 (vector->list (apply map-vector-with-lookahead (cons f (cons n (map list->vector lists))))))

;;; (map-vector-with-lookahead list 3 '#(a b c d) '#(1 2 3 4))
;;;     ==> #(((A B C) (1 2 3)) ((B C D) (2 3 4)) ((C D) (3 4)) ((D) (4)))
(define (map-vector-with-lookahead f n . vectors)
 (let* ((len (vector-length (first vectors)))
	(window-fun (lambda (v start-index)
		     (map
		      (lambda (a) (vector-ref v a))
		      (remove-if
		       (lambda (a) (> a (- len 1)))
		       (map  (lambda (a) (+ start-index a)) (enumerate n)))))))
  (list->vector
   (map-n
    (lambda (start-index) (apply f (map (lambda (v) (window-fun v start-index)) vectors)))
    len))))

(define (vector-orientation v) (atan (y v) (x v)))

(define (voc4-detection-width detection)
 (- (voc4-detection-x2 detection) (voc4-detection-x1 detection)))

(define (voc4-detection-height detection)
 (- (voc4-detection-y2 detection) (voc4-detection-y1 detection)))

(define (voc4-detection-center detection)
 (vector (/ (+ (voc4-detection-x2 detection) (voc4-detection-x1 detection)) 2)
	 (/ (+ (voc4-detection-y2 detection) (voc4-detection-y1 detection)) 2)))

(define (voc4-detection-aspect-ratio detection)
 (if (zero? (voc4-detection-height detection))
     (begin
      (unless (and (= -1 (voc4-detection-x2 detection))
		   (= -1 (voc4-detection-y2 detection)))
       (format #t "Box with zero size and it's not a dummy!~%"))
      0)
     (/ (voc4-detection-width detection) (voc4-detection-height detection))))

(define (voc4-detection-area detection)
 (let ((x1 (voc4-detection-x1 detection))
       (x2 (voc4-detection-x2 detection))
       (y1 (voc4-detection-y1 detection))
       (y2 (voc4-detection-y2 detection)))
  (* (- x2 x1) (- y2 y1))))

(define (voc4-detection-intersection-area box1 box2)
 ;; Intersection area of two voc4 boxes
 (let ((x1 (max (voc4-detection-x1 box1) (voc4-detection-x1 box2)))
       (x2 (min (voc4-detection-x2 box1) (voc4-detection-x2 box2)))
       (y1 (max (voc4-detection-y1 box1) (voc4-detection-y1 box2)))
       (y2 (min (voc4-detection-y2 box1) (voc4-detection-y2 box2))))
  (cond
   ((or (> 0 (voc4-detection-area box1)) (> 0 (voc4-detection-area box2))) -1)
   ((or (<= x2 x1) (<= y2 y1)) 0)
   (else (* (- x2 x1) (- y2 y1))))))

(define (voc4-detection-union-area box1 box2)
 ;; Union area of two voc4 boxes
 (let ((box1-area (voc4-detection-area box1))
       (box2-area (voc4-detection-area box2))
       (intersection-area (voc4-detection-intersection-area box1 box2)))
  (if (and (> box1-area 0) (> box2-area 0))
      (- (+ box1-area box2-area) intersection-area)
      0)))

(define (voc4-detection-intersection-divided-by-union box1 box2)
 ;; Intersection-area/union-area for voc4 boxes
 (let ((union-area (voc4-detection-union-area box1 box2))
       (intersection-area (voc4-detection-intersection-area box1 box2)))
  (if (> union-area 0)
      (/ intersection-area union-area)
      0)))

(define (non-maximal-suppression voc4-boxes nms-threshold)
 ;; Turn off suppression by passing nms-threshold = 1
 (define (area-overlap box1 box2)
  (/ (voc4-detection-intersection-area box1 box2) (voc4-detection-area box2)))
 (let loop ((boxes (sort voc4-boxes > voc4-detection-strength)) (good-boxes '()))
  (if (null? boxes)
      (reverse good-boxes)
      (loop (remove-if (lambda (test-box)
			(> (area-overlap (first boxes) test-box) nms-threshold))
		       (rest boxes))
	    (cons (first boxes) good-boxes)))))

(define (top-n-non-maximal-suppression voc4-boxes nms-threshold top-n)
 ;; Turn off suppression by passing nms-threshold = 1
 ;; Asymptotic complexity is top-n * (length voc4-boxes)
 ;; Results are always sorted
 (define (apply-box test-box boxes)
  (define (area-overlap box1 box2)
   (/ (voc4-detection-intersection-area box1 box2) (voc4-detection-area box2)))
  (define (test-box-stronger-than box)
   (> (voc4-detection-strength test-box) (voc4-detection-strength box)))
  (define (test-box-overlaps-with box)
   (> (area-overlap box test-box) nms-threshold))
  (let* ((weaker-boxes (remove-if-not test-box-stronger-than boxes))
	 (stronger-boxes (remove-if test-box-stronger-than boxes))
	 (result (append (remove-if test-box-overlaps-with weaker-boxes)
			 (if (some test-box-overlaps-with stronger-boxes)
			     '() ; A stronger box suppresses test-box
			     (list test-box))
			 stronger-boxes)))
   ;; We keep (length result) no larger than top-n
   (if (> (length result) top-n)
       (rest result) ; We never add more than 1 box
       result)))
 (let loop ((boxes (sort voc4-boxes > voc4-detection-strength))
	    (good-boxes '()))
  (if (null? boxes)
      (reverse good-boxes) ;; Note that apply-box is always sorted weak=>strong
      (loop (rest boxes) (apply-box (first boxes) good-boxes)))))

(define (voc4-detection->corners box)
 ;; clockwise tl, tr, br, bl
 (list (vector (voc4-detection-x1 box) (voc4-detection-y1 box))
       (vector (voc4-detection-x2 box) (voc4-detection-y1 box))
       (vector (voc4-detection-x2 box) (voc4-detection-y2 box))
       (vector (voc4-detection-x1 box) (voc4-detection-y2 box))))

(define (point-in-voc4-detection? point box)
 (and (< (voc4-detection-x1 box) (x point) (voc4-detection-x2 box))
      (< (voc4-detection-y1 box) (y point) (voc4-detection-y2 box))))

(define (ringing-by? box-a box-b)
 (or (some (lambda (c) (point-in-voc4-detection? c box-b)) (voc4-detection->corners box-a))
     (every (lambda (c) (point-in-voc4-detection? c box-a)) (voc4-detection->corners box-b))))

;;; siddharth TODO: merge this the actual top-boxes computation
(define (top-n-non-maximal-suppression-with-ringing boxes nms-threshold top-n)
 (let ((top-boxes (top-n-non-maximal-suppression boxes nms-threshold top-n))
       (num-detections (length boxes)))
  (for-each
   (lambda (top-box)
    (set-voc4-detection-ringing!
     top-box
     (/ (count-if (lambda (box) (ringing-by? box top-box)) boxes) num-detections)))
   top-boxes)
  top-boxes))

;;; siddharth: hack - ideally, this should be propogated through the call chains
(define *finite-difference-scale* 1)
(define (feature-finite-difference feature lookahead)
 (map
  (lambda (v)
   (cond ((list? v) (map (lambda (e) (* e *finite-difference-scale*)) v))
	 ((vector? v) (k*v *finite-difference-scale* v))
	 ((number? v) (* *finite-difference-scale* v))
	 (else (panic "feature-finite-difference: Unsupported form for v"))))
  (let* ((l (cdr
	     (reverse
	      (map-with-lookahead
	       (lambda (i)
		(if (null? (cdr i))
		    #f
		    (list-mean (map (lambda (a b) ((if (vector? a) v- -) b a))
				    (but-last i) (cdr i)))))
	       lookahead
	       feature)))))
   (reverse (cons (car l) l)))))

;;; Returns the areas of the boxes, all divided by the mean area of all the
;;; boxes, after accounting for 'invalid-track' boxes with area -1.
(define (normalized-area boxes)
 (let* ((raw-area (map voc4-detection-area boxes))
	(valid-areas (remove-if (lambda (n) (< n 0)) raw-area))
	(mean-area (if (null? valid-areas) '() (list-mean valid-areas))))
  (map (lambda (a) (if (< a 0) -1 (/ a mean-area))) raw-area)))

(define (person-boxes? boxes)
 (prefix? "person" (most-frequent-class boxes)))

(define (normalize-line-in-voc4-box l b)
 (let* ((vec (v- (q l) (p l)))
	(aspect-ratio (abs (voc4-detection-aspect-ratio b)))
	(new-ht (sqrt (/ aspect-ratio)))
	(new-wd (* aspect-ratio new-ht)))
  (vector (magnitude (vector (* (x vec) (/ new-wd (voc4-detection-width b)))
			     (* (y vec) (/ new-ht (voc4-detection-height b)))))
	  (vector-orientation vec))))

(define (part-center l)
 (vector (/ (+ (first l) (third l)) 2)
	 (/ (+ (second l) (fourth l)) 2)))

(define (part-boxes->part-features box empty-parts)
 (if (null? (voc4-detection-parts box))
     ;; This happens to boxes which have padding
     (map-vector (lambda (r) (if (every (lambda (e) (= e -1)) (vector->list r))
				 '#(-1 -1) ; the feature displacement for the part boxes
				 (fuck-up) ; it should never be triggered
				 ))
		 empty-parts)
     (map-vector
      (lambda (part)
       (normalize-line-in-voc4-box
	(make-line-segment (voc4-detection-center box) (part-center part))
	box))
      (list->vector (voc4-detection-parts box)))))

(define (get-part-vectors boxes)
 (let ((box (find-if (lambda (box) (not (null? (voc4-detection-parts box)))) boxes)))
  (unless box (panic "Track contains only padding"))
  (let ((empty-parts (map-matrix (const -1)
				 (list->vector (map list->vector (voc4-detection-parts box))))))
   (map
    (lambda (box) (vector->list (unshape-matrix (part-boxes->part-features box empty-parts))))
    boxes))))

(define (get-changes-in-part-vectors part-vector-positions lookahead)
 (let ((r (map
	   (lambda (positions)
	    (unshape-matrix
	     (map-vector polar->rect (shape-matrix (list->vector positions) 2))))
	   part-vector-positions)))
  (vector->list
   (map-vector
    (lambda (frame)
     (vector->list (unshape-matrix (map-vector rect->polar (shape-matrix frame 2)))))
    (list-of-lists->transposed-matrix
     (map-n
      (lambda (i) (feature-finite-difference (map (lambda (e)(vector-ref e i)) r) lookahead))
      16))))))				;hardwired

(define (unzip l)
 (map-n (lambda (i) (map (lambda (e) (list-ref e i)) l)) (length (first l))))

(define (zip-list l)
 (unless (every (lambda (e) (= (length e) (length (first l)))) l)
  (panic "clip-bad-tracks with tracks of different lengths"))
 (map-n (lambda (i) (map (lambda (e) (list-ref e i)) l)) (length (first l))))

(define (clip-bad-tracks tracks)
 (unzip (remove-if (lambda (boxes) (some dropped-box? boxes)) (zip-list tracks))))

(define (voc4-inside? v box)
 (and (< (voc4-detection-x1 box) (x v) (voc4-detection-x2 box))
      (< (voc4-detection-y1 box) (y v) (voc4-detection-y2 box))))

(define (quad-scale-and-shift quad scale shift)
 (let ((cx (+ (/ (+ (first quad) (third quad)) 2) (x shift)))
       (cy (+ (/ (+ (second quad) (fourth quad)) 2) (y shift)))
       (w (* (- (third quad) (first quad)) (x scale)))
       (h (* (- (fourth quad) (second quad)) (y scale))))
  (list (- cx (/ w 2)) (- cy (/ h 2)) (+ cx (/ w 2)) (+ cy (/ h 2)))))

(define (voc4-box->summary box)
 (list (exact-round (voc4-detection-x1 box))
       (exact-round (voc4-detection-y1 box))
       (exact-round (voc4-detection-x2 box))
       (exact-round (voc4-detection-y2 box))
       (voc4-detection-area box)
       (voc4-detection-model box)
       (voc4-detection-strength box)))

(define (voc4-scale-and-shift box scale shift delta)
 (let* ((center (v+ (voc4-detection-center box) shift))
	(width (* (x scale) (voc4-detection-width box)))
	(height (* (y scale) (voc4-detection-height box))))
  (make-voc4-detection (- (x center) (/ width 2))
		       (- (y center) (/ height 2))
		       (+ (x center) (/ width 2))
		       (+ (y center) (/ height 2))
		       (map
			(lambda (quad) (quad-scale-and-shift quad scale shift))
			(voc4-detection-parts box))
		       (voc4-detection-filter box)
		       (voc4-detection-strength box)
		       delta
		       (voc4-detection-ringing box)
		       (voc4-detection-model box))))

(define (get-scale-and-shift box klt)
 (let* ((klt (remove-if-not
	      (lambda (klt-pair) (voc4-inside? (klt-pair-current klt-pair) box))
	      klt))
	(xs (map - (map (o x klt-pair-next) klt) (map (o x klt-pair-current) klt)))
	(ys (map - (map (o y klt-pair-next) klt) (map (o y klt-pair-current) klt))))
  (if (null? klt)
      (list (vector 1 1) (vector 0 0))
      (list (vector
	     (+ 1 (/ (sqrt (list-variance xs)) (voc4-detection-width box)))
	     (+ 1 (/ (sqrt (list-variance ys)) (voc4-detection-height box))))
	    (vector (list-mean xs) (list-mean ys))))))

(define (voc4-scale-abs box scale)
 (make-voc4-detection
  (* scale (voc4-detection-x1 box))
  (* scale (voc4-detection-y1 box))
  (* scale (voc4-detection-x2 box))
  (* scale (voc4-detection-y2 box))
  (map (lambda (parts) (map (lambda (p) (* scale p)) parts))
       (voc4-detection-parts box))
  (voc4-detection-filter box)
  (voc4-detection-strength box)
  (voc4-detection-delta box)
  (voc4-detection-ringing box)
  (voc4-detection-model box)))

;; for optical-flow
(define (forward-project-box-scaled
         original-box scaled-transformation delta scale)
 ;; before projecting the box, we rescale the box (which is always in 1280x720
 ;; coordinate) to the video dimension (ie. x scale)
 (let ((box (voc4-scale-abs original-box scale))
       (transformation scaled-transformation))
  (voc4-scale-abs
   (cond
    ((and (list? transformation) (every klt-pair? transformation))
;;; needs work: changed API
     (let ((scale-and-shift (get-scale-and-shift box transformation)))
      (voc4-scale-and-shift
       box (first scale-and-shift) (second scale-and-shift) delta)))
    (else
     (voc4-scale-and-shift box '#(1 1) (average-flow-in-box box transformation) delta)))
   (/ scale))))

(define (box->vector box)
 (vector (voc4-detection-x1 box)
	 (voc4-detection-y1 box)
	 (voc4-detection-x2 box)
	 (voc4-detection-y2 box)))

(define (predict-boxes n boxes-movie transformation-movie
                       scale)
 (unless (= (length boxes-movie) (+ (length transformation-movie) 1))
  (panic "predict-boxes"))
 (let loop ((n n)
	    (prefix '(()))
	    (predicted-boxes-movie boxes-movie)
	    (transformation-movie transformation-movie)
	    (augmented-boxes-movie boxes-movie))
  (unless *quiet-mode?* (format #t "predict boxes: ~s~%" n))
  (if (zero? n)
      augmented-boxes-movie
      (let ((predicted-boxes-movie
	     (map2
	      (lambda (boxes transformation)
	       (map (lambda (box)
		     (forward-project-box-scaled box transformation
                                                 (length prefix)
                                                 scale))
		    boxes))
	      (but-last predicted-boxes-movie)
	      transformation-movie)))
       (loop (- n 1)
	     (cons '() prefix)
	     predicted-boxes-movie
	     (rest transformation-movie)
	     (map2 append
		   (append prefix predicted-boxes-movie)
		   augmented-boxes-movie))))))

(define (update-voc4-strength box strength)
 (make-voc4-detection (voc4-detection-x1 box)
		      (voc4-detection-y1 box)
		      (voc4-detection-x2 box)
		      (voc4-detection-y2 box)
		      (voc4-detection-parts box)
		      (voc4-detection-filter box)
		      strength
		      (voc4-detection-delta box)
		      (voc4-detection-ringing box)
		      (voc4-detection-model box)))

(define (model-threshold model-name model-path)
 (matlab-eval-strings
  (format #f "load('~a/~a');" model-path model-name)
  "if exist('model'); a=model; elseif exist('csc_model'); a=csc_model; else a='error'; end;"
  "thresh=a.thresh;")
 (matlab-get-double "thresh"))

;;; Likelihood computation

(define (medoids-pathname)
 ;; TODO This is just a temporary location, we should find a naming
 ;; scheme or even better bundle this with the hmm so we never make a
 ;; mistake
 (string-append *video-pathname* "/pose-codebook.sc"))

(define (track-profile-pathname video)
 (generic-root-pathname video "track-profile-data"))

(define (extract-common-features boxes lookahead)
 (let* ((position (map voc4-detection-center boxes))
	(position-x (map x position))
	(position-y (map y position))
	(aspect-ratio (map voc4-detection-aspect-ratio boxes))
	(velocity (feature-finite-difference (map vector position-x position-y) lookahead))
	(acceleration (feature-finite-difference velocity lookahead))
	(area (map voc4-detection-area boxes)))
  (list position-x position-y aspect-ratio (feature-finite-difference aspect-ratio lookahead)
	(map magnitude velocity) (map vector-orientation velocity)
	(map magnitude acceleration) (map vector-orientation acceleration)
	area (feature-finite-difference area lookahead))))

;; TODO Without this pose codebook indices for padding frames would be
;; wrong, should be in the trained hmm or at least a parameter
(define *objects*
 '( ;; Summer 2011 objects
   "bag" "baseball-bat" "bench" "bicycle" "big-ball" "bucket" "cage" "car"
   "cardboard-box" "cart" "chair" "closet" "dog" "door" "garbage-can"
   "golf-club" "ladder" "mailbox" "microwave" "motorcycle" "person"
   "person-crawl" "person-down" "pogo-stick" "pylon" "rake" "shovel"
   "skateboard" "small-ball" "suv" "table" "toy-truck" "trailer"
   "trash-bag" "tripod" "trophy" "truck"
   ;; 2012 toy demo objects
   "gun" "sign" "giraffe" "ball"))

(define (person-box? box)
 (and (voc4-detection-model box)
      (prefix? "person" (voc4-detection-model box))))

(define (get-closest-medoids boxes medoids)
 (map
  (lambda (box)
   (if (person-box? box)
       (let* ((center (voc4-detection-center box))
	      (normal-parts
	       (list->vector
		(join (map
		       (lambda (part)
			(vector->list
			 (normalize-line-in-voc4-box
			  (make-line-segment (part-center part) center) box)))
		       (voc4-detection-parts box))))))
	(position (minimump medoids (lambda (m) (distance m normal-parts))) medoids))
       (length medoids)))
  boxes))

(define (pairwise-track-features-depraved centers1 centers2 lookahead)
 (let* ((center-distance (map distance centers1 centers2))
	(center-velocity (feature-finite-difference center-distance lookahead))
	(center-orientation (map vector-orientation (map v- centers1 centers2))))
  (list-of-lists->transposed-matrix
   (map (lambda (fv) (drop-last-n-if-possible lookahead fv))
	(list center-distance center-velocity center-orientation)))))

(define (one-track-features type boxes lookahead medoids)
 (let ((common-features (extract-common-features boxes lookahead)))
  (list-of-lists->transposed-matrix
   (map (lambda (fv) (drop-last-n-if-possible lookahead fv))
	(case type
	 ((non-pose) common-features)
	 ((non-pose-no-x) (cddr common-features))
	 ((continuous-pose)
	  (let ((part-vector-positions
		 (if (person-boxes? boxes)
		     (get-part-vectors boxes)
		     (map-n (lambda (_) (map-n (lambda (_) 0) 16)) (length boxes)))))
	   (append common-features
		   (transpose-list-of-lists part-vector-positions)
		   (transpose-list-of-lists
		    (get-changes-in-part-vectors part-vector-positions lookahead)))))
	 ((continuous-pose-no-x)
	  (let ((part-vector-positions
		 (if (person-boxes? boxes)
		     (get-part-vectors boxes)
		     (map-n (lambda (_) (map-n (lambda (_) 0) 16)) (length boxes)))))
	   (append (cddr common-features)
		   (transpose-list-of-lists part-vector-positions)
		   (transpose-list-of-lists
		    (get-changes-in-part-vectors part-vector-positions lookahead)))))
	 ((discrete-pose)
	  (append
	   common-features
	   (list (get-closest-medoids boxes medoids)
		 (map
		  (lambda (box)
		   (let ((model-name (voc4-detection-model box)))
		    (if (equal? model-name "padding")
			(length *objects*)
			(position (if (person-box? box)
					;(prefix? "person" model-name)
				      "person"
				      model-name)
				  *objects*))))
		  boxes)
		 (map
		  (lambda (b) (case (voc4-detection-filter b)
			       ((1) 0) ((2) 1) ((3) 2) ((4) 3) ((5) 4) ((6) 5) (else 6)))
		  boxes))))
	 ((discrete-pose-no-x)
	  (append
	   (cddr common-features)
	   (list (get-closest-medoids boxes medoids)
		 (map
		  (lambda (box)
		   (let ((model-name (voc4-detection-model box)))
		    (if (equal? model-name "padding")
			(length *objects*)
			(position (if (person-box? box)
					;(prefix? "person" model-name)
				      "person"
				      model-name)
				  *objects*))))
		  boxes)
		 (map
		  (lambda (b) (case (voc4-detection-filter b)
			  ((1) 0) ((2) 1) ((3) 2) ((4) 3) ((5) 4) ((6) 5) (else 6)))
		  boxes))))
	 (else (panic "one-track-features unknown type ~a" type)))))))

(define (two-track-features type track1 track2 lookahead medoids)
 (unless (= (length track1) (length track2))
  (panic "Trying to compute a feature vector for tracks of unequal length"))
 (map-vector vector-append
	     (one-track-features type track1 2 medoids)
	     (one-track-features type track2 2 medoids)
	     (pairwise-track-features track1 track2 lookahead)))

(define (features-log-likelihood features trained-hmm)
 (let* ((rmat (features-vectors->rmat features))
	(log-likelihood (model-log-likelihood (trained-hmm-model trained-hmm) rmat)))
  (free-rmat rmat)
  log-likelihood))

(define (features->rmat features)
 (let ((rmat (allocate-rmat (length (car features)) (length features))))
  (for-each-indexed
   (lambda (feature-vector time)
    (for-each-indexed (lambda (feature i)
		       (rmat-set! rmat i time feature))
		      feature-vector))
   features)
  rmat))

(define (features-vectors->rmat features)
 (let* ((rmat (allocate-rmat (vector-length (x features)) (vector-length features))))
  (for-each-indexed-vector
   (lambda (feature-vector time)
    (for-each-indexed-vector
     (lambda (feature i) (rmat-set! rmat i time feature))
     feature-vector))
   features)
  rmat))

(define (map-all-unordered-pairs f l) (map (lambda (e) (f (first e) (second e))) (all-pairs l)))
(define (map-all-ordered-pairs f l) (join (map (lambda (a) (map (lambda (b) (f a b)) (remove a l))) l)))

(define (read-hmm filename)
 ;; this is a hack to clip and normalize the discrete distributions
 (define (normalise-row r)
  (let* ((v 1e-300) (l (map (lambda (a) (max a v)) r)) (s (reduce + l 0)))
   (map (lambda (e) (/ e s)) l)))
 (define (normalize-psi-parameters psi)
  (set-psi-parameters!
   psi
   (map-vector
    (lambda (s)
     (map-indexed-vector
      (lambda (d i) (if (equal? (first d) 'discrete) ;(or (= i 11) (= i 24))
		   (cons 'discrete (normalise-row (rest d)))
		   d))
      s))
    (psi-parameters psi)))
  (set-psi-a!
   psi
   (list->vector
    (map-indexed
     (lambda (r i)
      (vector-append
       (subvector r 0 i)
       (list->vector (normalise-row (vector->list (subvector r i (vector-length r)))))))
     (vector->list (psi-a psi)))))
  (set-psi-b! psi (list->vector (normalise-row (vector->list (psi-b psi)))))
  psi)
 (let ((trained-hmm (read-object-from-file
		     (expand-filename (default-extension filename "sc")))))
  ;; Check for legacy hmm models
  (unless (trained-hmm? trained-hmm)
   ;; feature type is one of: non-pose, continuous-pose, continuous-pose-no-x, discrete-pose
   ;; training type is one of: ml or dt
   (panic "You are trying to read in an hmm in the old format.
	   The new format adds three fields at the end containing the number of participants, feature type, and training type"))
  (set-trained-hmm-model!
   trained-hmm
   (psi->model (normalize-psi-parameters (trained-hmm-model trained-hmm))))
  trained-hmm))

(define (compute-likelihoods video named-tracks models medoids
			     likelihoods-cache-f unordered-pairs?)
 (let ((models-features-1 (remove-if-not (lambda (m) (= (trained-hmm-participants m) 1)) models))
       (models-features-2 (remove-if-not (lambda (m) (= (trained-hmm-participants m) 2)) models))
       (video-name (any-video->string video)))
  (define (compute-likelihood video track-names feature-vector models named-tracks)
   (map
    (lambda (model)
     (make-result
      video-name
      track-names
      (trained-hmm-feature-type model)
      (trained-hmm-states model)
      (trained-hmm-verb model)
      (if (= (vector-length feature-vector) 0)
	  (- infinity)
	  (features-log-likelihood feature-vector model))
      named-tracks))
    models))
  (append
   (map
    (lambda (named-track)
     (likelihoods-cache-f
      video (list named-track) models-features-1
      (lambda () (compute-likelihood
		  video
		  (list (first named-track))
		  (let ((a (one-track-features (trained-hmm-feature-type (first models))
					       (first (clip-bad-tracks (list (second named-track))))
					       2 medoids)))
		   (pp a)(newline)
		   a)
		  models-features-1 (list named-track)))))
    named-tracks)
   ((if unordered-pairs?
	map-all-unordered-pairs
	map-all-ordered-pairs)
    (lambda (named-track-1 named-track-2)
     (likelihoods-cache-f
      video (list named-track-1 named-track-2) models-features-2
      (lambda ()
       (let ((tracks (clip-bad-tracks (list (second named-track-1) (second named-track-2)))))
	(compute-likelihood
	 video
	 (list (first named-track-1) (first named-track-2))
	 (two-track-features (trained-hmm-feature-type (first models))
			     (first tracks) (second tracks) 2 medoids)
	 models-features-2 (list named-track-1 named-track-2))))))
    named-tracks))))

(define (compute-likelihoods-from-files
	 video model-names-file
	 medoids track-filter precomputed-likelihoods-f
	 unordered-pairs?)
 (compute-likelihoods video
		      (map
		       (lambda (available)
			(string-append (string-join "-" (but-last available)) "." (last available))
			(read-voc4-overgenerated-smooth-tracked-boxes video (second available) (third available)))
		       (remove-if track-filter (video-voc4-overgenerated-smooth-tracked-boxes-available video)))
		      (map read-hmm (remove "" (read-file model-names-file)))
		      medoids precomputed-likelihoods-f
		      unordered-pairs?))

;;; Viterbi tracker

(define (box-distance box1 box2)
 ;; This is what the Matlab code does but we believe that it should do
 ;; (distance (box->vector box1) (box->vector box2)).
 ;; Once upon a time we used squared distance, but it seems to work
 ;; more poorly than euclidean distance
 ;; (let* ((b1 (voc4-detection-center box1)) (b2 (voc4-detection-center box2)))
 ;;  (+ (sqr (- (x b2) (x b1)))
 ;;     (sqr (- (y b2) (y b1)))))
 (distance (voc4-detection-center box1) (voc4-detection-center box2)))

(define (box-pair-cost box1 transformation box2 scale)
 (box-distance (forward-project-box-scaled box1 transformation 0
                                           scale) box2))

(define (box-cost box) (- (voc4-detection-strength box)))

;; (define (box-ringing box) (- (* (voc4-detection-ringing box) (voc4-detection-ringing box))))
(define (box-ringing box)
 (when (or (= (voc4-detection-ringing box) infinity)
	   (= (voc4-detection-ringing box) (- infinity)))
  (fuck-up))
 (- (voc4-detection-ringing box)))

(define (position-of-first-valid-box boxes-movie threshold)
 (position-if (lambda (boxes) (some-valid-box? boxes threshold)) boxes-movie))

(define (in-bounds? box)
 (let ((c (voc4-detection-center box)))
  (and (<= 0 (x c) 1279) (<= 0 (y c) 719))))

(define (update-voc4-model box model)
 (make-voc4-detection (voc4-detection-x1 box)
		      (voc4-detection-y1 box)
		      (voc4-detection-x2 box)
		      (voc4-detection-y2 box)
		      (voc4-detection-parts box)
		      (voc4-detection-filter box)
		      (voc4-detection-strength box)
		      (voc4-detection-delta box)
		      (voc4-detection-ringing box)
		      model))

(define (viterbi-track-one boxes-movie transformation-movie
                           alpha beta subverted-model-threshold dt?
                           scale)
 (unless (and (>= (length boxes-movie) 2)
	      (= (length boxes-movie) (+ (length transformation-movie) 1)))
  (panic "viterbi-track-one 1"))
 (let* ((first-frame
	 (position-of-first-valid-box boxes-movie subverted-model-threshold))
	(last-frame
	 (- (length boxes-movie)
	    (position-of-first-valid-box (reverse boxes-movie) subverted-model-threshold)))
	(end-padding (- (length boxes-movie) last-frame))
	(boxes-movie
	 (let loop ((old-boxes-movie (sublist boxes-movie first-frame last-frame))
		    (new-boxes-movie '()))
	  (cond ((null? old-boxes-movie) (reverse new-boxes-movie))
		((null? (first old-boxes-movie))
		 (loop (rest old-boxes-movie)
		       (cons (first new-boxes-movie) new-boxes-movie)))
		(else (loop (rest old-boxes-movie)
			    (cons (first old-boxes-movie) new-boxes-movie)))))))
  (let loop ((frame (+ first-frame 1))
	     (box-movies (map list (first boxes-movie)))
	     (costs (map (lambda (box) (+ (* alpha (box-cost box)) (* beta (box-ringing box))))
			 (first boxes-movie)))
	     (boxes-movie (rest boxes-movie))
	     (transformation-movie
	      (sublist transformation-movie first-frame (- last-frame 1))))
   (unless *quiet-mode?* (format #t "viterbi-track: ~s~%" (length boxes-movie)))
   (when (null? costs) (panic "viterbi-track-one 2"))
   (if (null? boxes-movie)
       (append
	(map-n (lambda (i) (make-voc4-detection -1 -1 -1 -1 '() -1 minus-infinity 0 minus-infinity #f))
	       first-frame)
	(reverse (list-ref box-movies (positionv (reduce min costs infinity) costs)))
	(map-n (lambda (i) (make-voc4-detection -1 -1 -1 -1 '() -1 minus-infinity 0 minus-infinity #f))
	       end-padding))
       (let* ((new-box-movies-and-costs
	       (map
		(lambda (box)
		 (let* ((new-costs
			 (map2
			  (lambda (box-movie cost)
			   (+ cost
			      (box-pair-cost (first box-movie)
					     (first transformation-movie)
					     box
					     scale)))
			  box-movies costs))
			(best-cost (reduce min new-costs infinity)))
		  (list (cons box (list-ref box-movies (positionv best-cost new-costs)))
			(+ best-cost (* alpha (box-cost box)) (* beta (box-ringing box))))))
		(first boxes-movie))))
	(loop (+ frame 1)
	      (map first new-box-movies-and-costs)
	      (map second new-box-movies-and-costs)
	      (rest boxes-movie)
	      (rest transformation-movie)))))))

(define (some-valid-box? boxes threshold)
 (some (lambda (box) (>= (voc4-detection-strength box) threshold)) boxes))

(define (keep-track-in-context? box-movie box-movies)
 (define (soft-same-track? track1 track2)
  ;; TODO siddharth: move hardwired constants from here
  (>= (count-if
       (lambda (e) (<= (distance (box->vector (first e)) (box->vector (second e))) 20))
       (zip track1 track2))
      (* 0.3 (length track1))))
 (or (null? box-movies)
     (not (some (lambda (old-box-movie) (soft-same-track? box-movie old-box-movie)) box-movies))))

(define (update-voc4-ringing box ringing)
 (make-voc4-detection (voc4-detection-x1 box)
		      (voc4-detection-y1 box)
		      (voc4-detection-x2 box)
		      (voc4-detection-y2 box)
		      (voc4-detection-parts box)
		      (voc4-detection-filter box)
		      (voc4-detection-strength box)
		      (voc4-detection-delta box)
		      ringing
		      (voc4-detection-model box)))

(define (viterbi-track-multiple boxes-movie transformation-movie
				alpha beta subverted-model-threshold dt?
				minimum-track-length
				maximum-track-overlap-ratio
				overgeneration-minimum-track-length
				overgeneration-maximum-track-overlap-ratio
				suppression-delta scale
				max-tracks)
 (let loop ((boxes-movie boxes-movie) (box-movies '()) (i 0))
  (if (some (lambda (boxes) (some-valid-box? boxes subverted-model-threshold))
	    boxes-movie)
      (let ((box-movie (viterbi-track-one
			boxes-movie transformation-movie
			alpha beta subverted-model-threshold dt?
                        scale)))
       (if (>= i max-tracks)
	   (begin
	    (unless *quiet-mode?*
	     (format #t "Terminating: maximum of ~a iterations reached~%" i)
	     (format #t "Total Tracks: ~a~%" (length box-movies)))
	    (list (length box-movies) (reverse box-movies)))
	   (loop
	    (map2 (lambda (box boxes)
		   (if (null? boxes)
		       '()
		       (map (lambda (box2)
			     (if (or (some (lambda (c) (point-in-voc4-detection? c box))
					   (voc4-detection->corners box2))
				     (every (lambda (c) (point-in-voc4-detection? c box2))
					    (voc4-detection->corners box)))
				 ;; (voc4-inside? (voc4-detection-center box2) box)
				 (update-voc4-strength
				  (update-voc4-ringing
				   box2
				   (* 0.66 (voc4-detection-ringing box2)))
				  (min (+ subverted-model-threshold suppression-delta)
				       (voc4-detection-strength box2)))
				 box2))
			    boxes)))
		  box-movie boxes-movie)
	    ;; (cons box-movie box-movies) ; always produces max-tracks tracks
	    (if (keep-track-in-context? box-movie box-movies)
	    	(cons box-movie box-movies)
	    	box-movies)
	    (+ i 1))))
      (begin (format #t "Terminating: no more valid boxes~%")
	     (format #t "Total Tracks: ~a~%" (length box-movies))
	     (list (length box-movies) (reverse box-movies))))))

(define (get-cleaned-up-model-names-list video)
 (map
  (lambda (model-class)
   (remove-if (lambda (e) (or (suffix? "wheelbarrow" e) (suffix? "crawl" e))) model-class))
  (get-model-names-list video)))

(define (viterbi-track video klt-movie optical-flow-movie
		       detector-boxes-movie-model-name-pairs top-n with-dt?
		       model-path
		       model-threshold-tracker-offset
		       profile-best-boxes?
		       look-ahead
		       minimum-track-length
		       maximum-track-overlap-ratio
		       overgeneration-minimum-track-length
		       overgeneration-maximum-track-overlap-ratio
		       suppression-delta
		       alpha
		       beta
		       max-tracks)
 (start-matlab!)
 (let*
   ;; TODO This might fail if no detections exist for a model
   ((scale (video-scale video))
    (detector-boxes-movies
     (map (lambda (detector-boxes-movie)
	   (map (lambda (detector-boxes)
		 (sublist (sort detector-boxes > voc4-detection-strength)
			  0
			  (min (length detector-boxes) top-n)))
		detector-boxes-movie))
	  (map first detector-boxes-movie-model-name-pairs)))
    (model-names (map second detector-boxes-movie-model-name-pairs))
    (model-name (first model-names))
    (model-thresholds (map (lambda (model-name)
			    (model-threshold model-name model-path))
			   model-names))
    ;; The thresholds, max-threshold, and delta are computed after the
    ;; first top-n filter but before the second top-n filter.
    (thresholds
     (map (lambda (detector-boxes-movie model-threshold)
	   (min (matlab-threshold-otsu
		 (if profile-best-boxes?
		     (remove minus-infinity
			     (map (lambda (detector-boxes)
				   (map-reduce max
					       minus-infinity
					       voc4-detection-strength
					       detector-boxes))
				  detector-boxes-movie))
		     (map-reduce append
				 '()
				 (lambda (a) (map voc4-detection-strength a))
				 detector-boxes-movie)))
		(+ model-threshold model-threshold-tracker-offset)))
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
       (let ((detector-boxes
	      (sort (map-reduce append
				'()
				(lambda (detector-boxes-movie)
				 (list-ref detector-boxes-movie i))
				detector-boxes-movies)
		    >
		    voc4-detection-strength)))
	(if top-n
	    (sublist detector-boxes 0 (min (length detector-boxes) top-n))
	    detector-boxes)))
      (length (first detector-boxes-movies))))
    (predicted-boxes-movie
     (predict-boxes
      (min look-ahead
	   (- (video-last-frame video) (video-first-frame video)))
      detector-boxes-movie
      klt-movie scale))
    (count-and-box-movies
     (viterbi-track-multiple
      (map (lambda (bs) (remove-if-not in-bounds? bs)) predicted-boxes-movie)
      optical-flow-movie alpha beta max-threshold with-dt?
      minimum-track-length
      maximum-track-overlap-ratio
      overgeneration-minimum-track-length
      overgeneration-maximum-track-overlap-ratio
      suppression-delta scale max-tracks))
    (number-of-non-overgenerated-tracks (first count-and-box-movies))
    (box-movies (second count-and-box-movies))
    (smooth-box-movies
     (map
      (lambda (box-movie)
       (let* ((l (length (remove-if-not nondropped-box? box-movie)))
	      (pieces (max 5 (exact-round (* (/ l 140) 10)))))
	(smooth-box-movie box-movie pieces pieces pieces pieces)))
      box-movies)))
  (if number-of-non-overgenerated-tracks
      (vector predicted-boxes-movie
	      (sublist box-movies 0 number-of-non-overgenerated-tracks)
	      (sublist smooth-box-movies 0 number-of-non-overgenerated-tracks)
	      box-movies
	      smooth-box-movies)
      #f)))

(define (write-viterbi-track-results video model-name viterbi-track-results)
 (let ((predicted-boxes-movie (vector-ref viterbi-track-results 0))
       (box-movies (vector-ref viterbi-track-results 1))
       (smooth-box-movies (vector-ref viterbi-track-results 2))
       (overgenerated-box-movies (vector-ref viterbi-track-results 3))
       (overgenerated-smooth-box-movies (vector-ref viterbi-track-results 4)))
  (write-voc4-predicted-boxes-movie predicted-boxes-movie video model-name)
  ;; box-movies
  (for-each-indexed
   (lambda (box-movie i)
    (write-voc4-tracked-box-movie box-movie video model-name (number->string (+ i 1))))
   box-movies)
  ;; smooth-box-movies
  (for-each-indexed
   (lambda (box-movie i)
    (write-voc4-smooth-tracked-box-movie box-movie video model-name (number->string (+ i 1))))
   smooth-box-movies)
  ;; overgenerated-box-movies
  (for-each-indexed
   (lambda (box-movie i)
    (write-voc4-overgenerated-tracked-box-movie
     box-movie video model-name (number->string (+ i 1))))
   overgenerated-box-movies)
  ;; overgenerated-smooth-box-movies
  (for-each-indexed
   (lambda (box-movie i)
    (write-voc4-overgenerated-smooth-tracked-box-movie
     box-movie video model-name (number->string (+ i 1))))
   overgenerated-smooth-box-movies)))

(define (read-and-viterbi-track
	 video cuda-klt? cuda-optical-flow? cuda-object-detector?
	 model-lists nms alpha beta final-cascade-adjustment threshold-offset
	 top-n maximum-detections with-dt? model-path model-threshold-tracker-offset
	 ;; TODO profile-best-boxes is broken when a frame is missing detections
	 profile-best-boxes?
	 look-ahead
	 minimum-track-length
	 maximum-track-overlap-ratio
	 overgeneration-minimum-track-length
	 overgeneration-maximum-track-overlap-ratio
	 suppression-delta max-tracks)
 (let* ((model-lists (if model-lists
			 model-lists
			 (get-cleaned-up-model-names-list video)))
	(model-names (remove-duplicates (join model-lists)))
	(detector-boxes-movie-model-name-pairs
	 (time-code
	  (if cuda-object-detector?
	      (map list (run-felzenszwalb-detector
                         video model-names
                         final-cascade-adjustment
                         threshold-offset nms
                         top-n maximum-detections)
		   model-names)
	      (map (lambda (model-name)
		    (list (read-voc4-detector-boxes video model-name) model-name))
		   model-names))))
	(klt-movie (time-code (if cuda-klt?
				  (run-cuklt-with-defaults video)
				  (read-klt-movie video))))
	(optical-flow-movie (time-code
			     (if cuda-optical-flow?
				 (cuda-optical-flow video)
				 (read-optical-flow-movie-in-c video))))
	(tracks
	 (remove
	  #f
	  (map
	   (lambda (model-list)
	    (time-code
	     (viterbi-track video klt-movie optical-flow-movie
			    (map (lambda (model-name)
				  (find-if (lambda (x) (equal? (second x) model-name))
					   detector-boxes-movie-model-name-pairs))
				 model-list)
			    top-n with-dt? model-path
			    model-threshold-tracker-offset
			    profile-best-boxes?
			    look-ahead
			    minimum-track-length
			    maximum-track-overlap-ratio
			    overgeneration-minimum-track-length
			    overgeneration-maximum-track-overlap-ratio
			    suppression-delta
			    alpha beta
			    max-tracks)))
	   model-lists))))
  (for-each (o free c-optical-flow-handle) optical-flow-movie)
  tracks))

(define (viterbi-track-group-in-memory
	 video klt-movie optical-flow-movie
	 model-group detector-boxes-movies model-names
	 nms alpha beta top-n with-dt? model-path model-threshold-tracker-offset
	 profile-best-boxes?
	 look-ahead
	 minimum-track-length
	 maximum-track-overlap-ratio
	 overgeneration-minimum-track-length
	 overgeneration-maximum-track-overlap-ratio
	 suppression-delta max-tracks)
 (viterbi-track video klt-movie optical-flow-movie
		(map (lambda (model-name)
		      (find-if (lambda (x) (equal? model-name (second x)))
			       (zip detector-boxes-movies model-names)))
		     model-group)
		top-n with-dt? model-path
		model-threshold-tracker-offset
		profile-best-boxes?
		look-ahead
		minimum-track-length
		maximum-track-overlap-ratio
		overgeneration-minimum-track-length
		overgeneration-maximum-track-overlap-ratio
		suppression-delta
		alpha beta
		max-tracks))

;;; Sentences

(define *class->noun*
 '(("bag" "bag")
   ("baseball-bat" "bat")
   ("bench" "bench")
   ("bicycle" "bicycle")
   ("big-ball" "ball")
   ("bucket" "bucket")
   ("cage" "cage")
   ("cardboard-box" "box")
   ("car" "car")
   ("cart" "cart")
   ("chair" "chair")
   ("closet" "closet")
   ("dog" "dog")
   ("door" "door")
   ("garbage-can" "can")
   ("golf-club" "club")
   ("ladder" "ladder")
   ("mailbox" "mailbox")
   ("microwave" "microwave")
   ("motorcycle" "motorcycle")
   ("person-crouch" "person")
   ("person-down" "person")
   ("person" "person")
   ("pogo-stick" "pogo-stick")
   ("rake" "rake")
   ("shovel" "shovel")
   ("skateboard" "skateboard")
   ("small-ball" "ball")
   ("suv" "SUV")
   ("table" "table")
   ("toy-truck" "truck")
   ("trailer" "trailer")
   ("trash-bag" "bag")
   ("tripod" "tripod")
   ("truck" "truck")
   ("gun" "gun")
   ("ball" "ball")
   ("sign" "sign")
   ("giraffe" "giraffe")))

(define *class->restrictive-adjective*
 '(("baseball-bat" "baseball")
   ("cardboard-box" "cardboard")
   ("garbage-can" "garbage")
   ("golf-club" "golf")
   ("toy-truck" "toy")
   ("trash-bag" "trash")))

(define *class->size-adjective*
 '(("big-ball" "big")
   ("small-ball" "small")))

(define *class->size-adjective-bounds*
 '(("bag" (0.007 0.016))
   ;; ("baseball-bat" ())
   ;; ("bench" ())
   ("bicycle" (0.02 0.03))
   ;; ("bucket" ())
   ;; ("cage" ())
   ("cardboard-box" (0.009 0.016))
   ("car" (0.1 0.2))
   ;; ("cart" ())
   ;; ("chair" ())
   ;; ("closet" ())
   ("dog" (0.007 0.02))
   ("door" (0.016 0.05))
   ;; ("garbage-can" ())
   ;; ("golf-club" ())
   ;; ("ladder" ())
   ;; ("mailbox" ())
   ;; ("microwave" ())
   ("motorcycle" (0.08 0.18))
   ("person" (0.1 0.18))
   ;; ("pogo-stick" ())
   ;; ("rake" ())
   ;; ("shovel" ())
   ("skateboard" (0.007 0.014))
   ("suv" (0.2 0.5))
   ("table" (0.15 0.22))
   ("toy-truck" (0.1 0.15))
   ;; ("trailer" ())
   ;; ("trash-bag" ())
   ;; ("tripod" ())
   ("truck" (0.2 0.53))
   ("gun" (0.2 0.5))
   ("ball" (0.009 0.016))
   ("sign" (0.009 0.016))
   ("giraffe" (0.009 0.016))))

(define *class->shape-adjective-bound*
 '(("bag" 0.89)
   ;; ("baseball-bat" ())
   ;; ("bench" ())
   ("bicycle" 1.47)
   ;; ("bucket" ())
   ;; ("cage" ())
   ("cardboard-box" 0.97)
   ("car" 1.48)
   ;; ("cart" ())
   ;; ("chair" ())
   ;; ("closet" ())
   ("dog" 1.12)
   ("door" 0.42)
   ;; ("garbage-can" ())
   ;; ("golf-club" ())
   ;; ("ladder" ())
   ;; ("mailbox" ())
   ;; ("microwave" ())
   ("motorcycle" 1.43)
   ("person" 0.71)
   ;; ("pogo-stick" ())
   ;; ("rake" ())
   ;; ("shovel" ())
   ("skateboard" 2.11)
   ("suv" 1.47)
   ("table" 1.23)
   ("toy-truck" 1.08)
   ;; ("trailer" ())
   ;; ("trash-bag" ())
   ;; ("tripod" ())
   ("truck" 1.47)
   ("gun" 1.47)
   ("ball" 0.97)
   ("sign" 0.97)
   ("giraffe" 0.97)))

(define *verbs*
 '(("approach" "approached" required preverbal-adverb exogenous-motion-pp)
   ("arrive" "arrived" not-allowed postverbal-adverb exogenous-motion-pp)
   ("attach" "attached" required (before "an" "object" "to")
    (default "themselves") preverbal-adverb)
   ("bounce" "bounced" not-allowed postverbal-adverb endogenous-motion-pp)
   ("bury" "buried" required)
   ("carry" "carried" required preverbal-adverb endogenous-motion-pp)
   ("catch" "caught" required exogenous-motion-pp)
   ("chase" "chased" required preverbal-adverb endogenous-motion-pp)
   ("close" "closed" required)
   ("collide" "collided" required (before "with") preverbal-adverb
    exogenous-motion-pp)
   ("dig" "digging" optional (before "with") (restriction "rake" "shovel") aux)
   ("drop" "dropped" required)
   ("enter" "entered" required
    (conditional-default "something" "car" "suv" "truck" "door")
    preverbal-adverb endogenous-motion-pp)
   ("exchange" "exchanged" required (before "an" "object" "with")
    preverbal-adverb)
   ("exit" "exited" required
    (conditional-default "something" "car" "suv" "truck" "door")
    preverbal-adverb endogenous-motion-pp)
   ("fall" "fell" optional (before "because" "of") postverbal-adverb
    endogenous-motion-pp)
   ("flee" "fled" optional (before "from") postverbal-adverb
    endogenous-motion-pp)
   ("fly" "flew" not-allowed postverbal-adverb endogenous-motion-pp)
   ("follow" "followed" required preverbal-adverb endogenous-motion-pp)
   ("get" "got" required (before "an" "object" "from"))
   ("give" "gave" required (before "an" "object" "to"))
   ("go" "went" not-allowed (before "away") postverbal-adverb
    endogenous-motion-pp)
   ("hand" "handed" required (after "an" "object"))
   ("haul" "hauled" required preverbal-adverb endogenous-motion-pp)
   ("have" "had" required)
   ("hit" "hit" required
    (conditional-before ("something" "with") ("golf-club" "baseball-bat")))
   ("hold" "held" required)
   ("jump" "jumped" optional (before "over") postverbal-adverb
    endogenous-motion-pp)
   ("kick" "kicked" required preverbal-adverb endogenous-motion-pp)
   ("leave" "left" not-allowed postverbal-adverb endogenous-motion-pp)
   ("lift" "lifted" required preverbal-adverb)
   ("move" "moved" required (default "itself") preverbal-adverb
    endogenous-motion-pp)
   ("open" "opened" required)
   ("pass" "passed" required preverbal-adverb exogenous-motion-pp)
   ("pick up" "picked" required (after "up"))
   ("push" "pushed" required preverbal-adverb endogenous-motion-pp)
   ("put down" "put" required (after "down"))
   ("raise" "raised" required (default "themselves"))
   ("receive" "received" required
    (conditional-before ("an" "object" "from") ("person" "mailbox")))
   ("replace" "replaced" required preverbal-adverb)
   ("run" "ran" optional (before "to") postverbal-adverb endogenous-motion-pp)
   ("snatch" "snatched" required (before "an" "object" "from") preverbal-adverb)
   ("stop" "stopped" optional preverbal-adverb)
   ("take" "took" required (before "an" "object" "from") preverbal-adverb)
   ("throw" "threw" required preverbal-adverb endogenous-motion-pp)
   ("touch" "touched" required)
   ("turn" "turned" not-allowed endogenous-motion-pp)
   ("walk" "walked" optional (before "to") postverbal-adverb
    endogenous-motion-pp)))

(define *prepositions*
 '((right "to" "the" "left" "of")
   (above-right "below" "and" "to" "the" "left" "of")
   (above "below")
   (above-left "below" "and" "to" "the" "right" "of")
   (left "to" "the" "right" "of")
   (below-left "above" "and" "to" "the" "right" "of")
   (below "above")
   (below-right "above" "and" "to" "the" "left" "of")))

(define *endogenous-motion-pps*
 '((none)
   (right "rightward")
   (above-right "rightward" "and" "upward")
   (above "upward")
   (above-left "leftward" "and" "upward")
   (left "leftward")
   (below-left "leftward" "and" "downward")
   (below "downward")
   (below-right "rightward" "and" "downward")))

(define *exogenous-motion-pps*
 '((none)
   (right "from" "the" "left")
   (above-right "from" "below" "and" "to" "the" "left")
   (above "from" "below")
   (above-left "from" "below" "and" "to" "the" "right")
   (left "from" "the" "right")
   (below-left "from" "above" "and" "to" "the" "right")
   (below "from" "above")
   (below-right "from" "above" "and" "to" "the" "left")))

(define *minimum-velocity* 3)

(define *motion-threshold* 70)

(define (hyphens->spaces string)
 (list->string (map (lambda (char) (if (char=? char #\-) #\space char))
		    (string->list string))))

(define (angle->direction a)
 (cond ((<= (- (/ half-pi 4)) a (/ half-pi 4)) 'right)
       ((<= (/ half-pi 4) a (* 3 (/ half-pi 4))) 'above-right)
       ((<= (* 3 (/ half-pi 4)) a (* 5 (/ half-pi 4))) 'above)
       ((<= (* 5 (/ half-pi 4)) a (* 7 (/ half-pi 4))) 'above-left)
       ((or (<= (- pi) a (* 7 (- (/ half-pi 4))))
	    (<= (* 7 (/ half-pi 4)) a pi))
	'left)
       ((<= (* 3 (- (/ half-pi 4))) a (- (/ half-pi 4))) 'below-left)
       ((<= (* 5 (- (/ half-pi 4))) a (* 3 (- (/ half-pi 4)))) 'below)
       ((<= (* 7 (- (/ half-pi 4))) a (* 5 (- (/ half-pi 4)))) 'below-right)
       (else (fuck-up))))

(define (overall-distance track)
 (distance (voc4-detection-center (last track))
	   (voc4-detection-center (first track))))

(define (track-motion track)
 (let ((velocity-vectors
	(remove-if-vector
	 (lambda (velocity-vector)
	  (< (vector-ref velocity-vector 0) *minimum-velocity*))
	 (map-vector (lambda (feature-vector)
		      (vector
		       ;; magnitude
		       (vector-ref feature-vector 4)
		       ;; direction
		       (vector-ref feature-vector 5)))
		     (one-track-features 'non-pose track 2 #f)))))
  (if (zero? (vector-length velocity-vectors))
      '#(0 0)
      (k*v (/ (vector-length velocity-vectors))
	   (map-reduce-vector
	    v+ '#(0 0) polar->rect velocity-vectors)))))

(define (generate-adverb track)
 (if track
     (let ((velocity-vector (track-motion track)))
      ;; hardcoded values given profiling 14Mar2011
      (cond ((<= *minimum-velocity*
		 (magnitude velocity-vector)
		 (+ *minimum-velocity* 2))
	     "slowly")
	    ((> (magnitude velocity-vector) (+ *minimum-velocity* 4))
	     "quickly")
	    (else '())))
     '()))

(define (generate-direction track)
 (if track
     (let* ((velocity-vector (track-motion track)))
      (if (< (magnitude velocity-vector) *minimum-velocity*)
	  'none
	  (angle->direction (orientation velocity-vector))))
     'none))

(define (get-before lexical-entry)
 (if (some (lambda (feature)
	    (and (list? feature) (eq? (first feature) 'before)))
	   lexical-entry)
     (rest (find-if (lambda (feature)
		     (and (list? feature) (eq? (first feature) 'before)))
		    lexical-entry))
     '()))

(define (class track)
 (let ((class
	(voc4-detection-model
	 (find-if (lambda (box)
		   (and (voc4-detection-model box)
			(not (string=? (voc4-detection-model box) "padding"))
			(not (string=? (voc4-detection-model box) "."))))
		  track))))
  (if (or (string=? class "person-crouch") (string=? class "person-down"))
      "person"
      class)))

(define (get-conditional-before lexical-entry patient-track)
 (if (some (lambda (feature)
	    (and (list? feature) (eq? (first feature) 'conditional-before)))
	   lexical-entry)
     (let ((feature (find-if (lambda (feature)
			      (and (list? feature)
				   (eq? (first feature) 'conditional-before)))
			     lexical-entry)))
      (if (and patient-track (member (class patient-track) (third feature)))
	  (second feature)
	  '()))
     '()))

(define (get-after lexical-entry)
 (if (some (lambda (feature)
	    (and (list? feature) (eq? (first feature) 'after)))
	   lexical-entry)
     (rest (find-if (lambda (feature)
		     (and (list? feature) (eq? (first feature) 'after)))
		    lexical-entry))
     '()))

(define (get-restriction lexical-entry)
 (if (some (lambda (feature)
	    (and (list? feature) (eq? (first feature) 'restriction)))
	   lexical-entry)
     (rest (find-if (lambda (feature)
		     (and (list? feature) (eq? (first feature) 'restriction)))
		    lexical-entry))
     #f))

(define (get-default lexical-entry)
 (if (some (lambda (feature)
	    (and (list? feature) (eq? (first feature) 'default)))
	   lexical-entry)
     (second (find-if (lambda (feature)
		       (and (list? feature) (eq? (first feature) 'default)))
		      lexical-entry))
     #f))

(define (get-conditional-default lexical-entry patient-track np)
 (if (some (lambda (feature)
	    (and (list? feature) (eq? (first feature) 'conditional-default)))
	   lexical-entry)
     (let ((feature (find-if (lambda (feature)
			      (and (list? feature)
				   (eq? (first feature) 'conditional-default)))
			     lexical-entry)))
      (if (and patient-track
	       (member (class patient-track) (rest (rest feature))))
	  np
	  (second feature)))
     np))

(define (refered-to-in-subject? track subject-track)
 (and track
      (not (eq? (generate-direction subject-track) 'none))
      (< (magnitude (track-motion track)) *motion-threshold*)
      (< (magnitude (track-motion subject-track)) *motion-threshold*)))



(define (dummy-box? box)
 (and (= (voc4-detection-x1 box) -1) (= (voc4-detection-y1 box) -1)
      (= (voc4-detection-x2 box) -1) (= (voc4-detection-y2 box) -1)))

(define *maximum-score-variance* 0.1)
(define *maximum-size-variance* 0.15)
(define *maximum-shape-variance* 0.13)

(define (estimate-size-and-shape track)
 (define (closer v l u) (if (< (- v l) (- u v))l u))
 (let* ((track (remove-if dummy-box? track))
	(aspect-ratios (map voc4-detection-aspect-ratio track))
	(areas (map voc4-detection-area track))
	(strength-variance (list-variance (map voc4-detection-strength track)))
	(class (class track)))
  (if (and (<= strength-variance *maximum-score-variance*)
	   (<= (list-variance areas) *maximum-size-variance*)
	   (<= (list-variance aspect-ratios) *maximum-shape-variance*))
      (let* ((size (/ (list-mean areas) (* 1280 720))) ; relative to image
	     (ht (/ (list-mean (map voc4-detection-height track)) 720)) ; relative to image
	     (wd (/ (list-mean (map voc4-detection-width track)) 1280)) ; relative to image
	     (ar (list-mean aspect-ratios))
	     (size-bounds (second (assoc class *class->size-adjective-bounds*)))
	     (min-size (first size-bounds))
	     (max-size (second size-bounds))
	     (c-ar (second (assoc class *class->shape-adjective-bound*))))
       (list (cond ((< size min-size) "small")
		   ((> size max-size) "big")
		   (else '()))
	     (cond ((and (<= ar (* 0.7 c-ar))
			 (or (> size max-size) (= (closer size min-size max-size) max-size)))
		    "tall")
		   ((and (>= ar (* 1.3 c-ar))
			 (or (> size max-size) (= (closer size min-size max-size) max-size)))
		    "wide")
		   ((and (<= ar (* 0.7 c-ar))
			 (or (< size min-size) (= (closer size min-size max-size) min-size)))
		    "narrow")
		   ((and (>= ar (* 1.3 c-ar))
			 (or (< size min-size) (= (closer size min-size max-size) min-size)))
		    "short")
		   (else '()))))
      '())))

;; (define (dtracef s f v) (format #t "~a: ~a~%" s (f v)) v)

(define (most-frequent-class track)
 (caar (sort (transitive-equivalence-classesp
	      equal?
	      (map
	       voc4-detection-model
	       (remove-if (lambda (box)
			   (or (not (voc4-detection-model box))
			       (string=? (voc4-detection-model box) "padding")
			       (string=? (voc4-detection-model box) ".")))
			  track)))
	     >
	     length)))

(define (generate-adjectives kind subject-track object-track)
 ;; needs work
 ;;  adjectives
 ;;   color
 ;;   aspect ratio
 ;;   size
 ;;  only generate color if needed to prevent coreference of nonpersons
 ;;  order: other, size, shape, color, restrictive modifiers
 (let* ((track (case kind
		((subject) subject-track)
		((reference object) object-track)
		(else (fuck-up))))
	(subject-class (most-frequent-class subject-track))
	(object-class (if object-track (most-frequent-class object-track) #f))
	(class (case kind
		((subject) subject-class)
		((reference object) object-class)
		(else (fuck-up))))
	(proper-class (if (or (equal? class "person-crouch") (equal? class "person-down"))
			  "person"
			  class)))
  (list (if (and (not (eq? kind 'subject))
		 object-track
		 (equal? subject-class object-class))
	    "other"
	    '())
	;; size and shape
	(if (assoc proper-class *class->size-adjective*)
	    (second (assoc proper-class *class->size-adjective*))
	    (estimate-size-and-shape track))
	;; colour (useless and expensive)
	(if (assoc proper-class *class->restrictive-adjective*)
	    (second (assoc proper-class *class->restrictive-adjective*))
	    '())
	(if (and (string=? proper-class "person")
		 object-class
		 (not (equal? subject-class object-class)))
	    (cond ((string=? class "person") "upright")
		  ((string=? class "person-crouch") "crouched")
		  ((string=? class "person-down") "prone")
		  (else (fuck-up)))
	    '()))))

(define (generate-vp verb subject-track object-track)
 (let ((lexical-entry (assoc (hyphens->spaces verb) *verbs*)))
  (list
   (if (member 'aux lexical-entry) "was" '())
   (if (member 'preverbal-adverb lexical-entry)
       (generate-adverb subject-track)
       '())
   (second lexical-entry)
   (if (member 'postverbal-adverb lexical-entry)
       (generate-adverb subject-track)
       '())
   (case (third lexical-entry)
    ((not-allowed)
     (list (get-before lexical-entry)
	   (get-after lexical-entry)
	   (cond
	    ((member 'endogenous-motion-pp lexical-entry)
	     (rest (assq (generate-direction subject-track) *endogenous-motion-pps*)))
	    ((member 'exogenous-motion-pp lexical-entry)
	     (rest (assq (generate-direction subject-track) *exogenous-motion-pps*)))
	    (else '()))))
    ((optional)
     (if (or (not (get-restriction lexical-entry))
	     (and object-track
		  (member (class object-track)
			  (get-restriction lexical-entry))))
	 (list (if (member 'endogenous-motion-pp lexical-entry)
		   (rest (assq (generate-direction subject-track) *endogenous-motion-pps*))
		   '())
	       (get-before lexical-entry)
	       (get-conditional-before lexical-entry object-track)
	       (get-conditional-default
		lexical-entry
		object-track
		(generate-object-np
		 subject-track object-track (get-default lexical-entry)))
	       (get-after lexical-entry)
	       (if (member 'exogenous-motion-pp lexical-entry)
		   (rest (assq (generate-direction subject-track) *exogenous-motion-pps*))
		   '()))
	 '()))
    ((required)
     (list (get-before lexical-entry)
	   (get-conditional-before lexical-entry object-track)
	   (get-conditional-default
	    lexical-entry
	    object-track
	    (generate-object-np
	     subject-track object-track (get-default lexical-entry)))
	   (get-after lexical-entry)
	   (cond
	    ((member 'endogenous-motion-pp lexical-entry)
	     (rest (assq (generate-direction subject-track) *endogenous-motion-pps*)))
	    ((member 'exogenous-motion-pp lexical-entry)
	     (rest (assq (generate-direction subject-track) *exogenous-motion-pps*)))
	    (else '()))))
    (else (fuck-up))))))

(define (flatten tree)
 (if (string? tree)
     tree
     (reduce (lambda (s1 s2) (string-append s1 " " s2))
	     (remove "" (map flatten tree))
	     "")))

(define (sententify string)
 (let ((characters (string->list string)))
  (string-append
   (list->string (cons (char-upcase (first characters)) (rest characters)))
   ".")))

(define (server-specific-track-pathname server video name)
 (generic-full-pathname (format #f "/net/~a/aux/qobi/video-datasets" server)
			video
			(string-append "/" name)))



;;; Tracking, likelihoods and sentences
(define (show-result-mostly port r)
 (format port "#(RESULT ~a ~a ~a ~a ~a ~a ...)~%"
	 (result-video-name r)
	 (result-track-names r)
	 (result-features-type r)
	 (result-states r)
	 (result-verb r)
	 (result-loglk r)
	 ;; (result-names-tracks r)
	 ))

(define (result->sentence video result)
 (define (name-of named)
  (if (list? (first named)) (second (first named)) (first named)))
 (unless *quiet-mode?* (show-result-mostly #t result))
 (list
  (sententify
   (flatten
    (generate-sentence
     (result-verb result)
     (second (first (result-named-tracks result)))
     (if (< (length (result-named-tracks result)) 2)
	 #f
	 (second (second (result-named-tracks result)))))))
  (result-loglk result)
  (result-verb result)
  (result-named-tracks result)))

(define (video->sentences
	 video cuda-klt? cuda-optical-flow? cuda-object-detector?
	 model-names-groups final-cascade-adjustment threshold-offset
	 nms alpha beta top-n maximum-detections with-dt? model-path look-ahead
	 minimum-track-length maximum-track-overlap-ratio
	 model-threshold-tracker-offset overgeneration-minimum-track-length
	 overgeneration-maximum-track-overlap-ratio suppression-delta
	 event-models max-sentences max-tracks)
 (sort
  (map
   (lambda (result)
    (let ((sentence (result->sentence video result)))
     (list (first sentence)
	   (second sentence)
	   result)))
   (take-if-possible
    max-sentences
    (join
     (time-code
      (compute-likelihoods
       video
       (time-code
	(join
	 (map (lambda (track-name tracks)
	       (map (lambda (track) (list track-name track)) tracks))
	      (map first model-names-groups)
	      (map (lambda (a) (vector-ref a 4))
		   (read-and-viterbi-track
		    video cuda-klt? cuda-optical-flow? cuda-object-detector?
		    (if model-names-groups
			model-names-groups
			(get-cleaned-up-model-names-list video))
		    nms alpha beta final-cascade-adjustment threshold-offset top-n
		    maximum-detections
		    with-dt? model-path model-threshold-tracker-offset
		    #f look-ahead
		    minimum-track-length
		    maximum-track-overlap-ratio
		    overgeneration-minimum-track-length
		    overgeneration-maximum-track-overlap-ratio
		    suppression-delta
		    max-tracks)))))
       (map read-hmm event-models)
       #f
       (lambda (a b c d) (d))
       #f)))))
  >
  second))

(define (video->sentences-with-cuda-defaults video model-names-groups
                                             event-models
					     max-tracks)
 (video->sentences video #t #t #t model-names-groups 0 0 0 10 3 3 20000 #f
		   (string-append (getenv "HOME") "/video-datasets/C-D1/voc4-models/")
		   5 10 0.7 -0.4 0 0.1 0.1 event-models 10
		   max-tracks))

(define (generate-sentence verb agent-track patient-track)
 ;; switch? is mapping from semantic role to syntactic position.
 (let* ((switch? (and patient-track
		      (or (string=? verb "approach") (string=? verb "flee"))
		      (< (overall-distance agent-track) *motion-threshold*)
		      (>= (overall-distance patient-track)
			  ;; needs work: change 0.5 to >1
			  (* 0.5 (overall-distance agent-track)))))
	(subject-track (if switch? patient-track agent-track))
	(object-track (if switch? agent-track patient-track)))
  (list (generate-subject-np subject-track object-track)
	(generate-vp verb subject-track object-track))))

(define (assoc-checked e l)
 (unless (assoc e l)
  (panic "Object lookup failed ~a ~a~%" e l))
 (assoc e l))

(define (generate-reference-np subject-track object-track)
 (list (generate-determiner 'reference subject-track object-track)
       (generate-adjectives 'reference subject-track object-track)
       (second (assoc-checked (class object-track) *class->noun*))))

(define (generate-subject-np subject-track object-track)
 (list
  (generate-determiner 'subject subject-track object-track)
  (generate-adjectives 'subject subject-track object-track)
  (second (assoc-checked (class subject-track) *class->noun*))
  (if (refered-to-in-subject? object-track subject-track)
      (list (rest (assq (generate-direction subject-track) *prepositions*))
	    (generate-reference-np subject-track object-track))
      '())))

(define (generate-object-np subject-track object-track default)
 (cond
  (object-track
   (list (generate-determiner 'object subject-track object-track)
 	 (generate-adjectives 'object subject-track object-track)
 	 (second (assoc-checked (class object-track) *class->noun*))))
  (default (cond ((and (string=? default "itself")
		       (string=? (class subject-track) "person"))
		  "themselves")
		 ((and (string=? default "themselves")
		       (not (string=? (class subject-track) "person")))
		  "itself")
		 (else default)))
  (else "something")))

(define (generate-determiner kind subject-track object-track)
 ;; needs work: general coreference predicate
 ;;             other doesn't enter into this
 (cond
  ((not (and object-track
	     (equal? (class subject-track) (class object-track))))
   "the")
  ((and (eq? kind 'object) (refered-to-in-subject? object-track subject-track))
   "that")
  (else "some")))

(define (result->tracks result) (result-named-tracks result))

(define (annotated-models)
 (append
  (if (file-exists?
       (string-append (getenv "HOME")
		      "/darpa-collaboration/documentation/C-D1-recognition-annotations.csv"))
      (map (o fields swap-commas-and-spaces)
	   (read-file
	    (string-append (getenv "HOME")
			   "/darpa-collaboration/documentation/C-D1-recognition-annotations.csv")))
      '())
  (if (file-exists? (string-append
		     (getenv "HOME")
		     "/darpa-collaboration/documentation/C-E1-description-annotations.csv"))
      (map (o fields swap-commas-and-spaces)
	   (read-file (string-append (getenv "HOME")
				     "/darpa-collaboration/documentation/C-E1-description-annotations.csv")))
      '())
  (if (file-exists?
       (string-append (getenv "HOME")
		      "/darpa-collaboration/documentation/C-E1-minus-C-D1-annotations.csv"))
      (map (o fields swap-commas-and-spaces)
	   (read-file
	    (string-append (getenv "HOME")
			   "/darpa-collaboration/documentation/C-E1-minus-C-D1-annotations.csv")))
      '())))

(define (annotated-models-for-video video)
 (let ((video-annotation
	(find-if (lambda (annotation) (equal? (first annotation) (any-video->string video)))
		 (annotated-models))))
  (if video-annotation
      (let ((models (map first (group-nth (drop 2 video-annotation) 4))))
       (for-each (lambda (model) (unless (file-exists? (cuda-model-pathname model))
			     (format #t "Ignoring annotated model ~a because it lacks a trained model files~%" model)))
		 models)
       (map (lambda (ms) (if (list? ms) ms (list ms)))
	    (let ((models (remove-if-not (o file-exists? cuda-model-pathname) models)))
	     (if (find "person" models)
		 (cons '("person" "person-crouch" "person-down") (remove "person" models))
		 models))))
      #f)))

(define (scale-detector-output list-of-list-voc4-boxes from-video-width-to-1280)
 (map (lambda (list-voc4-boxes)
       (map (lambda (voc4-box)
             (voc4-scale-abs voc4-box from-video-width-to-1280))
            list-voc4-boxes)) list-of-list-voc4-boxes))
