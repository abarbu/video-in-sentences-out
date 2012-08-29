(MODULE
  UPGRADE-VIDEO
  (WITH
    QOBISCHEME
    XLIB
    CUPEDRO-BINDINGS
    HMM-TRAIN-CLASSIFY
    HMM-WBM
    IDEALIB-HASH-TABLE
    IDEALIB-MATPLOTLIB
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    TOOLLIB-C-BINDINGS
    TOOLLIB-CAMERA
    TOOLLIB-HACK-TRACK-DRAWABLE
    TOOLLIB-HACK-TRACK
    TOOLLIB-IMAGE-PROCESSING
    TOOLLIB-MATLAB
    TOOLLIB-MISC)
  (MAIN MAIN))
;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "upgrade-video.sch")

(set! *program* "upgrade-video")
(set! *panic?* #f)

(include "toollib-c-macros.sch")
(c-include "idealib-c.h")

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;; darpa-wrap ./upgrade-video -darpa CHASE3_A1_C1_Act2_6_URBAN_MC_AFTN_b436e7cc-07b6-11e0-a6d4-e80688cb869a
;; darpa-wrap ./upgrade-video -standalone CHASE3_A1_C1_Act2_6_URBAN_MC_AFTN_b436e7cc-07b6-11e0-a6d4-e80688cb869a
;; BOUNCE10_A1_C1_Act8_URBAN_MC_AFTN_DARK_43797fda-1dc6-11e0-ad1b-e80688ca39a2

;;; ---------------------------------------------------------------------------
;;; removed from idealib-stuff because of addition of ringing to voc4-detection
;;; also, they are legacy

(define (read-single-boxes video-name filename)
 (map car (read-boxes video-name filename)))

(define (read-boxes video-name filename)
 (if (file-exists? filename)
     (map (lambda (box-strings)
	   (map string->voc4-inexact-box box-strings))
	  (unpack-consolidated-frames (read-file filename)))
     (list (map-n (lambda _ (make-voc4-detection -1 -1 -1 -1 '() -1 minus-infinity 0 minus-infinity #f))
		  (video-length video-name)))))
(define (read-voc4-boxes file)
 (map quantize-voc4-detection (read-voc4-inexact-boxes file)))

(define (read-voc4-inexact-boxes file)
 (map (lambda (s) (string->voc4-inexact-box s))
      (remove-if (lambda (a) (= (number-of-fields a) 0)) (read-file file))))

(define (string->voc4-inexact-box string)
 (define (string->number-or-negative-inf str)
  (if (string->number str) (string->number str) (- infinity)))
 (let ((e (list->vector (fields string))))
  (cond ((< 5 (vector-length e) 9)
	 (make-voc4-detection
	  (string->number (vector-ref e 0)) (string->number (vector-ref e 1))
	  (string->number (vector-ref e 2)) (string->number (vector-ref e 3))
	  '()
	  (string->number (vector-ref e 4))
	  (string->number-or-negative-inf (vector-ref e 5))
	  (if (> (vector-length e) 6) (string->number (vector-ref e 6)) 0)
	  (if (> (vector-length e) 7) (vector-ref e 7) "padding")))
	((= (modulo (vector-length e) 4) 2)
	 (make-voc4-detection
	  (string->number (vector-ref e 0)) (string->number (vector-ref e 1))
	  (string->number (vector-ref e 2)) (string->number (vector-ref e 3))
	  (let loop ((i 4))
	   (if (< (- (vector-length e) i) 4)
	       '()
	       (cons (map string->number
			  (list (vector-ref e i) (vector-ref e (+ i 1))
				(vector-ref e (+ i 2)) (vector-ref e (+ i 3))))
		     (loop (+ i 4)))))
	  (string->number (vector-ref e (- (vector-length e) 2)))
	  (string->number-or-negative-inf
	   (vector-ref e (- (vector-length e) 1)))
	  0
	  "padding"))
	((= (modulo (vector-length e) 4) 0)
	 (make-voc4-detection
	  (string->number (vector-ref e 0)) (string->number (vector-ref e 1))
	  (string->number (vector-ref e 2)) (string->number (vector-ref e 3))
	  (let loop ((i 4))
	   (if (< (- (vector-length e) i) 4)
	       '()
	       (cons (map string->number
			  (list (vector-ref e i) (vector-ref e (+ i 1))
				(vector-ref e (+ i 2)) (vector-ref e (+ i 3))))
		     (loop (+ i 4)))))
	  (string->number (vector-ref e (- (vector-length e) 4)))
	  (string->number-or-negative-inf
	   (vector-ref e (- (vector-length e) 3)))
	  (string->number (vector-ref e (- (vector-length e) 2)))
	  (vector-ref e (- (vector-length e) 1))))
	(else (fuck-up)))))

(define (string->voc4-exact-box string)
 (quantize-voc4-detection (string->voc4-inexact-box string)))

;;; ---------------------------------------------------------------------------

(define (voc4-write-boxes-movie video boxes-movie name)
 (write-file
  (join
   (map
    (lambda (boxes)
     (let ((boxes (remove-if-not nondropped-box? boxes)))
      (cons (number->string (length boxes)) (map voc4-box->string boxes))))
    boxes-movie))
  (generic-root-pathname video name)))

(define (voc4-write-box-movies video box-movies name)
 (for-each-indexed
  (lambda (box-movie i)
   (write-file
    (join (map (lambda (box) (list "1" (voc4-box->string box))) box-movie))
    (generic-root-pathname video name)))
  box-movies))

(define (pre-zip-string->klt string i next?)
 (let ((string (list->string
		(map (lambda (char)
		      (cond ((char=? char #\|) #\space)
			    ((char=? char #\,) #\space)
			    ((char=? char #\=) #\space)
			    ((char=? char #\() #\space)
			    ((char=? char #\)) #\space)
			    (else char)))
		     (string->list string)))))
  (unless (= (string->number (field-ref string 0)) i) (panic "pre-zip-string->klt"))
  (if (and next? (not (zero? (string->number (field-ref string 3)))))
      #f
      (vector (string->number (field-ref string 1))
	      (string->number (field-ref string 2))))))

(define (pre-zip-read-klt-file video frame name next?)
 (let ((strings (read-file (generic-pathname video frame name))))
  (unless (string=? (field-ref (list-ref strings 10) 0) "nFeatures")
   (panic "pre-zip-read-klt-file"))
  (let ((n (string->number (field-ref (list-ref strings 10) 2))))
   (map-indexed (lambda (string i) (pre-zip-string->klt string i next?))
		(sublist strings 14 (length strings))))))

(define (pre-zip-read-klt-movie video)
 (if (file-exists?
      (generic-full-pathname *video-pathname* video "/klt.txt"))
     (let* ((dummy (format #t "reading per-klt-video...~%"))
	   (feature-movie
	    (call-with-input-file (generic-full-pathname
				   *video-pathname* video "/klt.txt")
	     (lambda (port)
	      (map-n
	       (lambda (frame-number)
		(map-n (lambda (feature-number)
			(let ((line (read-line port)))
			 (list (string->number (field-ref line 0))
			       (vector (string->number (field-ref line 1))
				       (string->number (field-ref line 2))))))
		       (string->number (read-line port))))
	       (string->number (read-line port)))))))
       (unless (= (length feature-movie) (video-last-frame video))
	       (panic "read-klt-movie"))
       (format #t "done reading per-klt-video.~%")
       (map (lambda (currents nexts)
	     (removeq #f
		      (map (lambda (next)
			    (let ((current
				   (find-if
				    (lambda (current)
				     (= (first current) (first next)))
				    currents)))
			     (if current
				 (make-klt-pair (second current) (second next))
				 #f)))
			   nexts)))
	    (but-last feature-movie)
	    (rest feature-movie)))
     (map-m-n
      (lambda (frame)
       (format #t "reading klt movie: ~s~%" frame)
       (let ((currents (pre-zip-read-klt-file video frame "klt-current.txt" #f))
	     (nexts (pre-zip-read-klt-file video frame "klt-next.txt" #t)))
	(unless (= (length currents) (length nexts))
	 (panic "KLT current and next have different numbers of features"))
	(remove-if (lambda (klt-pair) (not (klt-pair-next klt-pair)))
		   (map make-klt-pair currents nexts))))
      (video-first-frame video) (- (video-last-frame video) 1))))

(define (pre-zip-read-optical-flow-in-c video frame)
 (format #t "reading optical-flow movie: ~s~%" frame)
 (read-optical-flow-ssv-gz-in-c
  (generic-pathname
   video frame "optical-flow.ssv.gz") *optical-flow-height* *optical-flow-width*))

(define (pre-zip-read-optical-flow-movie-in-c video)
 (map-m-n (lambda (frame) (pre-zip-read-optical-flow-in-c video frame))
	  (video-first-frame video) (- (video-last-frame video) 1)))

(define (read-optical-flow-ssv-in-c filename height width)
 ((c-function pointer ("read_optical_flow_ssv" pointer unsigned unsigned))
  filename height width))

(define (read-optical-flow-ssv-gz-in-c filename height width)
 ((c-function pointer ("read_optical_flow_ssv_gz" pointer unsigned unsigned))
  filename height width))

(define (read-optical-flow-ssv filename height width)
 (let* ((c-flow (read-optical-flow-ssv-in-c filename height width))
	(flow (list (map-n-matrix
		     (lambda (i j)
		      (c-double-ref
		       c-flow
		       (* (c-sizeof "double") (+ j (* i width)))))
		     height width)
		    (map-n-matrix
		     (lambda (i j)
		      (c-double-ref
		       c-flow
		       (* (c-sizeof "double")
			  (+ (* width height) (+ j (* i width))))))
		     height width))))
  (free c-flow)
  flow))

(define (read-optical-flow-ssv-gz filename height width)
 (let* ((c-flow (read-optical-flow-ssv-gz-in-c filename height width))
	(flow (list (map-n-matrix
		     (lambda (i j)
		      (c-double-ref
		       c-flow
		       (* (c-sizeof "double") (+ j (* i width)))))
		     height width)
		    (map-n-matrix
		     (lambda (i j)
		      (c-double-ref
		       c-flow
		       (* (c-sizeof "double")
			  (+ (* width height) (+ j (* i width))))))
		     height width))))
  (free c-flow)
  flow))

(define (average-optical-flow-ssv-from-c ssv height width xl xh yl yh)
 (let* ((c-average-flow ((c-function pointer
				     ("average_optical_flow_ssv_from_c"
				      pointer
				      unsigned
				      unsigned
				      unsigned
				      unsigned
				      unsigned
				      unsigned))
			 ssv height width xl xh yl yh))
	(average-flow
	 (vector (c-double-ref c-average-flow (* (c-sizeof "double") 0))
		 (c-double-ref c-average-flow (* (c-sizeof "double") 1)))))
  (free c-average-flow)
  average-flow))

(define (average-optical-flow-ssv filename height width xl xh yl yh)
 (let* ((c-ssv (read-optical-flow-ssv-in-c filename height width))
	(average-flow
	 (average-optical-flow-ssv-from-c c-ssv height width xl xh yl yh)))
  (free c-ssv)
  average-flow))

(define (average-optical-flow-ssv-gz filename height width xl xh yl yh)
 (let* ((c-ssv (read-optical-flow-ssv-gz-in-c filename height width))
	(average-flow
	 (average-optical-flow-ssv-from-c c-ssv height width xl xh yl yh)))
  (free c-ssv)
  average-flow))

;;; Commands

;;; Top Level

(define (old-video-voc4-detector-boxes-available video)
 (map
  (lambda (pathname)
   (let ((parts (pregexp-split "-" (strip-extension (strip-directory pathname)))))
    (list (car parts)
	  (string-join "-" (take-if-possible 2 (cdr parts)))
	  #f
	  "boxes")))
  (directory-list (generic-pathname video (video-first-frame video) "/*.boxes"))))

(define (old-video-voc4-tracked-boxes-available video)
 (map
  (lambda (pathname)
   (let ((parts (pregexp-split "-" (strip-extension (strip-directory pathname)))))
    (list (car parts)
	 (string-join "-" (reverse (cdr (reverse (cdr parts)))))
	 (car (reverse parts))
	 "tracked_box")))
  (remove-if
   (lambda (fn) (pregexp-match "(.*)(crouch|down|wheelbarrow|crawl)(.*).tracked_box" fn))
   (directory-list (generic-pathname video (video-first-frame video) "/*.tracked_box")))))

(define (old-video-voc4-smooth-tracked-boxes-available video)
 (map
  (lambda (pathname)
   (let ((parts (pregexp-split "-" (strip-extension (strip-directory pathname)))))
    (list (car parts)
	  (string-join "-" (reverse (cdr (reverse (cdr parts)))))
	  (car (reverse parts))
	  "smooth_tracked_box")))
  (remove-if
   (lambda (fn) (pregexp-match "(.*)(crouch|down|wheelbarrow|crawl)(.*).smooth_tracked_box" fn))
   (directory-list (generic-pathname video (video-first-frame video) "/*.smooth_tracked_box")))))

(define (pre-zip-per-video-box-pathname video-name type label num ext)
 (generic-full-pathname
  *video-pathname* video-name
  (format #f "/~a-~a.~a"
	  type (if num (string*-append label "-" num) label)
	  ext)))


(define (filename video-name frame type label num ext)
 (generic-full-pathname
  *video-pathname* video-name
  (format #f "/~a/~a-~a.~a"
	  (number->padded-string-of-length frame 4)
	  type
	  (if num
	      (string*-append label "-" num)
	      label)
	  ext)))

(define (safe-read-boxes video-name frame type label num ext)
 (let ((filename (filename video-name frame type label num ext)))
  (if (file-exists? filename)
      (read-voc4-inexact-boxes filename)
      (begin (format #t "~a cannot be read!~%" filename) (fuck-up)))))

(define (safe-read-available-boxes video frame available)
 (safe-read-boxes video frame
		  (first available) (second available)
		  (if (third available)
		      (string->number (third available))
		      #f)
		  (fourth available)))

(define (upgrade-file-type video available rename?)
 (for-each
  (lambda (available)
   (voc4-write-boxes-movie
    video
    (map-frame
     (lambda (frame)
      (let ((boxes (safe-read-available-boxes video frame available)))
       (when rename?
	(for-each (lambda (a) (set-voc4-detection-model! a (second available))) boxes))
       boxes))
     video)
    (if (third available)
	(string*-append
	 (first available)  "-" (second available) "-" (third available) "." (fourth available))
	(string*-append
	 (first available)  "-" (second available) "." (fourth available)))))
  available))

(define (rm-upgraded video available)
 (for-each
  (lambda (available)
   (map-frame (lambda (frame)
	       (rm (filename video frame
			     (first available) (second available)
			     (if (third available) (string->number (third available)) #f)
			     (fourth available))))
	      video))
  available))

(define (is-upgraded? video available)
 (every
  (lambda (available)
   (file-exists?
    (generic-root-pathname
     video
     (if (third available)
	 (string*-append
	  (first available)  "-" (second available) "-" (third available) "." (fourth available))
	 (string*-append
	  (first available)  "-" (second available) "." (fourth available))))))
  available))

(define (upgrade-per-frame-to-per-video video if-needed? delete?)
 (let* ((available-detector (old-video-voc4-detector-boxes-available video))
	(available-tracked (old-video-voc4-tracked-boxes-available video))
	(available-smooth (old-video-voc4-smooth-tracked-boxes-available video))
	(available-predicted
	 (remove-duplicates (map (lambda (a) (append (take 2 a) (list #f "predicted_boxes")))
				 available-tracked))))
  (if (and if-needed?
	   (and (is-upgraded? video available-detector)
		(is-upgraded? video available-tracked)
		(is-upgraded? video available-smooth)
		(is-upgraded? video available-predicted)))
      (format #t "All boxes already upgraded~%")
      (begin
       (format #t "Processing the following detector boxes:~%")
       (pp available-detector)(newline)
       (format #t "Processing the following predicted boxes:~%")
       (pp available-predicted)(newline)
       (format #t "Processing the following tracked boxes:~%")
       (pp available-tracked)(newline)
       (format #t "Processing the following smooth tracked boxes:~%")
       (pp available-smooth)(newline)
       (format #t "Upgrading detectors~%")
       (upgrade-file-type video available-detector #t)
       (format #t "Upgrading tracked~%")
       (upgrade-file-type video available-tracked #f)
       (format #t "Upgrading smooth~%")
       (upgrade-file-type video available-smooth #f)
       (format #t "Upgrading predicted~%")
       (upgrade-file-type video available-predicted #f)
       (format #t "Done writing ~%")))
  (when delete?
   (rm-upgraded video available-detector)
   (rm-upgraded video available-tracked)
   (rm-upgraded video available-smooth)
   (rm-upgraded video available-predicted))))

(define (is-upgraded-zip? video available)
 (every
  (lambda (available)
   (file-exists?
    (generic-root-pathname
     video
     (string*-append
      (first available)  "-" (second available)
      (if (third available) (string*-append "-" (third available)) "")
      "." (fourth available) ".zip"))))
  available))

(define (upgrade-one-per-video-to-zip video-name type label num ext)
 (let ((zip-filename (per-video-box-pathname video-name type label num ext)))
  (rm-if-necessary zip-filename)
  (with-zip-file
   (lambda (zip2)
    (for-each-indexed
     (lambda (boxes frame)
      (zip:add-directory zip2 (number->padded-string-of-length
			       (+ (video-first-frame video-name) frame) 6))
      (zip:add-file zip2
		    (per-frame-boxes-in-zip-pathname
		     (+ (video-first-frame video-name) frame) type label num ext)
		    (map voc4-box->string boxes)))
     (pre-zip-fast-read-boxes
      (pre-zip-per-video-box-pathname video-name type label num ext))))
   zip-filename *zip:mode-create-new*)))

(define (upgrade-all-per-video-to-zip video-name if-needed? delete?)
 (let ((available-detector (pre-zip-video-voc4-detector-boxes-available video-name))
       (available-predicted (pre-zip-video-voc4-predicted-boxes-available video-name))
       (available-tracked (pre-zip-video-voc4-tracked-boxes-available video-name))
       (available-overgenerated
	(pre-zip-video-voc4-overgenerated-tracked-boxes-available video-name))
       (available-human
	(pre-zip-video-voc4-human-tracked-boxes-available video-name))
       (available-smooth
	(pre-zip-video-voc4-smooth-tracked-boxes-available video-name))
       (available-overgenerated-smooth
	(pre-zip-video-voc4-overgenerated-smooth-tracked-boxes-available video-name))
       (available-human-smooth
	(pre-zip-video-voc4-human-smooth-tracked-boxes-available video-name)))
  (define (upgrade available)
   (display available)(newline)
   (upgrade-one-per-video-to-zip video-name
				 (first available) (second available)
				 (third available) (fourth available)))
  (if (and if-needed?
	   (and (is-upgraded-zip? video-name available-detector)
		(is-upgraded-zip? video-name available-tracked)
		(is-upgraded-zip? video-name available-smooth)
		(is-upgraded-zip? video-name available-overgenerated)
		(is-upgraded-zip? video-name available-overgenerated-smooth)
		(is-upgraded-zip? video-name available-predicted)
		(is-upgraded-zip? video-name available-human)
		(is-upgraded-zip? video-name available-human-smooth)))
      (format #t "All boxes already upgraded~%")
      (begin
       (format #t "Processing the following detector boxes:~%")
       (pp available-detector)(newline)
       (format #t "Processing the following predicted boxes:~%")
       (pp available-predicted)(newline)
       (format #t "Processing the following tracked boxes:~%")
       (pp available-tracked)(newline)
       (format #t "Processing the following smooth tracked boxes:~%")
       (pp available-smooth)(newline)
       (format #t "Processing the following overgeneraed smooth tracked boxes:~%")
       (pp available-overgenerated)(newline)
       (format #t "Processing the following overgeneraed smooth tracked boxes:~%")
       (pp available-overgenerated-smooth)(newline)
       (format #t "Upgrading detectors~%")
       (for-each upgrade available-detector)
       (format #t "Upgrading tracked~%")
       (for-each upgrade available-tracked)
       (format #t "Upgrading smooth~%")
       (for-each upgrade available-smooth)
       (format #t "Upgrading overgenerated~%")
       (for-each upgrade available-overgenerated)
       (format #t "Upgrading overgenerated-smooth~%")
       (for-each upgrade available-overgenerated-smooth)
       (format #t "Upgrading human~%")
       (for-each upgrade available-human)
       (format #t "Upgrading human-smooth~%")
       (for-each upgrade available-human-smooth)
       (format #t "Upgrading predicted~%")
       (for-each upgrade available-predicted)
       (format #t "Done writing ~%")))
  (when delete? (fuck-up))))

(define (upgrade-klt video-name if-needed? delete?)
 (if (and if-needed? (file-exists? (klt-pathname video-name)))
     (format #t "KLT already upgrade~%")
     (begin
      (rm-if-necessary (klt-pathname video-name))
      (let ((klt-movie (pre-zip-read-klt-movie video-name)))
       (with-zip-file
	(lambda (zip2)
	 (for-each-indexed
	  (lambda (klt frame)
	   (zip:add-directory zip2 (number->padded-string-of-length
				    (+ (video-first-frame video-name) frame) 6))
	   (zip:add-file zip2
			 (per-frame-klt-in-zip-pathname
			  (+ (video-first-frame video-name) frame))
			 (map klt-pair->string klt)))
	  klt-movie))
	(klt-pathname video-name) *zip:mode-create-new*)))))

(define (upgrade-optical-flow video-name if-needed? delete?)
 (if (and if-needed? (file-exists? (optical-flow-pathname video-name)))
     (format #t "optical-flow already upgrade~%")
     (begin
      (rm-if-necessary (optical-flow-pathname video-name))
      (with-zip-file
       (lambda (zip2)
	(for-each-frame-but-last
	 (lambda (frame)
	  (let* ((buffer-size ;; TODO abstract this out
		  (+ (* *optical-flow-height* *optical-flow-width* 2 c-sizeof-float)
		     c-sizeof-float (* 2 c-sizeof-int)))
		 (buffer (malloc buffer-size))
		 (flow (pre-zip-read-optical-flow-in-c video-name frame))
		 (integral-flow
		  (integral-optical-flow-from-c
		   flow *optical-flow-width* *optical-flow-height*)))
	   (write-flo-to-buffer
	    (make-c-optical-flow
	     flow
	     *optical-flow-height*
	     *optical-flow-width*)
	    buffer buffer-size)
	   (free flow)
	   (free integral-flow)
	   (zip:add-file-from-buffer
	    zip2
	    (per-frame-optical-flow-in-zip-pathname frame)
	    buffer buffer-size #t)))
	 video-name))
       (optical-flow-pathname video-name) *zip:mode-create-new*))))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))
		    ("stand-alone" stand-alone? (path "path" string-argument ""))
		    ("demo" demo? (demo-name "name" string-argument "")))
       (exactly-one ("per-frame-to-per-video" per-frame-to-per-video?)
		    ("per-frame-to-zip" per-frame-to-zip?)
		    ("per-video-to-zip" per-video-to-zip?))
       (at-most-one ("video-datasets" video-datasets?
		     (video-datasets "pathname" string-argument "")))
       (at-most-one ("just-klt-and-flow" just-klt-and-flow?)
		    ("just-boxes" just-boxes?))
       (at-most-one ("delete" delete?))
       (at-most-one ("if-needed" if-needed?)))
 ;; Siddharth: TODO: fixme at some point.
 (panic "Cannot use because of addition of ringing to voc4-detection")
 (when video-datasets? (set! *video-pathname* video-datasets))
 (let* ((video (cond (standard?
		      (standard-corpus-video corpus sequence person location n))
		     (darpa? (string->darpa-video name))
		     (stand-alone? (make-stand-alone-video path))
		     (demo? (string->demo-video demo-name))
		     (else (fuck-up)))))
  (cond
   (per-frame-to-zip?
    (set! *frame-padding* 4)
    (unless just-klt-and-flow?
     (upgrade-per-frame-to-per-video video if-needed? delete?)
     (upgrade-all-per-video-to-zip video if-needed? delete?))
    (unless just-boxes?
     (upgrade-klt video #f #f)
     (upgrade-optical-flow video #f #f))
    (set! *frame-padding* 6))
   (per-video-to-zip?
    (set! *frame-padding* 4)
    (unless just-klt-and-flow?
     (upgrade-all-per-video-to-zip video if-needed? delete?))
    (unless just-boxes?
     (upgrade-klt video #f #f)
     (upgrade-optical-flow video #f #f))
    (set! *frame-padding* 6))
   (per-frame-to-per-video?
    (unless just-klt-and-flow?
     (upgrade-per-frame-to-per-video video if-needed? delete?)))
   (else (fuck-up)))))

;; (string->darpa-video "WALK9_A2_C1_Act6_URBAN1_ML_AFTN_DARK_b4852268-07b6-11e0-a3d4-e80688cb869a")
;; (upgrade-optical-flow (string->darpa-video "WALK9_A2_C1_Act6_URBAN1_ML_AFTN_DARK_b4852268-07b6-11e0-a3d4-e80688cb869a") #f #f)




(define (pre-zip-read-detector-boxes video-name type model)
 (pre-zip-fast-read-boxes (boxes-pathname video-name type model)))

(define (pre-zip-read-predicted-boxes video-name type model)
 (pre-zip-fast-read-boxes (predicted-boxes-pathname video-name type model)))

(define (pre-zip-read-smooth-tracked-boxes video-name type model number)
 (map first (pre-zip-fast-read-boxes (smooth-tracked-box-pathname video-name type model number))))

(define (pre-zip-read-tracked-boxes video-name type model number)
 (map first (pre-zip-fast-read-boxes (tracked-box-pathname video-name type model number))))

(define (pre-zip-read-voc4-detector-boxes video-name model)
 (read-detector-boxes video-name "voc4" model))

(define (pre-zip-read-voc4-predicted-boxes video-name model)
 (read-predicted-boxes video-name "voc4" model))

(define (pre-zip-read-voc4-smooth-tracked-boxes video-name model number)
 (read-smooth-tracked-boxes video-name "voc4" model number))

(define (pre-zip-read-voc4-tracked-boxes video-name model number)
 (read-tracked-boxes video-name "voc4" model number))

(define (pre-zip-read-voc4-overgenerated-detector-boxes video-name model)
 (read-detector-boxes video-name "voc4_overgenerated" model))

(define (pre-zip-read-voc4-overgenerated-predicted-boxes video-name model)
 (read-predicted-boxes video-name "voc4_overgenerated" model))

(define (pre-zip-read-voc4-overgenerated-smooth-tracked-boxes video-name model number)
 (read-smooth-tracked-boxes video-name "voc4_overgenerated" model number))

(define (pre-zip-read-voc4-overgenerated-tracked-boxes video-name model number)
 (read-tracked-boxes video-name "voc4_overgenerated" model number))

(define (pre-zip-read-voc4-event-detector-boxes video-name model)
 (read-detector-boxes video-name "voc4_event" model))

(define (pre-zip-read-voc4-event-predicted-boxes video-name model)
 (read-predicted-boxes video-name "voc4_event" model))

(define (pre-zip-read-voc4-event-smooth-tracked-boxes video-name model number)
 (read-smooth-tracked-boxes video-name "voc4_event" model number))

(define (pre-zip-read-voc4-event-tracked-boxes video-name model number)
 (read-tracked-boxes video-name "voc4_event" model number))

(define (pre-zip-read-single-boxes video-name filename)
 (map car (read-boxes video-name filename)))

(define (pre-zip-scheme-read-detector-boxes video-name model)
 ;; This function is around because the C code doesn't handle degraded
 ;; box files, that for example are missing a model name
 (read-boxes video-name (boxes-pathname video-name "voc4" model)))

(define (pre-zip-scheme-read-predicted-boxes video-name model)
 ;; This function is around because the C code doesn't handle degraded
 ;; box files, that for example are missing a model name
 (read-boxes video-name (predicted-boxes-pathname video-name "voc4" model)))

(define (pre-zip-scheme-read-smooth-tracked-boxes video-name model number)
 ;; This function is around because the C code doesn't handle degraded
 ;; box files, that for example are missing a model name
 (read-single-boxes
  video-name (smooth-tracked-box-pathname video-name "voc4" model number)))

(define (pre-zip-scheme-read-tracked-boxes video-name model number)
 ;; This function is around because the C code doesn't handle degraded
 ;; box files, that for example are missing a model name
 (read-single-boxes
  video-name (tracked-box-pathname video-name "voc4" model number)))

(define (boxes-pathname video-name type model)
 (generic-root-pathname video-name
			(string-append type "-" model ".boxes")))

(define (predicted-boxes-pathname video-name type model)
 (generic-root-pathname video-name
			(string-append type "-" model ".predicted-boxes")))

(define (smooth-tracked-box-pathname video-name type model number)
 (generic-root-pathname video-name
			(string*-append type "-" model "-" number ".smooth-tracked-box")))

(define (tracked-box-pathname video-name type model number)
 (generic-root-pathname video-name
			(string*-append type "-" model "-" number ".tracked-box")))

(define (pre-zip-boxes-movie->strings boxes-movie)
 (join
  (map
   (lambda (boxes)
    (let ((boxes (remove-if-not nondropped-box? boxes)))
     (cons (number->string (length boxes)) (map voc4-box->string boxes))))
   boxes-movie)))

(define (pre-zip-box-movie->strings box-movie)
 (join (map (lambda (box) (list "1" (voc4-box->string box))) box-movie)))

(define (pre-zip-write-box-movie box-movie pathname)
 (write-file (pre-zip-box-movie->strings box-movie) pathname))

(define (pre-zip-write-boxes-movie boxes-movie pathname)
 (write-file (pre-zip-boxes-movie->strings boxes-movie) pathname))

(define (pre-zip-write-voc4-boxes-movie boxes-movie video-name model)
 (pre-zip-write-boxes-movie boxes-movie (boxes-pathname video-name "voc4" model)))

(define (pre-zip-write-voc4-predicted-boxes-movie boxes-movie video-name model)
 (pre-zip-write-boxes-movie boxes-movie (predicted-boxes-pathname video-name "voc4" model)))

(define (pre-zip-write-voc4-tracked-box-movie box-movie video-name model number)
 (pre-zip-write-box-movie box-movie (tracked-box-pathname video-name "voc4" model number)))

(define (pre-zip-write-voc4-smooth-tracked-box-movie box-movie video-name model number)
 (pre-zip-write-box-movie box-movie (smooth-tracked-box-pathname video-name "voc4" model number)))

(define (pre-zip-write-voc4-overgenerated-boxes-movie boxes-movie video-name model)
 (pre-zip-write-boxes-movie boxes-movie (boxes-pathname video-name "voc4_overgenerated" model)))

(define (pre-zip-write-voc4-overgenerated-predicted-boxes-movie boxes-movie video-name model)
 (pre-zip-write-boxes-movie boxes-movie (predicted-boxes-pathname video-name "voc4_overgenerated" model)))

(define (pre-zip-write-voc4-overgenerated-tracked-box-movie box-movie video-name model number)
 (pre-zip-write-box-movie box-movie (tracked-box-pathname video-name "voc4_overgenerated" model number)))

(define (pre-zip-write-voc4-overgenerated-smooth-tracked-box-movie box-movie video-name model number)
 (pre-zip-write-box-movie box-movie (smooth-tracked-box-pathname video-name "voc4_overgenerated" model number)))

(define (pre-zip-write-voc4-event-boxes-movie boxes-movie video-name model)
 (pre-zip-write-boxes-movie boxes-movie (boxes-pathname video-name "voc4_event" model)))

(define (pre-zip-write-voc4-event-predicted-boxes-movie boxes-movie video-name model)
 (pre-zip-write-boxes-movie boxes-movie (predicted-boxes-pathname video-name "voc4_event" model)))

(define (pre-zip-write-voc4-event-tracked-box-movie box-movie video-name model number)
 (pre-zip-write-box-movie box-movie (tracked-box-pathname video-name "voc4_event" model number)))

(define (pre-zip-write-voc4-event-smooth-tracked-box-movie box-movie video-name model number)
 (pre-zip-write-box-movie box-movie (smooth-tracked-box-pathname video-name "voc4_event" model number)))

(define (pre-zip-video-boxes-available video type kind)
 (map
  (lambda (pathname)
   (let* ((parts (pregexp-split "-" (strip-extension (strip-directory pathname))))
	  (reverse-parts (reverse parts)))
    (cond ((string->number (first  reverse-parts))
	   (list (first parts)
		 (string-join "-" (cdr (reverse (cdr reverse-parts))))
		 (last parts)
		 (extension pathname)))
	  (else (list (first parts)
		      (string-join "-" (cdr parts))
		      #f
		      (extension pathname))))))
  (directory-list (generic-root-pathname video (string-append "/" type "-*." kind)))))

(define (pre-zip-video-detector-boxes-available video type)
 (pre-zip-video-boxes-available video type "boxes"))

(define (pre-zip-video-predicted-boxes-available video type)
 (pre-zip-video-boxes-available video type "predicted-boxes"))

(define (pre-zip-video-tracked-boxes-available video type)
 (pre-zip-video-boxes-available video type "tracked-box"))

(define (pre-zip-video-smooth-tracked-boxes-available video type)
 (pre-zip-video-boxes-available video type "smooth-tracked-box"))

(define (pre-zip-video-voc4-detector-boxes-available video)
 (pre-zip-video-boxes-available video "voc4" "boxes"))

(define (pre-zip-video-voc4-predicted-boxes-available video)
 (pre-zip-video-boxes-available video "voc4" "predicted-boxes"))

(define (pre-zip-video-voc4-tracked-boxes-available video)
 (pre-zip-video-boxes-available video "voc4" "tracked-box"))

(define (pre-zip-video-voc4-smooth-tracked-boxes-available video)
 (pre-zip-video-boxes-available video "voc4" "smooth-tracked-box"))

(define (pre-zip-video-voc4-overgenerated-tracked-boxes-available video)
 (pre-zip-video-boxes-available video "voc4_overgenerated" "tracked-box"))

(define (pre-zip-video-voc4-overgenerated-smooth-tracked-boxes-available video)
 (pre-zip-video-boxes-available video "voc4_overgenerated" "smooth-tracked-box"))

(define (pre-zip-video-voc4-human-tracked-boxes-available video)
 (pre-zip-video-boxes-available video "voc4_human" "tracked-box"))

(define (pre-zip-video-voc4-human-smooth-tracked-boxes-available video)
 (pre-zip-video-boxes-available video "voc4_human" "smooth-tracked-box"))
