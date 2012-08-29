(MODULE
  LIMBS-VITERBI
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
(include "limbs-viterbi.sch")

(set! *program* "limbs-viterbi")
(set! *panic?* #f)

;; darpa-wrap ./limbs-viterbi -darpa Approach2_A1_C1_Act1_2_Park3_MC_AFTN_47ff4ac2-c5af-11df-a99b-e80688cb869a -start-frame 60 -end-frame 90 -training-frames /tmp/b

(define (render-points-ppm ppm video-target frame-target video frame . rgb)
 (let ((voc4 (voc4-bloat
	      (first (read-voc4-boxes (smooth-tracked-box-pathname video-target frame-target "person" "1")))
	      0))
       (rgb (if (null? rgb) '#(0 255 0) (first rgb))))
  (for-each
   (lambda (p) (set-ppm-pixel! ppm (x p) (y p) rgb))
   (map quantize-point
	(map (lambda (p) (v+ (vector (voc4-detection-x1 voc4) (voc4-detection-y1 voc4)) p))
	     (points->target-bb
	      (join (read-object-from-file (cropped-lines-pathname video frame)))
	      (points-bounding-box
	       (join (read-object-from-file (cropped-lines-pathname video-target frame-target))))))))
  ppm))

(define (render-points video-target frame-target video frame)
 (render-points-ppm (read-pnm (ppm-pathname video frame-target))
		    video-target frame-target video frame))


(define (render-limbs video-target frame-target video frame)
 (render-limbs-ppm (read-pnm (ppm-pathname video-target frame-target))
		   video-target frame-target video frame))

(define (render-viterbi-results alpha video start-frame end-frame distance-limbs output)
 (write-object-to-file
  distance-limbs
  (string*-append "/tmp/limbs-" output "-" alpha "-" (darpa-video->string video) ".sc"))
 (let ((result (viterbi
		(lambda (old weight transition) (+ old weight (* alpha transition)))
		(lambda (a) (third a))
		(lambda (a b) (limbs-distance (fourth a) (fourth b)))
		distance-limbs)))
  (write-object-to-file
   result
   (string*-append "/tmp/viterbi-" output "-" alpha "-" (darpa-video->string video) ".sc"))
  (for-each-indexed
   (lambda (r i)
    (let ((ppm (read-pnm (ppm-pathname video (+ start-frame i)))))
     (render-limbs-ppm
      ppm video (+ start-frame i) (first r) (second r) '#(0 255 0))
     (write-pnm
      ppm
      (string*-append
       "/tmp/limbs-" output "-" alpha "-"
       (darpa-video->string video) "-"
       (number->padded-string-of-length (+ start-frame i) 10)))))
   (second result))))

(define (render-limbs-ppm ppm video-target frame-target video frame . rgb)
 (let ((voc4-target
	(voc4-bloat
	 (first (read-voc4-boxes
		 (smooth-tracked-box-pathname video-target frame-target "person" "1")))
	 0))
       (voc4
	(voc4-bloat
	 (first (read-voc4-boxes
		 (smooth-tracked-box-pathname video frame "person" "1")))
	 0))
       (rgb (if (null? rgb) '#(255 0 0) (first rgb))))
  (for-each
   (lambda (p)
    (set-ppm-pixel! ppm (x p) (y p) rgb)
    (set-ppm-pixel! ppm (+ (x p) 1) (y p) rgb)
    (set-ppm-pixel! ppm (x p) (+ (y p) 1) rgb))
   (map quantize-point
	(map (lambda (p) (v+ (vector (voc4-detection-x1 voc4-target)
				(voc4-detection-y1 voc4-target))
			p))
	     (points->other-target-bb
	      (join (map line-segment->points
			 (limbs->lines
			  (limbs->limbs-bb
			   (read-object-from-file
			    (human-limb-annotation-pathname video frame))
			   (voc4->bb voc4)))))
	      (join (read-object-from-file (cropped-lines-pathname video frame)))
	      (points-bounding-box
	       (join (read-object-from-file
		      (cropped-lines-pathname video-target frame-target))))))))
  ppm))

(define (render-test video-target frame-target video frame)
 (let ((ppm (read-pnm (ppm-pathname video-target frame-target))))
  (render-points-ppm ppm video-target frame-target video-target frame-target '#(255 0 0))
  (render-points-ppm ppm video-target frame-target video frame '#(0 255 0))
  (render-limbs-ppm ppm video-target frame-target video frame '#(0 0 255))
  ppm))

(define (read-distances2 video-name frame videos-frames)
 (removeq
  #f
  (map
   (lambda (v-f)
    (let ((filename
	   (frame-distance-pathname
	    video-name frame
	    (string->darpa-video (first v-f)) (second v-f))))
     (when (file-exists? filename)
      (list (string->darpa-video (first v-f))
	    (second v-f)
	    (read-object-from-file filename)))))
   videos-frames)))

(define (read-distances-and-limbs2 video start-frame end-frame videos-frames)
 (map-m-n
  (lambda (frame)
   (map
    (lambda (d)
     (append d (list (read-object-from-file
		      (human-limb-annotation-pathname (first d) (second d))))))
    (read-distances2 video frame videos-frames)))
  start-frame
  end-frame))

(define-command
 (main
  (exactly-one ("standard" standard?
		(corpus "corpus" string-argument "")
		(sequence "sequence" string-argument "")
		(person "person" string-argument "")
		(location "location" string-argument "")
		(n "n" integer-argument 0))
	       ("darpa" darpa? (name "name" string-argument "")))
  (at-most-one ("start-frame" start-frame? (start-frame "#" integer-argument 0)))
  (at-most-one ("end-frame" end-frame? (end-frame "#" integer-argument 0)))
  (exactly-one ("training-frames" training-frames?
		(training-frames "training-frames" string-argument "")))
  (at-most-one ("alpha" alpha? (alpha "#" real-argument 1.0)))
  (at-most-one ("output" output? (output "name" string-argument ""))))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up))))
	(training-frames (map (lambda (vf) (list (field-ref vf 0)
					    (string->number (field-ref vf 1))))
			      (read-file training-frames)))
	(start-frame (if start-frame? start-frame (video-first-frame video-name)))
	(end-frame (if end-frame? end-frame (video-last-frame video-name)))
	(distances (read-distances-and-limbs
		    video-name start-frame end-frame training-frames)))
  (pp training-frames)(newline)
  (render-viterbi-results alpha video-name start-frame end-frame distances output)))
