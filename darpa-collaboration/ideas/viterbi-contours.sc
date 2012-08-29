(MODULE
  VITERBI-CONTOURS
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
(include "viterbi-contours.sch")

(set! *program* "viterbi-contours")
(set! *panic?* #f)


(define (generate-pathname prefix name type . args)
 (let ((extra-args (map-reduce
		    string-append
		    ""
		    (lambda (arg) (string-append "-" arg))
		    (remove-if null? args))))
  (string-append
   prefix
   name 
   (case type
    ('base extra-args)
    ('image (string-append extra-args ".ppm"))
    ('pb (string-append "-berkeley" extra-args ".pgm"))
    ('superpixels (string-append "-slic" extra-args ".dat"))
    ('graph (string-append extra-args ".graph"))
    ('lines (string-append extra-args ".lines"))
    ('weights (string-append extra-args ".w"))
    ('cycle (string-append extra-args ".cycle"))
    ('output-image (string-append extra-args "-superpixels.ppm"))
    ('output-contour (string-append extra-args "-contour.pgm"))
    ('output-graph (string-append extra-args "-solid.pgm"))
    ('numbered-graph (string-append extra-args ".ps"))
    ('chains (string-append extra-args ".chains"))
    ('cycle-sc (string-append extra-args "-cycle.sc"))
    ('contour (string-append extra-args "-contour.sc"))
    (else "BAD")))))
 
(define (cropped-pathname video-name frame model model-number)
 (generic-pathname video-name frame (string-append "frame-cropped-" model "-" (number->string model-number) ".ppm")))


(define *experiment-videos*
 '("Attach2_A1_C1_Act1_PARK2_MR_MIDD_4426cf73-c5af-11df-87f0-e80688cb869a"
   "Catch8_A1_C1_Act1_PARK3_BL_AFTN_48033e0c-c5af-11df-ab27-e80688cb869a"
   "Give1_A1_C1_Act1_3_DOWNTOWN1A3_MC_MIDD_47a33c3a-c5af-11df-ae99-e80688cb869a"
   "Give2_A2_C1_Act1_2_DOWNTOWN1A3_MC_MIDD_47aa6191-c5af-11df-bdb3-e80688cb869a"
   "Jump7_A1_C1_Act1_PARK1_ML_MIDD_446e7a40-c5af-11df-9d6f-e80688cb869a"
   "Jump9_A1_C1_Act1_DOWNTOWN1A3_FR_MIDD_47b3ba4c-c5af-11df-ade7-e80688cb869a"))

(define (run-all-viterbi)
 (for-each
  (lambda (video)
   (compute-viterbi-contours (string->darpa-video video)))
  *experiment-videos*))

;;; Viterbi

(define-structure viterbi-frame contours optical-flow)

(define-structure viterbi-contour filename prev-contour distance confidence)

;; (compute-viterbi-contours "../qobi/purdue36-9/viterbi/" 3)
;; (define (compute-viterbi-contours path number)
;;  (let ((frames (read-viterbi-frames path number)))
;;   (compute-viterbi-weights! frames)
;;   (extract-viterbi-path (last frames))))

;; (compute-viterbi-contours (string->darpa-video "Attach2_A1_C1_Act1_PARK2_MR_MIDD_4426cf73-c5af-11df-87f0-e80688cb869a"))
(define (compute-viterbi-contours video-name)
 (let ((frames (read-viterbi-video video-name)))
  ;; (compute-viterbi-weights! frames)
  ;; (write-object-to-file
  ;;  (extract-viterbi-path (last frames))
  ;;  (generic-full-pathname *video-pathname* video-name "/viterbi-contour.sc"))
  (make-viterbi-video-frames video-name)
  (pgm-files->avi-file
   (generic-full-pathname *video-pathname* video-name "")
   "viterbi-contour-full" 10
   (generic-full-pathname *video-pathname* video-name "/viterbi-contour"))))



(define (compute-viterbi-weights! frames . weight)
 (let ((confidence-weight (if (null? weight)
			      -10
			      (first weight))))
  (for-each-pair
   (lambda (frame1 frame2)
    (let* ((optical-flow (optical-flow-average
			  (read-optical-flow
			   (viterbi-frame-optical-flow frame1)))))
     (display "frame pair done")(newline)
     (for-each
      (lambda (contour)
       (let* ((compared (min-contour contour
				     (viterbi-frame-contours frame1)
				     '()
				     infinity
				     optical-flow))
	      (distance (second compared))
	      (prev (first compared)))
	(set-viterbi-contour-prev-contour! contour prev)
	(set-viterbi-contour-confidence!
	 contour
	 (viterbi-confidence (viterbi-contour-filename contour)))
	(set-viterbi-contour-distance!
	 contour
	 (+
	  (+ distance (viterbi-contour-distance prev))
	  (* confidence-weight (viterbi-contour-confidence contour))))))
      (viterbi-frame-contours frame2))
     frames))
   frames)))

(define (for-each-pair pair-f l)
 (let loop ((n 1))
  (unless (>= n (length l))
   (pair-f
    (list-ref l (- n 1))
    (list-ref l n))
   (loop (+ n 1)))))

(define (min-contour contour prev-frame-contours minimum min-distance optical-flow)
 ;; Link to contour from previous frame
 (cond ((null? prev-frame-contours) (list minimum min-distance))
       (else
	(let* ((original-contour (pgm->pbm
				  (read-pnm
				   (viterbi-contour-filename contour)) 128))
	       (compared-contour (pgm->pbm
				  (read-pnm
				   (viterbi-contour-filename
				    (car prev-frame-contours))) 128) )
	       ;; (avg-flow
	       ;; 	(optical-flow-average-in-contour
	       ;; 	 optical-flow
	       ;; 	 compared-contour))
	       (distance
		(viterbi-distance
		 original-contour
		 (pbm-shift 
		  compared-contour
		  (x optical-flow)
		  (y optical-flow)))))
	 (min-contour
	  contour
	  (cdr prev-frame-contours)
	  (if (< distance min-distance)
	      (car prev-frame-contours)
	      minimum)
	  (min distance min-distance)
	  optical-flow)))))

(define (min-contour-distance frame-contours minimum min-distance)
 ;; Find contour with lowest distance
 (cond ((null? frame-contours) minimum)
       (else
	(min-contour-distance
	 (cdr frame-contours)
	 (if (< (viterbi-contour-distance (car frame-contours))
		min-distance)
	     (car frame-contours)
	     minimum)
	 (min
	  (viterbi-contour-distance (car frame-contours))
	  min-distance)))))

(define (extract-viterbi-path frame)
 ;; Does nothing more than dump out the filenames, currently
 (define (recurse-path contour frame-list)
  ;; (display (viterbi-contour-filename contour))
  ;; (newline)
  (write-pnm
   (read-pnm (viterbi-contour-filename contour))
   (string-append
    (directory (viterbi-contour-filename contour))
    "/viterbi-contour.pgm"))
  (cond ((eq? (viterbi-contour-prev-contour contour) '()) frame-list)
	(else (recurse-path
	       (viterbi-contour-prev-contour contour)
	       (append frame-list (list (viterbi-contour-filename contour)))))))
 (let ((best-contour
	(min-contour-distance
	 (viterbi-frame-contours frame)
	 '()
	 infinity)))
  (recurse-path best-contour '())))

(define (viterbi-confidence pbm-filename)
 ;; really bad: no single-frame confidence score yet
 0)

(define (viterbi-distance pbm1 pbm2)
 ;; bad: does not take motion into account
 ;; (let ((pbm1 (pgm->pbm (read-pnm pbm1-filename) 128))
 ;;       (pbm2 (pgm->pbm (read-pnm pbm2-filename) 128)))
 ;;  (sum-pbm (pbm-xor pbm1 pbm2)))
 (let* ((min-width (min (pnm-width pbm1) (pnm-width pbm2)))
	 (min-height (min (pnm-height pbm1) (pnm-height pbm2))))
  (sum-pbm (pbm-xor
	    (crop-image pbm1 0 0 min-width min-height)
	    (crop-image pbm2 0 0 min-width min-height)))))

;; (read-viterbi-frames "../qobi/purdue36-9/viterbi/" 3)
(define (read-viterbi-frames frame-path number-of-frames)
 ;; todo: run on darpa videos instead of arbitrary paths
 (map-n
  (lambda (frame)
   (make-viterbi-frame
    (map-n
     (lambda (contour)
      (make-viterbi-contour
       (string-append frame-path
		      (number->padded-string-of-length (+ frame 1) 4)
		      "/frame-" (number->string contour) "-contour.pgm")
       '() 0 0))
     (count-number-of-contours
      (string-append frame-path
		     (number->padded-string-of-length (+ frame 1) 4)
		     "/frame")
      0))
    ""))
  number-of-frames))

;; (read-viterbi-video (string->darpa-video "Approach7_A2_C1_Act3_URBAN6_MC_AFTN_4878ba73-c5af-11df-a416-e80688cb869a" ))
(define (read-viterbi-video video-name)
 (removeq
  #f
  (map-frame
   (lambda (frame)
    (display (string-append "Frame" (number->string frame))) (newline)
    (let* ((model "person")
	   (model-number 1)
	   (path (lambda (type . args)
		  (apply
		   generate-pathname
		   (append
		    `(,(generic-pathname video-name frame "") "frame-cropped" ,type ,model ,(number->string model-number))
		    args))))
	   (number-of-contours (find-contours path 0)))
     (if (file-exists?
	  (string-append (strip-extension
			  (scheme-optical-flow-pathname video-name frame))
			 ".ssv.sc"))
	 (make-viterbi-frame
	  (map-n
	   (lambda (n)
	    (make-viterbi-contour
	     (path 'output-contour (number->string n)) 
	     '() 0 0))
	   number-of-contours)
	  (string-append (strip-extension
			  (scheme-optical-flow-pathname video-name frame))
			 ".ssv.sc"))
	 #f)))
   video-name)))

(define (make-viterbi-video-frames video-name)
 (for-each-frame
  (lambda (frame)
   (display (string-append "Frame" (number->string frame))) (newline)
   (let* ((model "person")
	  (model-number 1)
	  (full-image (read-pnm (ppm-full-pathname video-name frame)))
	  (voc4 (fix-voc4-box
		 full-image
		 (voc4-bloat
		  (first (read-voc4-boxes
			  (smooth-tracked-box-pathname video-name frame model (number->string model-number))))
		  0))))
    (when (file-exists?
	   (generic-pathname video-name frame "viterbi-contour.pgm"))
     (write-pnm
      (pbm->pgm
       (cropped-pbm->full-pbm
	(pgm->pbm
	 (read-pnm (generic-pathname video-name frame "viterbi-contour.pgm")) 128)
	(voc4-detection-x1 voc4)
	(voc4-detection-y1 voc4)
	(pnm-width full-image)
	(pnm-height full-image)))
      (generic-pathname video-name frame "viterbi-contour-full.pgm"))
     (write-pnm
      (pbm-overlay-ppm 
       (cropped-pbm->full-pbm
	(pgm->pbm
	  (read-pnm
	   (generic-pathname video-name frame "viterbi-contour.pgm")) 128)
      	(voc4-detection-x1 voc4)
      	(voc4-detection-y1 voc4)
      	(pnm-width full-image)
      	(pnm-height full-image))
       full-image)
      (generic-pathname video-name frame "viterbi-contour-full.ppm")))))
  video-name))

(define (pbm-overlay-ppm pbm ppm)
 (let ((outline (pbm-dialate 1
			     (points->pbm
			      (region->boundary (pbm->points pbm))
			      (pnm-height ppm)
			      (pnm-width ppm)))))
  ;; (map-indexed-matrix
  ;;  (lambda (v i j)
  ;;   (when (safe-matrix-ref (pbm-bitmap outline) i j #f)
  ;;    (set-ppm-pixel! ppm j i '#(255 0 0))))
  ;;  (pbm-bitmap outline))
  ;; ppm
  (make-ppm
   (ppm-raw? ppm)
   (ppm-maxval ppm)
   (map-indexed-matrix
    (lambda (v i j)
     (if (matrix-ref (pbm-bitmap outline) i j)
  	 255
  	 v))
    (ppm-red ppm))
   (map-indexed-matrix
    (lambda (v i j)
     (if (matrix-ref (pbm-bitmap outline) i j)
  	 0
  	 v))
    (ppm-green ppm))
   (map-indexed-matrix
    (lambda (v i j)
     (if (matrix-ref (pbm-bitmap outline) i j)
  	 0
  	 v))
    (ppm-blue ppm)))))

(define (cropped-pbm->full-pbm pbm x-offset y-offset w h)
 (let ((full-pbm (pbm-constant w h #f)))
  (map-indexed-matrix
   (lambda (v i j)
    (matrix-set! (pbm-bitmap full-pbm)
		 (+ i y-offset)
		 (+ j x-offset)
		 v))
   (pbm-bitmap pbm))
  full-pbm))

(define (neighboring-pairs l)
 ;; Decided to go with for-each-pair, with side effects, instead
 (if (null? (cdr l)) '()
     (append
      (list (list (car l) (car (cdr l))))
      (neighboring-pairs (cdr l)))))

;;; Contour Combining

;; (combine-contours-video (string->darpa-video "Approach7_A2_C1_Act3_URBAN6_MC_AFTN_4878ba73-c5af-11df-a416-e80688cb869a" ) 5)
(define (combine-contours-video video-name number-to-process)
 (define (power-set-only-tuples set)
  (remove-if
   (lambda (set) (not (= (length set) 2)))
   (power-set set)))
 (for-each-frame
  (lambda (frame)
   (let* ((model "person")
	  (model-number 1)
	  (path (lambda (type . args)
		 (apply
		  generate-pathname
		  (append
		   `(,(generic-pathname video-name frame "") "frame-cropped" ,type ,model ,(number->string model-number))
		   args))))
	  (number-of-contours (find-contours path 0)))
    (display (string-append "Frame " (number->string frame))) (newline)
    (for-each-indexed
     (lambda (image-pair i)
      (write-pnm
       (pbm->pgm
	(pbm-or
	 (pgm->pbm (read-pnm (first image-pair)) 128)
	 (pgm->pbm (read-pnm (second image-pair)) 128)))
       (path 'output-contour
     	     (number->string (+ i number-of-contours)))))
     (power-set-only-tuples
      (map-n
       (lambda (n)
	(path 'output-contour (number->string n)))
       number-to-process)))
    ;; (for-each-indexed
    ;;  (lambda (image i)
    ;;   (write-pnm
    ;;    (pbm->pgm image)
    ;;    (path 'output-contour
    ;;  	     (number->string (+ i number-of-contours)))))
    ;;  (generate-contour-combinations
    ;;   (map-n
    ;;    (lambda (n)
    ;; 	(pgm->pbm
    ;; 	 (read-pnm
    ;; 	  (path 'output-contour (number->string n)))
    ;; 	 128))
    ;;    number-to-process)))
    ))
  video-name))



;; (generate-contour-combinations "../qobi/purdue36-9/or-test/fall3-0106")
(define (generate-contour-combinations input-images)
 (define (power-set-without-singletons set)
  (remove-if
   (lambda (set) (< (length set) 2))
   (power-set set)))
 (define (power-set-only-tuples set)
  (remove-if
   (lambda (set) (not (= (length set) 2)))
   (power-set set)))
 (let* (;; (input-images (load-contours path 1))
	(ored-images (map pbm-or-list
			  (power-set-without-singletons input-images))))
  (remove-duplicatesp ; Remove duplicate frames
    (lambda (img1 img2)
     (empty-pnm? (pbm-xor img1 img2)))
    (remove-if ; Remove disjoint contours
     (lambda (pnm)
      (> (length (connected-components (pbm->graph pnm 2))) 1))
     (remove-if ; Remove small contours
      (lambda (pnm)
       (< (sum-pbm pnm) 10))
     (append
      ored-images
      (map pbm-subtract-list
     	   (map-reduce append '() permutations
		       (power-set-only-tuples
     		       (append ; Allow subtraction between original
			       ; and unioned images
			input-images ored-images))))))))))

(define (count-number-of-contours path counter)
 (let ((file-name (string-append path "-" (number->string counter) "-contour.pgm")))
  (cond ((file-exists? file-name)
	 (+ 1 (count-number-of-contours path (+ counter 1))))
	(else 0))))

(define (load-contour-paths path counter)
 (let ((file-name (string-append path "-" (number->string counter) "-contour.pgm")))
  (cond ((file-exists? file-name)
	 (cons file-name
	       (load-contour-paths path (+ counter 1))))
	(else `()))))

(define (load-contours path counter)
 (let ((file-name (string-append path "-" (number->string counter) "-contour.pgm")))
  (cond ((file-exists? file-name)
	 (cons (pgm->pbm (read-pnm file-name) 128)
	       (load-contours path (+ counter 1))))
	(else `()))))

(define (sum-pbm pnm)
 (cond
  ((pbm? pnm)
   (reduce-vector
    +
    (map-vector
     (lambda (row)
      (count-if (lambda (value) (eq? value #t)) (vector->list row)))
     (pbm-bitmap pnm))
    0))
  (else (panic "Argument to THRESHOLD-PBM? is not a PBM"))))

(define (pbm-subtract pbm1 pbm2)
 (pbm-and
  pbm1
  (pbm-not(pbm-and
	   pbm1
	   pbm2))))

(define (pbm-or-list set)
 (cond ( (< (length set) 2) (first set))
       (else (pbm-or (car set) (pbm-or-list (cdr set))))))

(define (pbm-subtract-list set)
 (reduce pbm-subtract set '()))

(define (power-set set)
 (if (null? set) '(())
     (let ((remainder-set (power-set (cdr set))))
      (append remainder-set
	      (map (lambda (subset) (cons (car set) subset))
		   remainder-set)))))

(define (permutations items)
 (if (null? items) '(())
     (apply append
	    (map (lambda (element)
		  (map (lambda (permutation)
			(cons element permutation))
		       (permutations (remove element items))))
		 items))))

(define (find-contours path n)
 (if (file-exists? (path 'output-contour (number->string n)))
     (+ 1 (find-contours path (+ n 1)))
     0))

;;; Forward-project contours

;; (forward-project (string->darpa-video "Attach2_A1_C1_Act1_PARK2_MR_MIDD_4426cf73-c5af-11df-87f0-e80688cb869a" ) 5 2)
(define (forward-project video-name number-of-contours distance-to-project)
 (for-each-frame
  (lambda (frame)
   (let* ((model "person")
	  (model-number 1)
	  (path (lambda (type . args)
		 (apply
		  generate-pathname
		  (append
		   `(,(generic-pathname video-name frame "") "frame-cropped" ,type ,model ,(number->string model-number))
		   args))))
	  (full-image (read-pnm (ppm-full-pathname video-name frame)))
	  (voc4 (fix-voc4-box
		 full-image
		 (voc4-bloat
		  (first (read-voc4-boxes
			  (smooth-tracked-box-pathname video-name frame model (number->string model-number))))
		  0.2)))
	  ;; (image
	  ;;  (if (file-exists? (path 'image))
	  ;;      (read-pnm (path 'image))
	  ;;      (crop-voc4
	  ;; 	full-image
	  ;; 	voc4)))
	  (optical-flows
	   (map-n
	    (lambda (n)
	     (crop-optical-flow (read-optical-flow
				 (string-append
				  (strip-extension
				   (scheme-optical-flow-pathname video-name (+ frame n)))
				  ".ssv.sc"))
				voc4))
	    distance-to-project)))
    (display (string-append "Frame " (number->string frame))) (newline)
    (for-each-n
     (lambda (n)
      (display (string-append "--Contour " (number->string n))) (newline)
      (let ((contour
	     (pgm->pbm
	      (read-pnm
	       (path 'output-contour (number->string n)))
	      128)))
       (for-each-n
	(lambda (next-frame)
	 (let* ((path-next (lambda (type . args)
			   (apply
			    generate-pathname
			    (append
			     `(,(generic-pathname video-name (+ frame (+ next-frame 1)) "") "frame-cropped" ,type ,model ,(number->string model-number))
			     args))))
	       (number-of-contours-next (find-contours path-next 0)))
	  (write-pnm
	   (pbm->pgm
	    (forward-project-contour-recursive contour optical-flows 0 next-frame))
	   (path-next 'output-contour
		      (number->string ;; (+ n number-of-contours-next)
				      number-of-contours-next)))))
	distance-to-project)
       ))
     number-of-contours)))
  video-name))

;; (read-optical-flow "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/Approach7_A2_C1_Act3_URBAN6_MC_AFTN_4878ba73-c5af-11df-a416-e80688cb869a/0001/optical-flow.ssv" 720)

;; Requires the .ssv file be converted to .sc using
;; flatfile2sc in ~/darpa-collaboration/bin/
(define (read-optical-flow filename)
 (call-with-input-file filename
  (lambda (port)
   (let ((optical-flow-full (read-object-from-file filename)))
    (list
     (subvector optical-flow-full
		0
		(/ (vector-length optical-flow-full) 2))
     (subvector optical-flow-full
		(/ (vector-length optical-flow-full) 2)
		(vector-length optical-flow-full)))))))

(define (read-optical-flow-still-slow filename)
 (call-with-input-file filename
  (lambda (port)
   (let ((lines (list->vector (read-file filename))))
    (map-vector
     (lambda (line)
      (map
       string->number
       (pregexp-split " " line)))
     lines)))))

(define (read-optical-flow-slow filename h)
 (call-with-input-file filename
  (lambda (port)
   (list
    (list->vector
     (map-n
      (lambda (line-num)
       (list->vector
	(map
	 (lambda (element)
	  (string->number element))
	 (pregexp-split " " (read-line port)))))
      (/ h 2)))
    (list->vector
     (map-n
      (lambda (line-num)
       (list->vector
	(map
	 (lambda (element)
	  (string->number element))
	 (pregexp-split " " (read-line port)))))
      (/ h 2)))))))

;; (crop-optical-flow (read-optical-flow "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/Approach7_A2_C1_Act3_URBAN6_MC_AFTN_4878ba73-c5af-11df-a416-e80688cb869a/0001/optical-flow.ssv" 360) (voc4-bloat (first (read-voc4-boxes (smooth-tracked-box-pathname (string->darpa-video "Approach7_A2_C1_Act3_URBAN6_MC_AFTN_4878ba73-c5af-11df-a416-e80688cb869a") 1 "person" "1"))) 0.2))

(define (crop-abs m x1 y1 x2 y2)
 (map-vector
  (lambda (row)
   (subvector row x1 x2))
  (subvector m y1 y2)))

(define (crop-optical-flow optical-flow voc4)
 (let ((h (vector-length (first optical-flow)))
       (w (vector-length (vector-ref (first optical-flow) 0))))
 (define (condition-coordinate-x x-coordinate)
  (max 0
       (min (- w 1)
	    (inexact->exact (floor (/ x-coordinate  2))))))
 (define (condition-coordinate-y y-coordinate)
  (max 0
       (min (- h 1)
	    (inexact->exact (floor (/ y-coordinate  2))))))
 (list
  (crop-abs (first optical-flow)
	    (condition-coordinate-x (voc4-detection-x1 voc4))
	    (condition-coordinate-y (voc4-detection-y1 voc4))
	    (condition-coordinate-x (voc4-detection-x2 voc4))
	    (condition-coordinate-y (voc4-detection-y2 voc4)))
  (crop-abs (second optical-flow)
	    (condition-coordinate-x (voc4-detection-x1 voc4))
	    (condition-coordinate-y (voc4-detection-y1 voc4))
	    (condition-coordinate-x (voc4-detection-x2 voc4))
	    (condition-coordinate-y (voc4-detection-y2 voc4))))))

(define (fix-voc4-box image voc4)
  (define (bounds-check-x image val)
  (max 0 (min (pnm-width image) val)))
 (define (bounds-check-y image val)
  (max 0 (min (pnm-height image) val)))
 (make-voc4-detection
  (bounds-check-x image (voc4-detection-x1 voc4))
  (bounds-check-y image (voc4-detection-y1 voc4))
  (bounds-check-x image (voc4-detection-x2 voc4))
  (bounds-check-y image (voc4-detection-y2 voc4))
  (voc4-detection-filter voc4)
  (voc4-detection-strength voc4)))

(define (contour->outline pbm)
 (pbm-dialate
  1
  (points->pbm
   (region->boundary (pbm->points pbm))
   (pnm-height pbm)
   (pnm-width pbm))))

;; (write-pnm (pbm->pgm (forward-project-contour (pgm->pbm (read-pnm "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a/0019/frame-cropped-person-1-0-contour.pgm") 128) (read-optical-flow "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a/0019/optical-flow.ssv.sc"))) "test5.pgm")
(define (forward-project-contour contour optical-flow)
 ;; Real optical flow projection, deforming the contour
 (let* ((optical-flow-horizontal (first optical-flow))
	(optical-flow-vertical (second optical-flow))
	(projected (map
		    (lambda (point)
		     (vector
		      (+ (x point)
			 (inexact->exact
			  (floor
			   (safe-matrix-ref
			    optical-flow-horizontal
			    (inexact->exact (floor (/ (y point) 2)))
			    (inexact->exact (floor (/ (x point) 2)))
			    0))))
		      (+ (y point)
			 (inexact->exact
			  (floor
			   (safe-matrix-ref
			    optical-flow-vertical
			    (inexact->exact (floor (/ (y point) 2)))
			    (inexact->exact (floor (/ (x point) 2)))
			    0))))))
		    (region->boundary (pbm->points contour))) ))
  (pbm-erode
   5
   (pbm-flood
    (pbm-dialate 5 (points->pbm
  		    projected
  		    (pnm-height contour)
  		    (pnm-width contour)) )
    (quantize-point (centroid projected))))))

(define (forward-project-contour-fast contour optical-flow)
 ;; Relaxed projection, using average optical flow
 ;; and shifting the rigid contour
 (let* ((avg-flow (optical-flow-average-in-contour
		   optical-flow
		   contour))
	;; (optical-flow-horizontal (first optical-flow))
	;; (optical-flow-vertical (second optical-flow))
	;; (average-optical-flow-horizontal
	;;  (inexact->exact
	;;   (list-mean
	;;    (removeq
	;;     #f
	;;     (vector->list
	;;      (reduce
	;;       vector-append 
	;;       (vector->list
	;;        (map-indexed-matrix
	;; 	(lambda (v i j)
	;; 	 (if (safe-matrix-ref
	;; 	      (pbm-bitmap contour)
	;; 	      (* i 2) (* j 2) #f)
	;; 	     v
	;; 	     #f))
	;; 	optical-flow-horizontal)) '#()))))))
	;; (average-optical-flow-vertical
	;;  (inexact->exact
	;;   (floor 
	;;    (list-mean
	;;     (removeq
	;;      #f
	;;      (vector->list
	;;       (reduce
	;;        vector-append 
	;;        (vector->list
	;; 	(map-indexed-matrix
	;; 	 (lambda (v i j)
	;; 	  (if (safe-matrppix-ref
	;; 	       (pbm-bitmap contour)
	;; 	       (* i 2) (* j 2) #f)
	;; 	      v
	;; 	      #f))
	;; 	 optical-flow-vertical)) '#())))))))
	)
  (pbm-shift contour
	     ;; average-optical-flow-horizontal
	     (x avg-flow)
	     ;; average-optical-flow-vertical
	     (y avg-flow))))

(define (optical-flow-average optical-flow)
 (vector (inexact->exact
	  (list-mean
	   (vector->list
	    (reduce
	     vector-append 
	     (vector->list
	      (first optical-flow)) '#()))))
	 (inexact->exact
	  (list-mean
	   (vector->list
	    (reduce
	     vector-append 
	     (vector->list
	      (second optical-flow)) '#()))))))

(define (optical-flow-average-in-contour optical-flow contour)
 (vector (inexact->exact
	  (list-mean
	   (removeq
	    #f
	    (vector->list
	     (reduce
	      vector-append 
	      (vector->list
	       (map-indexed-matrix
		(lambda (v i j)
		 (if (safe-matrix-ref
		      (pbm-bitmap contour)
		      (* i 2) (* j 2) #f)
		     v
		     #f))
		(first optical-flow))) '#())))))
	 (inexact->exact
	  (floor 
	   (list-mean
	    (removeq
	     #f
	     (vector->list
	      (reduce
	       vector-append 
	       (vector->list
		(map-indexed-matrix
		 (lambda (v i j)
		  (if (safe-matrix-ref
		       (pbm-bitmap contour)
		       (* i 2) (* j 2) #f)
		      v
		      #f))
		 (second optical-flow))) '#()))))))))

(define (forward-project-contour-recursive contour optical-flows level final-level)
 ;; Recursively run fast projection
 (let ((new-contour (forward-project-contour-fast contour (car optical-flows))))
  (cond ((= level final-level) new-contour)
	(else (forward-project-contour-recursive
	       new-contour
	       (cdr optical-flows)
	       (+ level 1)
	       final-level)))))

(define (pbm-shift pbm x y)
 (define (bound lower i upper)
  (min upper (max lower i)))
 (let ((pbm-shifted (pbm-constant (pnm-width pbm) (pnm-height pbm) #f)))
  (map-indexed-matrix
   (lambda (v i j)
    (when v
     (matrix-set!
      (pbm-bitmap pbm-shifted)
      (bound 0 (+ i y) (pnm-height pbm))
      (bound 0 (+ j x) (pnm-width pbm))
      #t)))
   (pbm-bitmap pbm))
  pbm-shifted))

(define (pbm-erode n pbm)
 (let* ((height (pnm-height pbm))
	(width (pnm-width pbm))
	(bitmap (make-matrix height width #t)))
  (for-each-n
   (lambda (y)
    (for-each-n
     (lambda (x)
      (do ((y1 (max (- y n) 0) (+ y1 1)))
	((>= y1 (min (+ y (+ n 1)) height)))
       (do ((x1 (max (- x n) 0) (+ x1 1)))
	 ((>= x1 (min (+ x (+ n 1)) width)))
	(when (not (matrix-ref (pbm-bitmap pbm) y1 x1))
	 (matrix-set! bitmap y x #f)))))
     (pnm-width pbm)))
   (pnm-height pbm))
  (make-pbm #t bitmap)))

(define (pbm-dialate n pbm)
 (let* ((height (pnm-height pbm))
	(width (pnm-width pbm))
	(bitmap (make-matrix height width #f)))
  (for-each-n
   (lambda (y)
    (for-each-n
     (lambda (x)
      (do ((y1 (max (- y n) 0) (+ y1 1)))
	((>= y1 (min (+ y (+ n 1)) height)))
       (do ((x1 (max (- x n) 0) (+ x1 1)))
	 ((>= x1 (min (+ x (+ n 1)) width)))
	(when (matrix-ref (pbm-bitmap pbm) y1 x1)
	 (matrix-set! bitmap y x #t)))))
     (pnm-width pbm)))
   (pnm-height pbm))
  (make-pbm #t bitmap)))

(define (take-every n l)
 ;; Works for everything but lists with #f in them
 ;; Rather useless if the points aren't evenly distributed,
 ;; it turns out...
 (removeq
  #f
  (map-indexed
   (lambda (e i)
    (if (zero? (remainder (+ i 1) n)) e #f))
   l)))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up)))))
  ;;  (forward-project video-name)
  ((make-viterbi-video-frames video-name))))

;; Experiment

(define (experiment-1)
 ;; really sloppy
 (let ((video "Catch8_A1_C1_Act1_PARK3_BL_AFTN_48033e0c-c5af-11df-ab27-e80688cb869a")
       (start-frame 6)
       (number-to-project 5))
  (write-pnm
   (pbm-overlay-ppm
    (pgm->pbm
     (read-pnm
      (string-append "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/" video "/" (number->padded-string-of-length start-frame 4) "/frame-cropped-person-1-0-contour.pgm")) 128)
    (pgm->ppm
     (ppm->pgm
      (read-pnm
       (string-append "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/" video "/" (number->padded-string-of-length start-frame 4) "/frame-cropped-person-1.ppm"))))) "test0-render.ppm")
  (write-pnm
   (pbm->pgm
    (forward-project-contour
     (pgm->pbm
      (read-pnm
       (string-append "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/" video "/" (number->padded-string-of-length start-frame 4) "/frame-cropped-person-1-0-contour.pgm")) 128)
     (crop-optical-flow
      (read-optical-flow
       (string-append "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/" video "/" (number->padded-string-of-length start-frame 4) "/optical-flow.ssv.sc"))
      (first (read-voc4-boxes
	      (string-append "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/" video "/" (number->padded-string-of-length start-frame 4) "/voc4-person-1.smooth_tracked_box"))))))
   "test1.pgm")  
  (for-each-n
   (lambda (n)
    (let ((frame (+ start-frame (+ n 1))))
     (write-pnm
      (pbm->pgm
       (forward-project-contour
	(pgm->pbm
	 (read-pnm
	  (string-append "test" (number->string (+ n 1))  ".pgm")) 128)
	(crop-optical-flow
	 (read-optical-flow
	  (string-append "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/" video "/" (number->padded-string-of-length frame 4) "/optical-flow.ssv.sc"))
	 (first (read-voc4-boxes
		 (string-append "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/" video "/" (number->padded-string-of-length frame 4) "/voc4-person-1.smooth_tracked_box"))))))
      (string-append "test" (number->string (+ n 2))  ".pgm"))
     
     (write-pnm
      (pbm-overlay-ppm
       (pgm->pbm
	(read-pnm (string-append "test" (number->string (+ n 1))  ".pgm")) 128)
       (pgm->ppm
	(ppm->pgm
	 (read-pnm
	  (string-append "/home/waggonej/video-datasets/C-D1a/SINGLE_VERB/" video "/" (number->padded-string-of-length frame 4) "/frame-cropped-person-1.ppm")))))
      (string-append "test" (number->string (+ n 1))  "-render.ppm"))))
   number-to-project)))

;; Zhiqi didn't generate these, so I have to
(define (generate-cropped  video-name)
 (for-each-frame
  (lambda (frame)
   (let* ((model "person")
	  (model-number 1)
	  (path (lambda (type . args)
		 (apply
		  generate-pathname
		  (append
		   `(,(generic-pathname video-name frame "") "frame-cropped" ,type ,model ,(number->string model-number))
		   args))))
	  (full-image (read-pnm (ppm-full-pathname video-name frame)))
	  (voc4 (fix-voc4-box
		 full-image
		 (first (read-voc4-boxes
			  (smooth-tracked-box-pathname video-name frame model (number->string model-number))))))
	  (image
	   (crop-voc4
	  	full-image
	  	voc4)))
    (display (string-append "Frame " (number->string frame))) (newline)
    (write-pnm image (path 'image))))
  video-name))