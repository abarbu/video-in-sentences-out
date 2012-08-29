(MODULE
  FEATURE-VECTORS
  (WITH
    QOBISCHEME
    XLIB
    TOOLLIB-MATLAB
    TOOLLIB-MISC
    TOOLLIB-C-BINDINGS
    TOOLLIB-IMAGE-PROCESSING
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    IDEALIB-MATPLOTLIB)
  (MAIN MAIN))
;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "feature-vectors.sch")

;; darpa-wrap ./feature-vectors -darpa CHASE3_A1_C1_Act2_6_URBAN_MC_AFTN_b436e7cc-07b6-11e0-a6d4-e80688cb869a bicycle-1

(set! *program* "feature-vectors")
(set! *panic?* #t)

(define (video-best-participant video tracks)
 (define (get-track type)
  (find-if (lambda (t) (equal? type (track-annotation-name t))) tracks))
 (if (null? tracks)
     #f
     (cond ((get-track "person") (get-track "person"))
	   ((get-track "car") (get-track "car"))
	   ((get-track "suv") (get-track "suv"))
	   ((get-track "motorcycle") (get-track "motorcycle"))
	   (else (first tracks)))))

(define (tracks-by-priority video)
 (map (lambda (a) (list (track-annotation-name a)
		   (track-annotation-number a)))
      (let loop ((tracks
		  (remove-if-not
		   track-annotation-good?
		   (read-object-from-file
		    (human-track-annotation-pathname video)))))
       (if (null? tracks)
	   '()
	   (let ((track (video-best-participant video tracks)))
	    (cons track (loop (remove track tracks))))))))

(define (write-feature-vectors video track-names lookahead)
 (let* ((boxes (map (lambda (track-name)
		     (read-voc4-overgenerated-smooth-tracked-boxes
		      video (first track-name) (second track-name)))
		    track-names))
	(features (compute-feature-vectors boxes lookahead)))
  (format #t "Wrote FV ~a~%" (features-pathname video (length track-names)))
  (write-file
   (join (map (lambda (fv) (list "1" (string*-join " " (vector->list fv))))
	      (vector->list features)))
   (features-pathname video (length track-names)))))

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
       (at-most-one ("lookahead" lookahead? (lookahead "n" integer-argument 2)))
       (any-number ("participant" participant?
		    (participant "name-nr" string-argument)))
       (at-most-one ("from-human-annotation" from-human-annotation?)))
 (when (and participant? from-human-annotation?)
  (panic "Can either say -participant or -from-human-annotation not both"))
 (unless (or participant? from-human-annotation?)
  (panic "Need either -participant or -from-human-annotation"))
 (let* ( ;; QobiScheme gives this to us in reverse order
	(video (cond (standard?
		      (standard-corpus-video corpus sequence person location n))
		     (darpa? (string->darpa-video name))
		     (stand-alone? (make-stand-alone-video path))
		     (demo? (string->demo-video demo-name))
		     (else (fuck-up))))
	(track-names
	 (cond (participant?
		(map (lambda (l) (pregexp-split "-" l))
		     (reverse participant)))
	       (from-human-annotation? (tracks-by-priority video)))))
  (display track-names)(newline)
  (if from-human-annotation?
      (let loop ((track-names (take-if-possible 2 track-names)))
       (if (null? track-names)
	   #f
	   (begin
	    (write-feature-vectors video track-names lookahead)
	    (loop (but-last track-names)))))
      (write-feature-vectors video track-names lookahead))))
