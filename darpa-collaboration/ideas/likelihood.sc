(MODULE
  LIKELIHOOD
  (WITH
    QOBISCHEME
    XLIB
    TOOLLIB-MATLAB
    TOOLLIB-MISC
    TOOLLIB-C-BINDINGS
    TOOLLIB-IMAGE-PROCESSING
    IDEALIB-HASH-TABLE
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    IDEALIB-MATPLOTLIB
    HMM-WBM
    HMM-TRAIN-CLASSIFY)
  (MAIN MAIN))

(include "QobiScheme.sch")
(include "likelihood.sch")

(set! *program* "likelihood")
(set! *panic?* #f)

(define-structure trained-hmm verb videos states log-likelihood model)

;; TODO We should abstract this out and it should be part of the hmm
;; as it is needed for the pose codebook
(define *person-poses* '("person" "person-crouch" "person-down"))

(define (write-hmm trained-hmm filename)
 (write-object-to-file
  (make-trained-hmm (trained-hmm-verb trained-hmm)
		    (trained-hmm-videos trained-hmm)
		    (trained-hmm-states trained-hmm)
		    (trained-hmm-log-likelihood trained-hmm)
		    (model->psi "" #f #f (trained-hmm-model trained-hmm)))
  filename))

;;; needs work: human-track-annotation-pathname
(define (read-videos-file-for-features videos-file feature)
 (remove-if
  (lambda (v)
   (< (count-if track-annotation-good?
		(read-object-from-file (human-track-annotation-pathname v)))
      feature))
  (read-file videos-file)))

;;; needs work: FV dependent
(define (num-discrete i)
 (case i
  ;; cluster indices
  ((10 23) 50)
  ;; model-indices
  ((11 24) 38)
  ;; filter indices
  ((12 25) 7)
  (else (fuck-up))))

;;; needs work: FV dependent
(define (*feature-types* feature)
 (map-n
  (lambda (i) (case i
	  ((5 7 18 20 28) 'radial)
	  ((10 11 12 23 24 25) 'discrete)
	  (else 'continuous)))
  (case feature
   ((1) 13)
   ((2) (+ (* 2 13) 3))
   (else (fuck-up)))))

(define (get-human-annotation-tracks video)
 (map (lambda (track-name)
       (list (string-append "voc4_overgenerated-"
			    (first track-name)
			    "-"
			    (second track-name)
			    ".smooth-tracked-box")
	     (read-voc4-overgenerated-smooth-tracked-boxes
	      video (first track-name) (second track-name))))
      (take-if-possible 2 (tracks-by-priority video))))

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
 (map (lambda (a) (list (track-annotation-name a) (track-annotation-number a)))
      (let loop ((tracks (remove-if-not
			  track-annotation-good?
			  (read-object-from-file (human-track-annotation-pathname video)))))
       (if (null? tracks)
	   '()
	   (let ((track (video-best-participant video tracks)))
	    (cons track (loop (remove track tracks))))))))

;;; needs work: bring up to speed
(define (compute-fv video feature)
 (let ((video (string->darpa-video video))
       (tracks (map (lambda (track-name)
		     (read-voc4-overgenerated-smooth-tracked-boxes
		      video (first track-name) (second track-name)))
		    (case feature
		     ((1) (take-if-possible 1 (tracks-by-priority video)))
		     ((2) (take-if-possible 2 (tracks-by-priority video)))
		     (else (fuck-up))))))
  (vector->list
   (map-vector
    vector->list
    (case feature
     ((1) (if (= (length tracks) 1)
	      (remove-bad-features (single-track-features-typed 'discrete-pose (first tracks) 2) 1)
	      (begin (write video) (newline) '#())))
     ((2) (if (= (length tracks) 2)
	      (let ((singles (map (lambda (track) (single-track-features-typed 'discrete-pose track 2)) tracks))
		    (pairs (map-all-ordered-pairs (lambda (t1 t2) (pairwise-track-features t1 t2 2)) tracks)))
	       (remove-bad-features
		(apply
		 map-vector
		 (cons (lambda (. vectors) (list->vector (join (map vector->list vectors))))
		       (append singles pairs)))
		2))
	      (begin (write video) (newline) '#())))
     (else (fuck-up)))))))

;;; needs work: FV dependent
;;; needs work: bring up to speed
(define (train-hmm-with-discrete verb videos states features feature-types
				 training-epsilon min-epochs max-epochs upper-triangular? random-a-and-b?
				 restarts)
 (let* ((feature-vectors (remove-if null? (map (lambda (v) (compute-fv v features)) videos)))
	(rmats (map features->rmat feature-vectors))
	(number-of-features (length (caar feature-vectors)))
	(model (allocate-model number-of-features states))
	(best-model (allocate-model number-of-features states))
	(rmat-vector (allocate-rmat-vector (length rmats))))
  (for-each-n
   (lambda (i)
    (case (list-ref feature-types i)
     ((radial)
      (format #t "radial: ~a~%" i)
      (set-model-feature-type-radial! model i)
      (set-model-feature-type-radial! best-model i))
     ((continuous)
      (format #t "continuous: ~a~%" i)
      (set-model-feature-type-continuous! model i 2000.0)
      (set-model-feature-type-continuous! best-model i 2000.0))
     ((discrete)
      (format #t "discrete: i ~a di ~a~%" i (num-discrete i))
      (set-model-feature-type-discrete! model i)
      (set-model-feature-type-discrete! best-model i)
      (for-each-n
       (lambda (s)
	(set-model-parameter! model s i 0 (num-discrete i))
	(set-model-parameter! best-model s i 0 (num-discrete i)))
       states))
     (else (fuck-up))))
   number-of-features)
  (for-each-indexed (lambda (rmat i) (rmat-vector-set! rmat-vector i rmat))
		    rmats)
  (let outer ((nn restarts)
	      (best-model best-model)
	      (best minus-infinity)
	      (model model))
   (format #t "HMM Training random-restart ~a (best ~a):~%" (- restarts nn) best)
   (pp (model->psi "" #f #f best-model))
   (newline)
   (format #t "best-model: ")
   (write (make-trained-hmm verb videos states best
   			    (model->psi "" #f #f best-model)))
   (newline)
   (cond ;; todo returning psi was more user friendly
    ((zero? nn)
     (free-model model)
     ;; we return this now, you should remember to free it
     (when #f (free-model best-model))
     (for-each free-rmat rmats)
     (free-rmat-vector rmat-vector)
     (make-trained-hmm verb videos states best best-model))
    (else
     (format #t "here o1~%")
     (randomise-model! model upper-triangular? random-a-and-b?)
     (format #t "here o2~%")
     (force-init-globals!)
     (format #t "here o3~%")
     (let inner ((last (rmats-log-likelihood model rmats)) (epoch 1))
      (format #t "iteration ~a likelihood ~a~%" epoch last)
      (cond ((and max-epochs (> epoch max-epochs))
	     (outer (- nn 1) best-model best model))
	    (else (format #t "here i1~%")
		  (update-model! model rmat-vector (length rmats))
		  (format #t "here i2~%")
		  (let ((this (rmats-log-likelihood model rmats)))
		   (when (> last this) (format #t "Increasing likelihood ~a -> ~a~%" last this))
		   (format #t "Would stop? ~a~%"
			   (< (/ (- this last) (abs (* 2 (+ this last)))) training-epsilon))
		   (cond ((and (< (/ (- this last) (abs (* 2 (+ this last))))
				  training-epsilon)
			       (>= epoch min-epochs))
			  (begin
			   (format #t "~a~%" this)
			   (if (> this best)
			       (outer (- nn 1) model this best-model)
			       (outer (- nn 1) best-model best model))))
			 (else (inner this (+ epoch 1)))))))))))))

;;; needs work: remove hardcoding
(define (train-and-write-with-discrete verb videos-file states feature output)
 (format #t "With discrete~%")
 (write-hmm
  (train-hmm-with-discrete verb
			   ;; (read-file videos-file)
			   (take-if-possible
			    200
			    (deal (read-videos-file-for-features videos-file feature)))
			   states
			   feature
			   (*feature-types* feature)
			   1e-06 20 #f #t #t
			   20)
  (string-append "/aux/"
		 (getenv "USER")
		 "/cd1-hmm-with-discrete/hmm-"
		 (number->string feature)
		 "-"
		 (number->string states)
		 "-" verb "-" output ".sc")))


(define (unwanted-model? track-data profile-data dropped-models)
 (let ((stats (find-if (lambda (a) (equal? (first a) (second track-data))) profile-data)))
  (unless stats (fuck-up))		; so we know
  (and (not (equal? (second track-data) "person"))
       (or (member (second track-data) dropped-models)
	   (and (string->number (list-ref stats 2)) (< (string->number (list-ref stats 2)) 0.035))
	   (and (string->number (list-ref stats 5)) (< (string->number (list-ref stats 5)) 0.35))))))

(define-command (main (exactly-one ("train" train?) ("test" test?))
		      (exactly-one ("non-pose" non-pose?)
				   ("continuous-pose" continuous-pose?)
				   ("discrete-pose" discrete-pose?))
		      (required (video "video" string-argument))
		      (required (states "states" integer-argument))
		      (required (models "models-list" string-argument))
		      (required (output "output" string-argument)))
 (let ((type (cond (non-pose? 'non-pose)
		   (continuous-pose? 'continuous-pose)
		   (discrete-pose? 'discrete-pose)
		   (else (fuck-up))))
       (profile-data 
	(map (lambda (r) (pregexp-split "," r))
	     (read-file (track-profile-pathname video))))
       (precomputed-likelihoods
	(alist->hash-table
	 (if (file-exists? output)
	     (map (lambda (a) (cons (list (second (result-name a)) (result-verb a)) a))
		  (join (removeq #f (read-object-from-file output))))
	     '())))
       ;; FIXME hardcoded for now but should be commandline arguments
       ;; or live in some file
       (dropped-models
	'("baseball-bat" "pogo-stick" "bucket" "golf-club" "trailer" "rake" "closet"
	  "trash-bag" "shovel" "garbage-can")))
  (cond (train? (fuck-up))
	(test? 
	 (write-object-to-file
	  (compute-likelihoods-from-files
	   type video (number->string states)
	   models output medoids
	   (lambda (tracks) 
	    (unwanted-model? tracks profile-data dropped-models))
	   (lambda (video tracks models compute)
	    (let ((key (lambda (model)
			(append (let ((t (map first tracks)))
				 ;; FIXME This is a bad convention
				 (if (= (length t) 1) (first t) t))
				(list (trained-hmm-verb (first model)))))))
	     (if (hash-table-ref likelihoods-hash-table (key (first models)) #f)
		 (map (lambda (m) (hash-table-ref likelihoods-hash-table (key m))) models)
		 (compute))))
	   precomputed-likelihoods #f dropped-models)
	  output))
	(else (fuck-up)))
  (format #t "Completed without fucking up!~%DONE~%")))
