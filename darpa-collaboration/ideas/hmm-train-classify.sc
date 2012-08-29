(MODULE HMM-TRAIN-CLASSIFY)
;;; LaHaShem HaAretz U'Mloah

(include "QobiScheme.sch")
(include "hmm-train-classify.sch")

;;; Constants

(define-c-external (c-int-vector-ref pointer int) int "c_int_vector_ref")
(define-c-external (free-c-vector! pointer) void "free_c_vector")

(define sqrt-two-pi (sqrt (* 2.0 pi)))

(define log-two-pi (log (* 2.0 pi)))

;;; Structures

(define-structure match psi log-likelihood permutation)

(define-structure state-sequence states deltas)

;;; Variables

(define *delta* 1e-11)

(define *verbose?* #f)

;;; Procedures

(define (log-univariate-gaussian x mu sigma)
 (- (* -0.5 (sqr (/ (- x mu) sigma))) (log (* sigma sqrt-two-pi))))

(define (log-d-univariate-gaussian/d-x x mu sigma)
 (+ (log (/ (- mu x) (* sigma sigma sigma)))
    (log-univariate-gaussian x mu sigma)))

(define (lbessi0 x)
 (if (< (abs x) 3.75)
     (let ((y (sqr (/ x 3.75))))
      (log (+ 1.0
	      (* y
		 (+ 3.5156229
		    (* y
		       (+ 3.0899424
			  (* y
			     (+ 1.2067492
				(* y
				   (+ 0.2659732
				      (* y
					 (+ 0.360768e-1
					    (* y 0.45813e-2))))))))))))))
     (let* ((ax (abs x))
	    (y (/ 3.75 ax)))
      (+ (- ax (* 0.5 (log ax)))
	 (log
	  (+
	   0.39894228
	   (* y
	      (+
	       0.1328592e-1
	       (* y
		  (+
		   0.225319e-2
		   (* y
		      (+
		       -0.157565e-2
		       (* y
			  (+
			   0.916281e-2
			   (* y
			      (+
			       -0.2057706e-1
			       (* y
				  (+
				   0.2635537e-1
				   (* y
				      (+
				       -0.1647633e-1
				       (* y 0.392377e-2)))))))))))))))))))))

(define (log-von-mises x mean kappa)
 (- (* kappa (cos (- x mean))) log-two-pi (lbessi0 kappa)))

(define (log-d-von-mises/d-x x mean kappa)
 (+ (log (- (* kappa (sin (- x mean))))) (log-von-mises x mean kappa)))

(define (log-likelihood-of-feature-matrix psi feature-matrix)
 (let* ((uu (vector-length (psi-b psi)))
	(tt (matrix-rows feature-matrix))
	(ii (matrix-columns feature-matrix))
	(delta (make-matrix uu tt))
	(log-a (map-vector (lambda (row) (map-vector log row)) (psi-a psi)))
	(b (psi-b psi))
	(parameters (psi-parameters psi)))
  (for-each-n
   (lambda (t)
    (let ((feature-vector (vector-ref feature-matrix t)))
     (for-each-n
      (lambda (u)
       (let ((parameters (vector-ref parameters u)))
	(matrix-set!
	 delta u t
	 ;; needs work: Special version of sum that uses 0.0 instead of 0.
	 (+ (sum (lambda (i)
		  (let ((parameters (vector-ref parameters i)))
		   (case (first parameters)
		    ((continuous)
		     (log-univariate-gaussian (vector-ref feature-vector i)
					      (second parameters)
					      (third parameters)))
		    ((radial)
		     (log-von-mises (vector-ref feature-vector i)
				    (second parameters)
				    (third parameters)))
		    ((discrete)
		     (log (list-ref (rest parameters)
				    (round (vector-ref feature-vector i)))))
		    (else (fuck-up)))))
		 ii)
	    (if (zero? t)
		(log (vector-ref b u))
		(log-sum
		 (lambda (v)
		  (+ (matrix-ref log-a v u) (matrix-ref delta v (- t 1))))
		 uu))))))
      uu)))
   tt)
  (/ (log-sum (lambda (u) (matrix-ref delta u (- tt 1))) uu) ii)))

(define (e+ e0 e1)
 (make-ellipse (+ (ellipse-x0 e0) (ellipse-x0 e1))
	       (+ (ellipse-y0 e0) (ellipse-y0 e1))
	       (+ (ellipse-t0 e0) (ellipse-t0 e1))
	       (+ (ellipse-a e0) (ellipse-a e1))
	       (+ (ellipse-b e0) (ellipse-b e1))))

(define (sum-ellipse f n)
 (let loop ((n (- n 1))
	    ;; The constants are hardwired to be inexact for efficiency.
	    (c (make-ellipse 0.0 0.0 0.0 0.0 0.0)))
  (if (negative? n) c (loop (- n 1) (e+ c (f n))))))

(define (k*e k e)
 (make-ellipse (* k (ellipse-x0 e))
	       (* k (ellipse-y0 e))
	       (* k (ellipse-t0 e))
	       (* k (ellipse-a e))
	       (* k (ellipse-b e))))

(define (psi-ii psi) (vector-length (vector-ref (psi-parameters psi) 0)))

(define (psi-uu psi) (vector-length (psi-parameters psi)))

(define (psi->model psi)
 (let ((model (allocate-model (psi-ii psi) (psi-uu psi))))
  (do ((i 0 (+ i 1))) ((= i (psi-ii psi)))
   (case (first (matrix-ref (psi-parameters psi) 0 i))
    ((continuous) (set-model-feature-type-continuous! model i 100.0))
    ((radial) (set-model-feature-type-radial! model i))
    ((discrete) (set-model-feature-type-discrete! model i))
    (else (panic "Unrecognized feature type"))))
  (do ((u 0 (+ u 1))) ((= u (psi-uu psi)))
   (set-model-b! model u (vector-ref (psi-b psi) u))
   (do ((v 0 (+ v 1))) ((= v (psi-uu psi)))
    (set-model-a! model u v (matrix-ref (psi-a psi) u v)))
   (do ((i 0 (+ i 1))) ((= i (psi-ii psi)))
    (let ((parameters (matrix-ref (psi-parameters psi) u i)))
     (case (first parameters)
      ((continuous)
       (set-model-parameter! model u i 0 (second parameters))
       (set-model-parameter! model u i 1 (third parameters)))
      ((radial)
       (set-model-parameter! model u i 0 (second parameters))
       (set-model-parameter! model u i 1 (third parameters)))
      ((discrete)
       (set-model-parameter! model u i 0 (length (rest parameters)))
       (for-each-indexed
	(lambda (parameter j)
	 (set-model-parameter! model u i (+ j 1) parameter))
	(rest parameters)))
      (else (panic "Unrecognized feature type"))))))
  model))

(define (model->psi name features kk model)
 (make-psi name
	   features
	   kk
	   (let ((parameters (make-matrix (model-uu model) (model-ii model))))
	    (do ((u 0 (+ u 1))) ((= u (model-uu model)))
	     (do ((i 0 (+ i 1))) ((= i (model-ii model)))
	      (matrix-set! parameters
			   u
			   i
			   (if (= (model-feature-type model i) 2)
			       (cons
				'discrete
				(map-n
				  (lambda (j) (model-parameter model u i (+ j 1)))
				 (round (model-parameter model u i 0))))
			       (list
				(case (model-feature-type model i)
				 ((0) 'continuous)
				 ((1) 'radial)
				 (else (panic "Unrecognized feature type")))
				(model-parameter model u i 0)
				(model-parameter model u i 1))))))
	    parameters)
	   (let ((a (make-matrix (model-uu model) (model-uu model))))
	    (do ((u 0 (+ u 1))) ((= u (model-uu model)))
	     (do ((v 0 (+ v 1))) ((= v (model-uu model)))
	      (matrix-set! a u v (model-a model u v))))
	    a)
	   (let ((b (make-vector (model-uu model))))
	    (do ((u 0 (+ u 1))) ((= u (model-uu model)))
	     (vector-set! b u (model-b model u)))
	    b)))

(define (format-state-sequence sequence first-frame)
 (let ((string (string))
       (states (state-sequence-states sequence))
       (deltas (state-sequence-deltas sequence))
       (counter 0))
  (for-each-n (lambda (n)
	       (let* ((new-string
		       (format #f "~s -~s-> "
			       (vector-ref states n)
			       (+ (vector-ref deltas n) first-frame 2)))
		      (ns-length (string-length new-string)))
		(when (> (+ counter ns-length) 75)
		 (set! string (string-append string (format #f "~%")))
		 (set! counter 0))
		(set! string (string-append string new-string))
		(set! counter (+ counter ns-length))))
	      (vector-length deltas))
  (string-append
   string (format #f "~s" (vector-ref states (vector-length deltas))))))

(define (best-states tt model rmat)
 (let* ((best-state-sequence (best-state-sequence model rmat))
	(counter 0)
	(states (make-vector tt -1))
	(deltas (make-vector tt)))
  (when (= tt 0) (panic "BEST-STATES: movie has zero length"))
  (vector-set! states 0 (c-int-vector-ref best-state-sequence 0))
  (for-each-n (lambda (t)
	       (let ((state (c-int-vector-ref best-state-sequence t)))
		(unless (= (vector-ref states counter) state)
		 (vector-set! deltas counter t)
		 (set! counter (+ counter 1))
		 (vector-set! states counter state))))
	      tt)
  (let ((s (make-vector (+ counter 1)))
	(d (make-vector counter)))
   (vector-set! s counter (vector-ref states counter))
   (for-each-n (lambda (i)
		(vector-set! s i (vector-ref states i))
		(vector-set! d i (vector-ref deltas i)))
	       counter)
   (free-c-vector! best-state-sequence)
   (make-state-sequence s d))))

(define (rmats-log-likelihood model rmats)
 (reduce + (map (lambda (rmat) (model-log-likelihood model rmat)) rmats) 0.0))

;;; Tam V'Nishlam Shevah L'El Borei Olam
