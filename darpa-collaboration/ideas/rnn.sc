(MODULE
  RNN
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
;;; Copyright 2011 Purdue University. All rights reserved.

(include "QobiScheme-AD.sch")
(include "rnn.sch")

(set! *program* "rnn")
(set! *panic?* #f)

;;; Representation for weights:
;;;  list with one element for each layer following the input;
;;;  each such list has one element for each unit in that layer;
;;;  which consists of a bias, followed by the weights for each
;;;  unit in the previous layer.

;;; Basic MLP

(define (sigmoid x) (/ 1 (+ (exp (- 0 x)) 1)))

(define (neuron inputs)
 (lambda (weights)
  (unless (= (length inputs) (length (rest weights))) (fuck-up))
  (sigmoid (map-reduce + (first weights) * (rest weights) inputs))))

(define (neuron-layer inputs weights) (map (neuron inputs) weights))

(define (neuron-network weights)
 (lambda (inputs)
  (if (null? weights)
      inputs
      ((neuron-network (rest weights)) (neuron-layer inputs (first weights))))))

(define (recurrent-neuron-network weights)
 (lambda (inputs)
  (let loop ((inputs inputs) (state (map (lambda (i) 0) (last weights))))
   (if (null? inputs)
       state
       (loop (rest inputs)
	     ((neuron-network weights) (append (first inputs) state)))))))

(define (error-on-dataset dataset)
 (lambda (weights)
  (map-reduce
   +
   0
   (lambda (exemplar)
    (let ((inputs (first exemplar)) (outputs (second exemplar)))
     (map-reduce
      + 0 sqr (map - outputs ((recurrent-neuron-network weights) inputs)))))
   dataset)))

;;; Optimization of the sort used with MLPs and backpropagation,
;;; often called "vanilla backprop"

;;; Scaled structure subtraction
;;; computes x-k*y where x and y are vectors and k is a scalar

(define (s-k* x k y)
 (cond ((real? x) (- x (* k y)))
       ((pair? x) (cons (s-k* (car x) k (car y)) (s-k* (cdr x) k (cdr y))))
       (else x)))

;;; Vanilla gradient optimization.
;;; Gradient minimize f starting at w0 for n iterations via
;;; w(t+1) = w(t) - eta * grad_w f.
;;; returns the last f(w)

(define (weight-gradient f)
 (lambda (ws)
  (set! *e* (+ *e* 1))
  (let* ((ws (map (lambda (l)
		   (map (lambda (u) (map (lambda (w) (tape *e* w '() '())) u))
			l))
		  ws))
	 (y (f ws)))
   (cond ((and (tape? y) (not (<_e (tape-epsilon y) *e*)))
	  (determine-fanout! y)
	  (reverse-phase! 1 y)))
   (set! *e* (- *e* 1))
   (map (lambda (l) (map (lambda (u) (map tape-sensitivity u)) l)) ws))))

(define (vanilla f weights0 n eta)
 (if (zero? n)
     (f weights0)
     (vanilla
      f (s-k* weights0 eta ((weight-gradient f) weights0)) (- n 1) eta)))

;;; XOR network

(define (random-weight) (- (* 2 (random-real)) 1))

(define (parity-ws0)
 (list (list (list (random-weight) (random-weight) (random-weight))
	     (list (random-weight) (random-weight) (random-weight)))
       (list (list (random-weight) (random-weight) (random-weight)))))

(define (parity-data1)
 '((((0)) (0))
   (((1)) (1))))

(define (parity-data2)
 '((((0) (0)) (0))
   (((0) (1)) (1))
   (((1) (0)) (1))
   (((1) (1)) (0))))

(define (parity-data3)
 '((((0) (0) (0)) (0))
   (((0) (0) (1)) (1))
   (((0) (1) (0)) (1))
   (((0) (1) (1)) (0))
   (((1) (0) (0)) (1))
   (((1) (0) (1)) (0))
   (((1) (1) (0)) (0))
   (((1) (1) (1)) (1))))

(define (parity-data4)
 '((((0) (0) (0) (0)) (0))
   (((0) (0) (0) (1)) (1))
   (((0) (0) (1) (0)) (1))
   (((0) (0) (1) (1)) (0))
   (((0) (1) (0) (0)) (1))
   (((0) (1) (0) (1)) (0))
   (((0) (1) (1) (0)) (0))
   (((0) (1) (1) (1)) (1))
   (((1) (0) (0) (0)) (1))
   (((1) (0) (0) (1)) (0))
   (((1) (0) (1) (0)) (0))
   (((1) (0) (1) (1)) (1))
   (((1) (1) (0) (0)) (0))
   (((1) (1) (0) (1)) (1))
   (((1) (1) (1) (0)) (1))
   (((1) (1) (1) (1)) (0))))

(define (test n)
 (vanilla (error-on-dataset
	   (append (parity-data1) (parity-data2) (parity-data3) (parity-data4)))
	  (parity-ws0) n 0.3))

(define (g-tapes e)
 (cond ((number? e) (tape *e* e '() '()))
       ((vector? e) (map-vector g-tapes e))
       ((list? e) (map g-tapes e))
       (else (fuck-up))))

(define (g-sensitivities e)
 (cond ((tape? e) (tape-sensitivity e))
       ((vector? e) (map-vector g-sensitivities e))
       ((list? e) (map g-sensitivities e))
       (else (fuck-up))))

(define (g-dot a b)
 (cond ((number? a) (* a b))
       ((vector? a) (reduce-vector + (map-vector g-dot a b) 0))
       ((list? a) (reduce + (map g-dot a b) 0))
       (else (fuck-up))))

(define (g+ a b)
 (cond ((number? a) (+ a b))
       ((vector? a) (map-vector g+ a b))
       ((list? a) (map g+ a b))
       (else (fuck-up))))

(define (g- a b)
 (cond ((number? a) (- a b))
       ((vector? a) (map-vector g- a b))
       ((list? a) (map g- a b))
       (else (fuck-up))))

(define (k*g k g)
 (define (*g g)
  (cond ((number? g) (* k g))
	((vector? g) (map-vector *g g))
	((list? g) (map *g g))
	(else (fuck-up))))
 (*g g))

(define (gradient-R-g f)
 (lambda (a)
  (set! *e* (+ *e* 1))
  (let* ((t (g-tapes a)) (r (f t)))
   (cond ((and (tape? r) (not (<_e (tape-epsilon r) *e*)))
	  (determine-fanout! r)
	  (reverse-phase! 1 r)))
   (set! *e* (- *e* 1))
   (g-sensitivities t))))

(define (g-magnitude g) (sqrt (g-dot g g)))
(define (g-distance a b) (g-magnitude (g- b a)))

(define (multivariate-argmin-F-args f x start max tol)
 (let ((g (gradient-R-g f)))
  (letrec ((loop
	    (lambda (x fx gx eta i j)
	     (when (or (and (= (modulo i 10) 0) #t) #f)
	      (write x) (newline)
	      (write fx) (newline)
	      (write gx) (newline)
	      (write eta) (newline)
	      (write i) (newline)
	      (write j) (newline)
	      (newline))
	     (cond ((or (<= (g-magnitude gx) tol) (= j max))
		    x)
		   ((= i 10) (loop x fx gx (* 2.0 eta) 0 j))
		   (else
		    (let ((x-prime (g- x (k*g eta gx))))
		     (if (<= (g-distance x x-prime) tol)
			 x
			 (let ((fx-prime (f x-prime)))
			  (if (< fx-prime fx)
			      (loop x-prime fx-prime (g x-prime) eta (+ i 1) (+ j 1))
			      (loop x fx gx (/ eta 2.0) 0 j))))))))))
   (loop x (f x) (g x) start 0 0))))

(define (video-length2 vignette)
 (length
  (directory-list
   (string-append
    (if #t
	"/aux/abarbu/steele/"
	"/home/andrei/video-datasets/C-D1a/SINGLE_VERB/")
    vignette "/*/features-1.text"))))

(define (on i n)
 (let ((bits (make-vector n 0)))
  (vector-set! bits i 1)
  (vector->list bits)))

(define (unary x low high n)
 (when (>= (inexact->exact (round (* (- n 1) (/ (- x low) (- high low))))) n)
  (format #t "~a ~a ~a (~a,~a)~%" (inexact->exact (round (* (- n 1) (/ (- x low) (- high low))))) n x low high)
  (fuck-up))
 (on (inexact->exact (round (* (- n 1) (/ (- x low) (- high low))))) n))

(define (read-feature-vector vignette frame bits)
 (let ((raw (map string->number
		 (read-file
		  (string-append (if #t
				     "/aux/abarbu/steele/"
				     "/home/andrei/video-datasets/C-D1a/SINGLE_VERB/")
				 vignette
				 "/"
				 (number->padded-string-of-length frame 4)
				 "/features-1.text")))))
  (if (and (= (first raw) -1) (= (second raw) -1))
      (map-n (lambda (i) 0) (* 8 bits))
      (append (unary (first raw) 0 1279 bits)
	      (unary (second raw) 0 719 bits)
	      (unary (third raw) 0 100 bits)
	      (unary (fourth raw) -20 20 bits)
	      (unary (fifth raw) -0 2000 bits)
	      (unary (sixth raw) minus-pi pi bits)
	      (unary (seventh raw) 0 2000 bits)
	      (unary (eighth raw) minus-pi pi bits)))))

(define (read-time-series vignette bits)
 (map-n (lambda (frame) (read-feature-vector vignette (+ frame 1) bits))
	(video-length2 vignette)))

(define *jump-vignettes*
 '("Jump4_A1_C1_Act1_DOWNTOWN1A3_FC_MIDD_47b1f18a-c5af-11df-9563-e80688cb869a"
   "Jump4_A1_C1_Act1_PARK1_MC_AFTN_47e94d1c-c5af-11df-a4b5-e80688cb869a"
   "Jump4_A1_C1_Act1_PARK1_ML_MIDD_446826fd-c5af-11df-9771-e80688cb869a"
   "Jump4_A1_C1_Act1_PARK2_MR_MIDD_44687f6e-c5af-11df-bc7e-e80688cb869a"
   "Jump4_A1_C1_Act1_Park3_MC_AFTN_482162cc-c5af-11df-bb07-e80688cb869a"))

(define *bounce-vignettes*
 '("Bounce1_A1_C1_Act4_DOWNTOWN1A3_ML_MIDD_479d8345-c5af-11df-8e21-e80688cb869a"
   "Bounce1_A1_C1_Act4_PARK1_MC_AFTN_47cbce0c-c5af-11df-be7b-e80688cb869a"
   "Bounce1_A1_C1_Act4_PARK1_ML_MIDD_442bdd2b-c5af-11df-957b-e80688cb869a"
   "Bounce1_A1_C1_Act4_PARK2_MR_MIDD_442c3994-c5af-11df-8259-e80688cb869a"
   "Bounce1_A1_C1_Act4_Park3_MC_AFTN_4802774c-c5af-11df-9211-e80688cb869a"))

(define *arrive-vignettes*
 '("Arrive4_A1_C1_Act1_2_URBAN7_ML_MORN_48a47eb5-c5af-11df-a64f-e80688cb869a"
   "Arrive4_A2_C1_Act1_2_URBAN7_MC_MORN_48a4d6c2-c5af-11df-9f63-e80688cb869a"
   "Arrive6_A1_C1_Act5_URBAN7_MR_MORN_48a53107-c5af-11df-a876-e80688cb869a"
   "Arrive6_A2_C1_Act5_URBAN7_BR_MORN_48a5894f-c5af-11df-b5d5-e80688cb869a"))

(define *attach-vignettes*
 '("Attach1_A2_C1_Act5_URBAN7_MC_MORN_48a5e0ee-c5af-11df-b4db-e80688cb869a"
   "Attach2_A1_C1_Act1_DOWNTOWN1A3_MR_MIDD_479c66a8-c5af-11df-af5e-e80688cb869a"
   "Attach2_A1_C1_Act1_PARK1_MC_AFTN_47cb104a-c5af-11df-ac21-e80688cb869a"
   "Attach2_A1_C1_Act1_PARK1_ML_MIDD_44257b2e-c5af-11df-8353-e80688cb869a"
   "Attach2_A1_C1_Act1_PARK2_MR_MIDD_4426cf73-c5af-11df-87f0-e80688cb869a"))

(define (data vignette-sets bits state)
 (reduce append
	 (map-indexed (lambda (vignettes i)
		       (map (lambda (vignette)
			     (list (read-time-series vignette bits)
				   (append (on i (length vignette-sets))
					   (map-n (lambda (i) 1) state))))
			    vignettes))
		      vignette-sets)
	 '()))

(define (ws0 bits classes hidden state)
 (list (map-n (lambda (i)
	       (map-n (lambda (i) (random-weight))
		      (+ (+ (* 8 bits) state classes) 1)))
	      hidden)
       (map-n (lambda (i) (map-n (lambda (i) (random-weight)) (+ hidden 1)))
	      (+ classes state))))

(define (vanilla-weights f weights0 n eta)
 (when (modulo n 10)
  (format #t "~a ~a~%" n (f weights0)))
 (if (zero? n)
     weights0
     (vanilla-weights
      f (s-k* weights0 eta ((weight-gradient f) weights0)) (- n 1) eta)))

(define (test1 n bits hidden state eta)
 (let* ((vignette-sets
	 (list *jump-vignettes*
	       ;; *bounce-vignettes*
	       ;; *arrive-vignettes*
	       *attach-vignettes*))
	(samples (data vignette-sets bits state))
	(weights (vanilla-weights (error-on-dataset samples)
				  (ws0 bits (length vignette-sets) hidden state)
				  n
				  eta)))
  (write ((error-on-dataset samples) weights)) (newline)
  (map (lambda (sample)
	(let ((l (sublist ((recurrent-neuron-network weights) (first sample))
			  0
			  (length vignette-sets))))
	 (positionv (reduce max l minus-infinity) l)))
       samples)))

(define-command
 (main (any-number ("classes" classes? (classes "name" string-argument)))
       (at-most-one ("new-opt" new-opt?))
       (exactly-one ("iterations" iterations? (iterations "nr" integer-argument 100)))
       (exactly-one ("hidden" hidden? (hidden "nr" integer-argument 10)))
       (exactly-one ("state" state? (state "nr" integer-argument 10)))
       (exactly-one ("bits" bits? (bits "nr" integer-argument 10))))
 (let* ((classes (reverse classes)) ; QobiScheme reverses the order of the input files
	(vignette-sets (map read-file (reverse classes)))
	(samples (data vignette-sets bits state))
	(f (error-on-dataset samples))
	(initial (ws0 bits (length vignette-sets) hidden state))
	(weights
	 (if new-opt?
	     (multivariate-argmin-F-args f initial 0.3 iterations 1e-5)
	     (vanilla-weights f initial iterations 0.3))))
  (display "----------------------------")(newline)
  (pp weights)(newline)
  (display "----------------------------")(newline)
  (write ((error-on-dataset samples) weights))
  (newline)
  (pp (map (lambda (sample)
	    (let ((l (sublist ((recurrent-neuron-network weights) (first sample))
			      0
			      (length vignette-sets))))
	     (positionv (reduce max l minus-infinity) l)))
	   samples))
  (newline)
  (let ((results
	 (map (lambda (sample video)
	       (let* ((l (sublist ((recurrent-neuron-network weights) (first sample))
				  0
				  (length vignette-sets)))
		      (idx (positionv (reduce max l minus-infinity) l)))
		(list (car (pregexp-split "[0-9]" video))
		      (car (pregexp-split "[0-9]" (car (list-ref vignette-sets idx)))))))
	      samples (join vignette-sets))))
   (pp results)(newline)
   (pp (list (length results)
	     (length (removeq #f (map (lambda (a) (equal? (first a) (second a))) results)))
	     (/ (length (removeq #f (map (lambda (a) (equal? (first a) (second a))) results)))
		(length results))))
   (newline))))
