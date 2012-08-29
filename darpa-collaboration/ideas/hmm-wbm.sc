(MODULE HMM-WBM)
;;; LaHaShem HaAretz U'Mloah

(include "QobiScheme.sch")
(include "hmm-wbm.sch")
(include "toollib-c-macros.sch")

;; 0<=u,v<uu states
;; 0<=t<tt time
;; 0<=i<ii feature
;; 0<=l<ll training example
;; 0<=k<kk discrete feature class
;; 0<=n parameter index
;; 0<=c<cc class

;; m[c] is model for class c
;;  sometimes has implicit c
;; data[l][i][t]
;; x[l][t] is data for feature i
;; weight[l][t] is called gamma elsewhere
;; c_ls[l] is the correct class for training sample l
;;  sometimes has implicit l
;; postpC[c][l] is P(c|x[l], lambda'), i.e., the posterior probability of
;;  class c given logL(x[l])
;;  when postpC==NULL use ML, i.e. train only on sample for each class
;;  sometimes has implicit c and even implicit l
;; prior[c] is the prior probability of class c

;;; Structures

(define-structure psi name features kk parameters a b)

;;; Parameters

;;; Procedures

(define-c-external (allocate-rmat-internal int int) pointer "allocRMat")

(define-c-external (rmat-get-internal pointer int int) double "rmat_get")

(define-c-external (rmat-set!-internal pointer int int double) void "rmat_set")

(define-c-external (free-rmat-internal pointer) void "freeRMat")

(define-c-external (allocate-rmat-vector-internal int) pointer
 "allocate_rmat_vector")

(define-c-external (rmat-vector-set!-internal pointer int pointer) void
 "rmat_vector_set")

(define-c-external (free-rmat-vector-internal pointer) void "free_rmat_vector")

(define-c-external (allocate-model-internal int int) pointer "allocModel")

(define-c-external (copy-model!-internal pointer pointer) void "copyModel")

(define-c-external (model-ii-internal pointer) int "model_ii")

(define-c-external (model-uu-internal pointer) int "model_uu")

(define-c-external (model-feature-type-internal pointer int) int
 "model_feature_type")

(define-c-external
 (set-model-feature-type-continuous!-internal pointer int double)
 void "defineContFeat")

(define-c-external (set-model-feature-type-radial!-internal pointer int) void
 "defineRadialFeat")

(define-c-external (set-model-feature-type-discrete!-internal pointer int) void
 "defineDiscreteFeat")

(define-c-external (model-parameter-internal pointer int int int) double
 "model_parameter")

(define-c-external (set-model-parameter!-internal pointer int int int double)
 void "set_model_parameter")

(define-c-external (model-a-internal pointer int int) double "model_a")

(define-c-external (set-model-a!-internal pointer int int double) void
 "set_model_a")

(define-c-external (model-b-internal pointer int) double "model_b")

(define-c-external (set-model-b!-internal pointer int double) void
 "set_model_b")

(define-c-external (randomise-model!-internal pointer int int) void
 "randomiseModel")

(define-c-external (print-model-internal pointer) void "print_model")

(define-c-external (free-model-internal pointer) void "freeModel")

(define-c-external (model-log-likelihood-internal pointer pointer) double
 "logLike")

(define-c-external (best-state-sequence-internal pointer pointer) pointer
 "best_state_sequence")

(define-c-external (force-init-globals!-internal) void "force_init_globals")

(define (allocate-rmat ii tt) (allocate-rmat-internal ii tt))

(define (rmat-get rmat ii tt) (rmat-get-internal rmat ii tt))

(define (rmat-set! rmat ii tt value) (rmat-set!-internal rmat ii tt value) #f)

(define (free-rmat rmat) (free-rmat-internal rmat) #f)

(define (allocate-rmat-vector nn) (allocate-rmat-vector-internal nn))

(define (rmat-vector-set! rmat-vector n rmat)
 (rmat-vector-set!-internal rmat-vector n rmat)
 #f)

(define (free-rmat-vector rmat-vector)
 (free-rmat-vector-internal rmat-vector)
 #f)

(define (allocate-model ii uu) (allocate-model-internal ii uu))

(define (copy-model! dst-model src-model)
 (copy-model!-internal dst-model src-model)
 #f)

(define (model-ii model) (model-ii-internal model))

(define (model-uu model) (model-uu-internal model))

(define (model-feature-type model i) (model-feature-type-internal model i))

(define (set-model-feature-type-continuous! model i value)
 (set-model-feature-type-continuous!-internal model i value)
 #f)

(define (set-model-feature-type-radial! model i)
 (set-model-feature-type-radial!-internal model i)
 #f)

(define (set-model-feature-type-discrete! model i)
 (set-model-feature-type-discrete!-internal model i)
 #f)

(define (model-parameter model i u f) (model-parameter-internal model i u f))

(define (set-model-parameter! model i u f value)
 (set-model-parameter!-internal model i u f value)
 #f)

(define (model-a model u1 u2) (model-a-internal model u1 u2))

(define (set-model-a! model u1 u2 value)
 (set-model-a!-internal model u1 u2 value)
 #f)

(define (model-b model u) (model-b-internal model u))

(define (set-model-b! model u value) (set-model-b!-internal model u value) #f)

(define (randomise-model! model upper-triangular? random-a-and-b?)
 (randomise-model!-internal
  model (if upper-triangular? 1 0) (if random-a-and-b? 1 0))
 #f)

(define (print-model model) (print-model-internal model) #f)

(define (free-model model) (free-model-internal model) #f)

(define (model-log-likelihood model rmat)
 (model-log-likelihood-internal model rmat))

(define (best-state-sequence model rmat)
 (best-state-sequence-internal model rmat))

(define (force-init-globals!) (force-init-globals!-internal) #f)

(define-c-external (update!-internal pointer pointer pointer pointer double
				     pointer int int int int)
 int "update")

(define-c-external (allocate-hmm!-internal int) pointer "allocHMM")
(define-c-external (free-hmm!-internal pointer) void "freeHMM")

(define (allocate-hmm i) (allocate-hmm!-internal i))
(define (free-hmm p) (free-hmm!-internal p)
 #f)

(define (update-model-ml! model scratch-hmm data data-size upper-triangular?)
 (with-c-pointers
  (lambda (model-array)
   (with-c-pointers
    (lambda (scratch-model-array)
     (with-array data-size
		 c-sizeof-int
		 (lambda (class-array)
		  (update!-internal
		   model-array
		   scratch-model-array
		   data
		   0
		   0
		   (vector->c-exact-array class-array (make-vector data-size 0)
					  c-sizeof-int #f)
		   data-size
		   1
		   *hmm-maximum-likelihood-training*
		   (if upper-triangular? 1 0)))))
    (vector scratch-hmm)))
  (vector model)))

(define (update-model-ml-multi! models scratch-hmms data data-size class-assignments upper-triangular?)
 (with-c-pointers
  (lambda (model-array)
   (with-c-pointers
    (lambda (scratch-model-array)
     (with-array data-size
		 c-sizeof-int
		 (lambda (class-array)
		  (update!-internal
		   model-array
		   scratch-model-array
		   data
		   0
		   0
		   (vector->c-exact-array class-array class-assignments c-sizeof-int #f)
		   data-size
		   (vector-length models)
		   *hmm-maximum-likelihood-training*
		   (if upper-triangular? 1 0)))))
    scratch-hmms))
  models))


(define-c-external (compute-posterior!-internal
		    pointer pointer pointer pointer int
		    int int pointer pointer pointer)
 pointer "compute_posterior")

(define (compute-posterior! models data priors class-assignments 
			    data-size training-mode posterior-array)
 (with-c-pointers
  (lambda (model-array)
   (with-array 
    data-size c-sizeof-int
    (lambda (class-array)
     (with-array 
      data-size c-sizeof-double
      (lambda (priors-array)
       (with-alloc
	c-sizeof-double
	(lambda (objective-function-value)
	 (with-alloc
	  c-sizeof-double
	  (lambda (auxiliary)
	   (compute-posterior!-internal
	    model-array 
	    data
	    (vector->c-inexact-array priors-array priors c-sizeof-double #f)
	    (vector->c-exact-array class-array class-assignments c-sizeof-int #f)
	    data-size
	    (vector-length models)
	    training-mode
	    objective-function-value
	    auxiliary
	    posterior-array)
	   (vector (c-double-ref objective-function-value 0)
		   (c-double-ref auxiliary 0)))))))))))
  models))

(define (update-model-dt! models scratch-hmms data posterior log-D
			  class-assignments data-size upper-triangular?)
 (unless (= (vector-length models) (vector-length scratch-hmms))
  (fuck-up))
 (with-c-pointers
  (lambda (model-array)
   (with-c-pointers
    (lambda (scratch-model-array)
     (with-array data-size
		 c-sizeof-int
		 (lambda (class-array)
		  (update!-internal
		   model-array
		   scratch-model-array
		   data
		   posterior
		   log-D
		   (vector->c-exact-array class-array class-assignments c-sizeof-int #f)
		   data-size
		   (vector-length models)
		   *hmm-discriminative-training*
		   (if upper-triangular? 1 0)))))
    scratch-hmms))
  models))


(define-c-external (state-probabilities-internal pointer
						 pointer)
 pointer "state_probabilities")

(define-c-external (state-probabilities-with-box-scores-internal pointer
								 pointer
								 pointer)
 pointer "state_probabilities_with_box_scores")

(define (debug-state-probabilities model features number-of-states)
  (let* ((rmat (features->rmat features))
	(array (state-probabilities-internal model rmat)))
	array))

(define (state-probabilities model features number-of-states)
 ;; features is a list of feature vectors as returned by compute-fv
 ;; model is an hmm model as returned by (trained-hmm-model hmm)
 ;; number of states is the number of states in the hmm
 ;; returns a list of lists
 ;; each inner list corresponds to a frame of video
 ;; each element in each inner list corresponds to a state
 ;; and is the probability that that frame in in that state
 (let* ((rmat (features->rmat features))
	(array (state-probabilities-internal model rmat))
	(pointers (c-exact-array->list array
					   c-sizeof-long
					   (length features)
					   #f))
	(probabilities (map (lambda (p)
			     (map exp (c-inexact-array->list p
						    c-sizeof-double
						    number-of-states
						    #t)))
			    pointers)))
  (map (lambda (p)
	(free p))
       pointers)
  (free array)
  probabilities))

(define (state-probabilities-with-box-scores model
					     features
					     number-of-states
					     scores-rmat)
 ;; features is a list of feature vectors as returned by compute-fv
 ;; model is an hmm model as returned by (trained-hmm-model hmm)
 ;; number of states is the number of states in the hmm
 ;; returns a list of lists
 ;; each inner list corresponds to a frame of video
 ;; each element in each inner list corresponds to a state
 ;; and is the probability that that frame in in that state
 (let* ((rmat (features->rmat features))
	(array (state-probabilities-with-box-scores-internal model
							     rmat
							     scores-rmat))
	(pointers (c-exact-array->list array
					   c-sizeof-long
					   (length features)
					   #f))
	(probabilities (map (lambda (p)
			     (map exp (c-inexact-array->list p
						    c-sizeof-double
						    number-of-states
						    #t)))
			    pointers)))
  (map (lambda (p)
	(free p))
       pointers)
  (free array)
  probabilities))

(define-c-external (update!-with-box-scores-internal pointer pointer pointer pointer double
				     pointer int int int int pointer)
 int "update_with_box_scores")

(define (update-model-ml-with-box-scores! model scratch-hmm data data-size upper-triangular? scores-rmats)
 (with-c-pointers
  (lambda (model-array)
   (with-c-pointers
    (lambda (scratch-model-array)
     (with-array data-size
		 c-sizeof-int
		 (lambda (class-array)
		  (update!-with-box-scores-internal
		   model-array
		   scratch-model-array
		   data
		   0
		   0
		   (vector->c-exact-array class-array (make-vector data-size 0)
					  c-sizeof-int #f)
		   data-size
		   1
		   *hmm-maximum-likelihood-training*
		   (if upper-triangular? 1 0)
		   scores-rmats))))
    (vector scratch-hmm)))
  (vector model)))

(define (model-log-likelihood-with-box-scores model rmat score-rmat)
 (model-log-likelihood-with-box-scores-internal model rmat score-rmat))

(define-c-external (model-log-likelihood-with-box-scores-internal pointer pointer pointer) double
 "logLike_with_box_scores")

;;; Tam V'Nishlam Shevah L'El Borei Olam
