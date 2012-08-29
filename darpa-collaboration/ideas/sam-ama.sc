(MODULE
 SAM-SLIC
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

(set! *program* "sam-ama")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;;  darpa-wrap ./sam-ama  -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a

(define-structure feature has-value? value name)

(define (parse-features video frame)
 (define (parse port)
  (let ((value (read port)))
   (cond ((eof-object? value) '())
	 (else (cons
		(make-feature (not (and (symbol? value)
					(eq? value '*)))
			      value (read port))
		(parse port))))))
 (call-with-input-file (generic-pathname video frame "features-1.txt") parse))

(define (has-feature? feature-list name)
 (> (length (remove-if (lambda (f) (not (eq? (feature-name f) name))) feature-list)) 0))

(define (get-feature feature-list name)
 (first (remove-if (lambda (f) (not (eq? (feature-name f) name))) feature-list)))

(define (has-feature-value? feature-list name)
 (if (has-feature? feature-list name)
     (feature-has-value? (get-feature feature-list name))
     #f))

(define (get-feature-value feature-list name)
 (feature-value (get-feature feature-list name)))

;; f is a function which will be called with the feature-list as an argument
;; and should return true if the proposition is true in this frame
;; features is a list of the features that this proposition depends on.
(define-structure proposition name f features)

(define *propositions*
 (list
  (make-proposition
   'agent-is-accelerating
   (lambda (feature-list)
    (> (get-feature-value feature-list 'agent-acceleration-magnitude) .2))
   '(agent-acceleration-magnitude))
  (make-proposition
   'agent-is-stationary
   (lambda (feature-list)
    (< (abs (get-feature-value feature-list 'agent-velocity-magnitude)) 1))
   '(agent-velocity-magnitude))
  ))

(define (check-proposition feature-list prop)
 (and (map-reduce (lambda (a b) (or a b)) #f
		  (lambda (f) (has-feature-value? feature-list f))
		  (proposition-features prop))
      ((proposition-f prop) feature-list)))

(define (get-and-formula video frame)
 (let ((feature-list (parse-features video frame)))
  (define (get-props prop-list)
   (cond ((null? prop-list) '())
	 ((check-proposition feature-list (first prop-list))
	  (cons (proposition-name (first prop-list)) (get-props (rest prop-list))))
	 (else (get-props (rest prop-list)))))
  (get-props *propositions*)))
   

(define (sam-slic-main video)
 (display video)
 (newline))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))))
 (let ((video-name
	(cond (standard?
	       (standard-corpus-video corpus sequence person location n))
	      (darpa? (string->darpa-video name))
	      (else (fuck-up)))))
  (sam-slic-main video-name)))

