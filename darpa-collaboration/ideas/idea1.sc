(MODULE IDEA1 (WITH QOBISCHEME XLIB IDEALIB-STUFF) (MAIN MAIN))
;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme-AD.sch")
(include "idea1.sch")

(set! *program* "idea1")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

(define *view?* #f)

(define *interpreted?* #t)

;;; Parameters

;;; C Externals

;;; Procedures

;;; Tests

(define (id) (if *interpreted?* 0 (getpid)))

(define (view-pbm pbm)
 (write-pnm (pbm->pgm pbm) (format #f "/tmp/foo-~s.pgm" (id)))
 (when *view?*
  (system (format #f "ssh tlamachilistli ee /net/jalitusteabe/tmp/foo-~s.pgm"
		  (id)))
  (rm (format #f "/tmp/foo-~s.pgm" (id)))))

(define (view-pbms pbms)
 (for-each-indexed
  (lambda (pbm i)
   (write-pnm (pbm->pgm pbm) (format #f "/tmp/foo-~s-~a.pgm"
				     (id)
				     (number->padded-string-of-length i 3))))
  pbms)
 (when *view?*
  (system (format #f "ssh tlamachilistli ee /net/jalitusteabe/tmp/foo-~s-*.pgm"
		  (id)))
  (rm (format #f "/tmp/foo-~s-*.pgm" (id)))))

(define (test1 model u)
 (view-pbm (points->pbm (map-reduce append
				    '()
				    (lambda (l) (line-segment->points l))
				    (model u))
			120 160)))

(define (test2 model u)
 (view-pbm
  (points->pbm (map-reduce
		append
		'()
		(lambda (l) (ellipse->points (line-segment->ellipse l)))
		(model u))
	       120 160)))

(define (test3 model u)
 (view-pbm (points->pbm (map-reduce
			 append
			 '()
			 (lambda (l)
			  (append (line-segment->points l)
				  (ellipse->points (line-segment->ellipse l))))
			 (model u))
			120 160)))

(define (test4 frame)
 (view-pbm (moving-edges-frame "a" "qobi" "mohio" 0 frame)))

(define (test5)
 (let* ((pbm (moving-edges-frame "a" "qobi" "mohio" 0 3))
	(fit (points->pbm (fit-and-render-frame pbm
						one-lsm-model
						'#(0 0 0 150)
						'#(#(1 1 0 0)
						   #(0 0 1 0)
						   #(0 0 0 1)))
			  (pnm-height pbm)
			  (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

(define (test6)
 (let* ((pbm (moving-edges-frame "a" "foob" "chino" 0 30))
	(fit (points->pbm (fit-and-render-frame pbm
						one-lsm-model
						'#(0 0 0 150)
						'#(#(1 1 0 0)
						   #(0 0 1 0)
						   #(0 0 0 1)))
			  (pnm-height pbm)
			  (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

(define (test7)
 (let* ((pbm (moving-edges-frame "a" "foob" "chino" 0 30))
	(fit (points->pbm (fit-and-render-frame pbm
						two-uncoupled-lsm-model
						'#(0 0 0 150 0 0 0 100)
						'#(#(1 1 0 0 0 0 0 0)
						   #(0 0 1 0 0 0 0 0)
						   #(0 0 0 1 0 0 0 0)
						   #(0 0 0 0 1 1 0 0)
						   #(0 0 0 0 0 0 1 0)
						   #(0 0 0 0 0 0 0 1)))
			  (pnm-height pbm)
			  (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

(define (test8)
 (let* ((pbm (moving-edges-frame "a" "foob" "chino" 0 30))
	(fit (points->pbm (fit-and-render-frame pbm
						two-lsm-model
						'#(0 0 0 150 0 0 100)
						'#(#(1 1 0 0 0 0 0)
						   #(0 0 1 0 0 0 0)
						   #(0 0 0 1 0 0 0)
						   #(0 0 0 0 1 0 0)
						   #(0 0 0 0 0 1 0)
						   #(0 0 0 0 0 0 1)))
			  (pnm-height pbm)
			  (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

(define (test9)
 (let* ((pbm (moving-edges-frame "a" "foob" "chino" 0 29))
	(fit (points->pbm (fit-and-render-frame pbm
						two-lsm-model
						'#(0 0 0 150 0 0 100)
						'#(#(1 1 0 0 0 0 0)
						   #(0 0 1 0 0 0 0)
						   #(0 0 0 1 0 0 0)
						   #(0 0 0 0 1 0 0)
						   #(0 0 0 0 0 1 0)
						   #(0 0 0 0 0 0 1)))
			  (pnm-height pbm)
			  (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

(define (test10)
 (let* ((pbms (moving-edges-frames "a" "foob" "chino" 0 5 8))
	(fit (map (lambda (points)
		   (points->pbm points
				(pnm-height (first pbms))
				(pnm-width (first pbms))))
		  (fit-and-render-frames pbms
					 (linear-temporal-model
					  one-lsm-model
					  '#((0 1) (2 3) 4 5))
					 '#(1 1 1 1 0 150)
					 '#(#(1 1 1 1 1 1))))))
  (view-pbms (map pbm-or pbms fit))))

(define (test11)
 (let* ((pbms (moving-edges-frames "a" "foob" "chino" 0 27 30))
	(fit (map (lambda (points)
		   (points->pbm points
				(pnm-height (first pbms))
				(pnm-width (first pbms))))
		  (fit-and-render-frames pbms
					 (linear-temporal-model
					  two-lsm-model
					  '#(0 1 2 3 4 5 (6 7)))
					 '#(0 0 0 150 0 0 1 1)
					 '#(#(1 1 1 1 1 1 1 1))))))
  (view-pbms (map pbm-or pbms fit))))

(define (test12)
 (let* ((pbm (moving-edges-frame "a" "foob" "chino" 0 30))
	(u1 (fit-frame pbm
		       one-lsm-model
		       '#(0 0 0 150)
		       '#(#(1 1 0 0)
			  #(0 0 1 0)
			  #(0 0 0 1))))
	(fit (points->pbm
	      (fit-and-render-frame pbm
				    (lambda (u)
				     (two-lsm-model (vector-append u1 u)))
				    '#(0 0 100)
				    '#(#(1 0 0)
				       #(0 1 0)
				       #(0 0 1)))
	      (pnm-height pbm)
	      (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

(define (test13)
 (let* ((pbm
	 (pgm->pbm (read-pnm "/tmp/a-foob-chino-0/0085/foo-subsample.pgm") 128))
	(fit (points->pbm
	      (fit-and-render-frame
	       pbm
	       (lambda (u)
		(two-lsm-model
		 (vector
		  ;; torso midpoint
		  (vector-ref u 0) (vector-ref u 1)
		  ;; torso angle
		  270.0
		  ;; torso length
		  (vector-ref u 2)
		  ;; shoulder joint percentage
		  (vector-ref u 3)
		  ;; shoulder joint angle
		  (vector-ref u 4)
		  ;; arm length
		  (vector-ref u 5))))
	       '#(0 0 150 50 0 100)
	       '#(#(1 1 1 1 1 1)))
	      (pnm-height pbm)
	      (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

(define (test14)
 (let* ((pbm
	 (pgm->pbm (read-pnm "/tmp/a-foob-chino-0/0085/foo-subsample.pgm") 128))
	(fit (points->pbm
	      (fit-and-render-frame
	       pbm
	       (lambda (u)
		(two-lsm-model
		 (vector
		  ;; torso midpoint
		  (vector-ref u 0) (vector-ref u 1)
		  ;; torso angle
		  270.0
		  ;; torso length
		  (vector-ref u 2)
		  ;; shoulder joint percentage
		  (vector-ref u 3)
		  ;; shoulder joint angle
		  (vector-ref u 4)
		  ;; arm length
		  (vector-ref u 5))))
	       '#(90 50 100 60 210 75)
	       '#(#(1 1 1 1 1 1)))
	      (pnm-height pbm)
	      (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

(define (test15)
 (let* ((pbm
	 (pgm->pbm (read-pnm "/tmp/a-foob-chino-0/0085/foo-subsample.pgm") 128))
	(fit (points->pbm
	      (fit-and-render-frame
	       pbm
	       (lambda (u)
		(two-lsm-model
		 (vector
		  ;; torso midpoint
		  (vector-ref u 0) (vector-ref u 1)
		  ;; torso angle
		  (vector-ref u 2)
		  ;; torso length
		  (vector-ref u 3)
		  ;; shoulder joint percentage
		  (vector-ref u 4)
		  ;; shoulder joint angle
		  (vector-ref u 5)
		  ;; arm length
		  (vector-ref u 6))))
	       '#(90 50 270 100 60 210 75)
	       '#(#(1 1 1 1 1 1 1)))
	      (pnm-height pbm)
	      (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

(define (test16)
 (let* ((pbm (subsample-pbm
	      (pgm->pbm
	       (read-pnm
		"/aux/qobi/video-datasets/office/a-foob-chino-0/0085/foo.pgm")
	       128)))
	(fit (points->pbm
	      (fit-and-render-frame-with-restarts
	       pbm
	       (lambda (u)
		(two-lsm-model
		 (vector
		  ;; torso midpoint
		  (vector-ref u 0) (vector-ref u 1)
		  ;; torso angle
		  (vector-ref u 2)
		  ;; torso length
		  (vector-ref u 3)
		  ;; shoulder joint percentage
		  (vector-ref u 4)
		  ;; shoulder joint angle
		  (vector-ref u 5)
		  ;; arm length
		  (vector-ref u 6))))
	       '#((1 160 2)
		  (1 120 2)
		  (1 180 2)
		  (1 160 2)
		  (1 100 2)
		  (1 180 2)
		  (1 160 2))
	       '#(#(1 1 1 1 1 1 1)))
	      (pnm-height pbm)
	      (pnm-width pbm))))
  (view-pbm (pbm-or pbm fit))))

;;; Commands

;;; Top Level

(define-command (main (at-most-one ("view" view?))
		      (required (test "test" string-argument)))
 (set! *interpreted?* #f)
 (set! *view?* view?)
 (cond ((string=? test "test1")
	(test3 (lambda (u)
		(two-lsm-model
		 (vector
		  ;; torso midpoint
		  (vector-ref u 0) (vector-ref u 1)
		  ;; torso angle
		  270.0
		  ;; torso length
		  (vector-ref u 2)
		  ;; shoulder joint percentage
		  60.0
		  ;; shoulder joint angle
		  210.0
		  ;; arm length
		  (vector-ref u 3))))
	       '#(90 50 100 75)))
       ((string=? test "test2") (test2 one-lsm-model '#(0 0 0 150)))
       ((string=? test "test3") (test3 one-lsm-model '#(0 0 0 150)))
       ((string=? test "test4") (test4 0))
       ((string=? test "test5") (test5))
       ((string=? test "test6") (test6))
       ((string=? test "test7") (test7))
       ((string=? test "test8") (test8))
       ((string=? test "test9") (test9))
       ((string=? test "test10") (test10))
       ((string=? test "test11") (test11))
       ((string=? test "test12") (test12))
       ((string=? test "test13") (test13))
       ((string=? test "test14") (test14))
       ((string=? test "test15") (test15))
       ((string=? test "test16") (test16))
       (else (panic "Unknown test"))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
