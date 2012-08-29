(MODULE
  SCHEMEREADER
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
(include "QobiScheme.sch")
(include "schemereader.sch")

(set! *program* "schemereader")
(set! *panic?* #f)

(define (get-simple-area x1 y1 x2 y2)
   (* (- x2 x1) (- y2 y1)))
(define (close-to-equal? a b tol)
 (if (equal? (length a) (length b))
     (<= 
      (map-reduce
       +
       0
       (lambda (a b)
	(distance a b))
       a b)
      (* tol (length a)))
     #f))
; (<= (distance (first a) (first  b)) tol))



(define (hausdorf-distance d)
 (lambda (p q)
  (map-reduce
   max
   minus-infinity
   (lambda (p) (map-reduce min infinity (lambda (q) (d p q)) q)) p)))

(define (chamfer-distance d)
 (lambda (p q)
  (map-reduce
   + 0 (lambda (p) (map-reduce min infinity (lambda (q) (d p q)) q)) p)))

(define (complete-linkage-distance d)
 (lambda (p q)
  (map-reduce
   max
   minus-infinity
   (lambda (p) (map-reduce max minus-infinity (lambda (q) (d p q)) q)) p)))

(define (single-linkage-distance d)
 (lambda (p q)
  (map-reduce
   min
   infinity
   (lambda (p) (map-reduce min infinity (lambda (q) (d p q)) q)) p)))

(define (average-linkage-distance d)
 (lambda (p q)
  (/ (map-reduce + 0 (lambda (p) (map-reduce + 0 (lambda (q) (d p q)) q)) p)
     (* (length p) (length q)))))

(define (minimum-average-distance d)
 (lambda (p q)
  (map-reduce
   min infinity (lambda (p) (map-reduce + 0 (lambda (q) (d p q)) q)) p)))

(define (point-with-minimum-average-distance d)
 (lambda (p q)
  (let loop ((p (rest p))
	     (b (first p))
	     (c (map-reduce + 0 (lambda (q) (d (first p) q)) q)))
   (if (null? p)
       b
       (let ((c1 (map-reduce + 0 (lambda (q) (d (first p) q)) q)))
	(if (< c1 c)
	    (loop (rest p) (first p) c1)
	    (loop (rest p) b c)))))))

(define (invert-distance d) (lambda (p q) (d q p)))

(define (bidirectional d s)
 (lambda (p q) (s (d p q) ((invert-distance d) p q))))

(define (medoid d)
 (lambda (ps) ((point-with-minimum-average-distance d) ps ps)))

(define (k-medoids medoid d p n)
 (let* ((ps (random-partition-of-size n p)) (medoids (map medoid ps)))
  (let loop ((ps ps)
	     (medoids medoids)
	     (cost
	      (map-reduce
	       +
	       0
	       (lambda (p medoid) (map-reduce + 0 (lambda (p) (d p medoid)) p))
	       ps
	       medoids)))
   
   (let* ((pis (map (lambda (p)
		     (let ((ds (map (lambda (medoid) (d p medoid)) medoids)))
		      (list p (positionv (reduce min ds infinity) ds))))
		    p))
	  (ps1 (removeq
		'()
		(map-n
		 (lambda (i)
		  (map first
		       (remove-if-not (lambda (pi) (= (second pi) i)) pis)))
		 (length medoids))))
	  (medoids1 (map medoid ps1))
	  (cost1
	   (map-reduce
	    +
	    0
	    (lambda (p medoid) (map-reduce + 0 (lambda (p) (d p medoid)) p))
	    ps1
	    medoids1)))
    (if (< cost1 cost)
	(begin
	 (dtrace "loop" "LOOP")
	 (loop ps1 medoids1 cost1))
	(map
	 (lambda (p medoid) (append (list medoid) (list ps))) ps medoids))))))

;; minimum of bidirection of min average distance


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-command
 (main (at-most-one ("named" named? (zname "zname" string-argument "/home/zburchil/poses/person"))
		    ("model-name" model-name? (model-name "model-name" string-argument ""))
		    ("combine" combine? (name "name" string-argument ""))
		    ("k" k? (k "k" integer-argument 10)))) 
 (let ((frames (map-indexed
		(lambda (frame i)
		 (append frame (list (+ 4094 i))))
		(read-object-from-file "/home/zburchil/persondownthing.text")))) ; Hardwired to my directory

  (cond
   ;; Clustering
   
   (k?
    (let ((video-list (read-object-from-file "/home/zburchil/fullperson-normalized-parts")))
     (write-object-to-file 
;;     (map
;;      (lambda (cluster)
       ;(dtrace "cluster" (first cluster))
       ;(dtrace "cruster" (second cluster))
       ;(newline)
       ;(if (> (determinant (list-covariance cluster)) 3.0e-37)
	   ;(map
	  ;  (lambda (cluster)
	  ;   (newline)
	     ;;(dtrace "D" (determinant (list-covariance cluster)))
	     ;(dtrace "L" (length cluster))
	     ;;(dtrace "OO" cluster)
		     ;(map
		      ;(lambda (thing)
		       ;(second (assoc thing video-list))) cluster))
	   ;  (newline))
	  ;  (k-medoids (medoid distance)
	;	      distance
	;	      cluster 3))
;;     (begin
	;(newline)
					;(dtrace "D" (determinant (list-covariance cluster)))
					;(dtrace "L" (length cluster))
;;	(append cluster '())
;;					(dtrace "0" (map
;;					(lambda (thing)
;;					(second (assoc thing video-list))) cluster))
;;	))
      (k-medoids (medoid distance)
		 distance
		 (map first video-list) k)
;;      )
     "/home/zburchil/medoid-clusters")
     ))
   
   ;; Writing the vectors out
   (named?
    (for-each
     (lambda (frame)
      (dtrace "a" frame))
     frames)
    (write-object-to-file 
     (map
      (lambda (frame)
       (let ((zvideo-name (string->darpa-video (first frame))))
	;(dtrace "fafda" frame)
	(if (file-exists?
  	     (boxes-pathname zvideo-name "voc4"
  			     (if model-name? model-name "person-down")))
	    (append 
	     ;; Gets the Voc4 box that most overlaps the annotated box
	     ;; Turns it into a list
	     (maximump
	      (map
	       (lambda (box)
		(append (vector->list box) 
			(list (let ((intersect (list
						(max (y box) (fourth frame))
						(max (z box) (third frame))
						(min (vector-ref box 3) (sixth frame))
						(min (vector-ref box 4) (fifth frame)))))
			       (if (and (< (first intersect) (third intersect)) (< (second intersect) (fourth intersect)))
				   (/ (get-simple-area (first intersect) (second intersect) (third intersect) (fourth intersect))
				      (- (+ (get-simple-area (y box) (z box) (vector-ref box 3) (vector-ref box 4))
					    (get-simple-area (fourth frame) (third frame) (sixth frame) (fifth frame)))
					 (get-simple-area (first intersect) (second intersect) (third intersect) (fourth intersect))))
				   minus-infinity)))))
	       (list-ref (read-voc4-detector-boxes
			  zvideo-name
			  (if model-name? model-name "person-down")) (- (second frame) 1)))
	      last)
	     (list (seventh frame)))
	    #f)))
      frames)
     zname))
    
   ;; Turning the vectors to normalized stuff
   (combine?
    (let* ((path-and-name name)
  	   (file1 (read-object-from-file (string-append path-and-name "1")))
  	   (file2 (read-object-from-file (string-append path-and-name "2")))
  	   (file3 (read-object-from-file (string-append path-and-name "3")))
  	   (file4 (read-object-from-file (string-append path-and-name "4")))
  	   (empty-list (map-n (lambda (_) '()) (length file1)))
  	   (finished-list (map-indexed
  			   (lambda (e i)
  			     (map-reduce
  			      append
  			      '()
  			      (lambda (file)
  			       (if (list-ref file i)
  				   (list-ref file i)
  				   '()))
  			      (list file1 file2 file3 file4)))
  			   empty-list)))

     
     (write-object-to-file finished-list (string-append path-and-name "-boxes-and-scores"))
     (dtrace "aaa" (first file1))
     (write-object-to-file (map
  			    (lambda (box)
  			     (let ((center (voc4-detection-center (first box))))
			      (append
			       (list (list->vector (join (map
							  (lambda (part)
							   (vector->list
							    (normalize-line-in-voc4-box
							      (make-line-segment (part-center part) center) (first box))))
							  (voc4-detection-parts (first box))))))
			       (list (second box)))))
  			    (remove 'remove 
  				   (map
  				     (lambda (box)
  				      (if (null? box) 'remove (append
							       (list (list->vector (take (- (length box) 2) box)))
							       (list (list-ref box (- (length box) 1))))))
  				     finished-list)))
  			    (string-append path-and-name "-normalized-parts")))))
  (format #t "done~%")))
