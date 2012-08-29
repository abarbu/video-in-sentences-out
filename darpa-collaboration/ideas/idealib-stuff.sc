(MODULE IDEALIB-STUFF)

;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

;;;  1. maybe use smooth min
;;;  2. maybe use distance squared

(include "QobiScheme-AD.sch")
(include "idealib-stuff.sch")
(include "toollib-c-macros.sch")

(c-include "Imlib2.h")
(c-include "idealib-c.h")
(c-include "cuklt.h")

(c-include "stdio.h")
(c-include "hmm.h")
(c-include "zip.h")

;;; Macros

;;; Structures

(define-structure vertex pixels vertex edges)

(define-structure edge u v)

(define-structure graph vertices edges)

(define-structure rc-chains width height vertices solid-edges dashed-edges)

(define-structure rc-vertex pixels vertex solid-edge)

(define-structure solid-edge u v w1 w2 pixels)

(define-structure dashed-edge u v w1 w2)

(define-structure spline coefficients points error)

(define-structure annotation pixels label)
(define-structure essa-annotation label level region)

(define-structure cell list)

(define-structure CR id size parent neighbours children)
(define-structure H level max-id CRs)
(define-structure I left right)
(define-structure S size Is)
(define-structure R id size neighbors top-y parent Ss)
(define-structure D id width height Hs Rs)
(define-structure F n Ds)

(define-structure slic name track flow pixels)
(define-structure turbopixel
 name global-name track
 center speed speed-reliable? pixels)
(define-structure superpixel name next parent children velocity pixels)

(define-structure standard-video corpus sequence person location n)
(define-structure darpa-video
 corpus action subtype unknown1 unknown2
 actors background location time-of-day uuid)
(define-structure stand-alone-video pathname)

(define-structure voc4-detection x1 y1 x2 y2 parts filter strength delta ringing model)

(define-structure superpixel-annotation label superpixel)
(define-structure human-annotation-statistics timestamp clicks time type)

(define-structure superpixel-vertex
 id boundary-pixels region-pixels neighbours perimeter? label)
(define-structure superpixel-edge u v weight)

(define-structure limb-annotation
 l-arm r-arm
 l-elbow r-elbow
 l-shoulder r-shoulder
 l-pelvis r-pelvis
 l-knee r-knee
 l-foot r-foot)

(define-structure result video-name track-names features-type states verb loglk named-tracks)

(define-structure limb position visible?)

(define-structure selected point getter setter! label)

(define-structure track-annotation good? name number)

(c-define-struct-field "dim3array" "rows" int)
(c-define-struct-field "dim3array" "cols" int)
(c-define-struct-field "dim3array" "channels" int)
(c-define-struct-field "dim3array" "data" pointer)

;;; Structures used in pedro-model files
(define-structure pedro-filter blocklabel symmetric size flip symbol w)
(define-structure pedro-rule-offset w blocklabel)
(define-structure pedro-rule-def w blocklabel flip symmetric)
(define-structure pedro-rule type lhs rhs detwindow i offsets anchors defs scores)
(define-structure pedro-symbol type i filter scores)
(define-structure pedro-bbox x1 y1 x2 y2)
(define-structure pedro-model class year note numfilters numblocks numsymbols start maxsize
 minsize interval sbin thresh pyra-pad pyra-scales
 filters rules symbols blocksizes
 regmult learnmult lowerbounds fusage bboxpred scoretpt)
(define-structure pedro-pyra feats scales im-size padx pady)

(define-structure zip-file-stat filename index size compressed-size)

(define-structure zip-archive handle)
(c-ffi:add-custom-type zip pointer make-zip-archive zip-archive-handle)
(define-structure zip-source handle)
(c-ffi:add-custom-type zip-source pointer make-zip-source zip-source-handle)
(define-structure zip-file handle)
(c-ffi:add-custom-type zip-file pointer make-zip-file zip-file-handle)
(define-structure ffmpeg-video handle)
(c-ffi:add-custom-type ffmpeg-video pointer make-ffmpeg-video ffmpeg-video-handle)
(define-structure imlib-image handle)
(c-ffi:add-custom-type imlib-image pointer make-imlib-image imlib-image-handle)

(define-structure c-optical-flow handle width height)

(define-structure trained-hmm verb videos states log-likelihood
 model participants feature-type training-type)

(define-structure cuda-felzenszwalb handle next-max-detections)
(define-structure roi x1 y1 x2 y2)

;;; Variables

;; This is the new standard, if you lack this directory create a symlink to
;; where your videos are, on the servers they are in /aux/qobi/video-datasests
(define *video-pathname* (format #f "~a/video-datasets" (getenv "HOME")))

(define *live-darpa-backgrounds* '(("URBAN" 7) ("Outside" #F) ("Urban" 7) ("PARK" 6)))

(define *callback-function* #f)
(define *detector-handle* #f)
(define *optical-flow-continuation* #f)
(define *detector-continuation* #f)
(define *klt-continuation* #f)
;; TODO These are used for callbacks but they shouldn't be
(define *klt-movie* '())
(define *optical-flow-movie* '())
(define *detector-boxes-movies* '())
(define *quiet-mode?* #f)

;;; Parameters

;;; C Externals

;;; Procedures

;;; General

(define (fsystem s . fmt) (system (apply format (cons s fmt))))

(define (fsystem-output s . fmt)
 (system-output (apply format (cons s fmt))))

(define (timed-f on-time-callback thunk)
 ;; For example:
 ;; (timed-f
 ;;  (lambda (seconds)
 ;;   (format #t "Thunk executed for ~a seconds~%"
 ;; 	      (number->string-of-length-and-precision seconds 8 2)))
 ;;  thunk)
 (let* ((start (clock-sample))
	(result (thunk))
	(end (clock-sample)))
  (on-time-callback (- end start))
  result))

(define (expand-filename filename)
 (first (system-output (string-append "echo " filename))))

(define (n-cuda-cards)
 (length (remove-if-not
	  (lambda (line) (prefix? "GPU" line))
	  (system-output "nvidia-smi -L 2>/dev/null"))))

(define (set-cuda-device device)
 ((c-function void ("setCudaDevice" int)) device)
 #f)

(define (with-temporary-files a b . bs)
 (let* ((args (cons a (cons b bs)))
	(f (last args))
	(files (reverse (cdr (reverse args))))
	(filenames (map unique-temporary-file files))
	(result (apply f filenames)))
  ;; (for-each rm-if-necessary filenames)
  result))

;;; Same as read-object-from-file, but expects a gzipped file
(define (read-object-from-gzip-file filename)
 (with-temporary-file
  "ungzipsed-sc"
  (lambda (fifo)
   (system (format #f "rm -rf ~a; mkfifo ~a" fifo fifo))
   (if (and (file-exists? filename)
	    (equal? 0 (system (format #f "exec 1>&-; exec 2>&-; exec 0<&-; { cat ~a | gunzip -cd > ~a ; } & " filename fifo))))
       (read-object-from-file fifo)
       #f))))

(define unique remove-duplicates)

(define (nondeterministic-map f l)
 ;; workaround for map breaking in non-deterministic code
 (let loop ((c '()) (l l))
  (if (null? l) (reverse c) (loop (cons (f (first l)) c) (rest l)))))

(define (map-reduce2 g i f l)
 (if (null? l)
     i
     (map-reduce2
      g
      (g i (f (car l)))
      f
      (cdr l))))

(define (map-reduce3 g i f l1 l2)
 (if (null? l1)
     i
     (map-reduce3
      g
      (g i (f (car l1) (car l2)))
      f
      (cdr l1)
      (cdr l2))))

(define (all-pairs l)
 (if (null? l)
     '()
     (append
      (all-pairs (cdr l))
      (map (lambda (r) (list (car l) r)) (cdr l)))))

(define (map-all-pairs f l) (map (lambda (e) (f (first e) (second e))) (all-pairs l)))

(define (centroid pixels)
 `#(,(/ (map-reduce + 0 x pixels) (length pixels))
    ,(/ (map-reduce + 0 y pixels) (length pixels))))

(define (const a) (lambda _ a))

(define (for-each-pixel-in-rectangle x1 y1 half-x-size half-y-size f)
 (for-each-range
  (lambda (dx)
   (for-each-range
    (lambda (dy)
     (f (+ dx x1) (+ dy y1)))
    (- half-y-size) half-y-size))
  (- half-x-size) half-x-size))

(define (for-each-approximate-rectangle
	 f width height rectangle-width rectangle-height)
 ;; Will slightly scale requested rectangles so that they tile the input
 (let* ((ny (exact-round (/ height rectangle-height)))
	(nx (exact-round (/ width rectangle-width)))
	(rh (/ height ny))
	(rw (/ width nx)))
  (for-each-n
   (lambda (x)
    (for-each-n
     (lambda (y) (f (exact-round (* x rw))
	       (exact-round (* y rh))
	       (exact-round (* (+ x 1) rw))
	       (exact-round (* (+ y 1) rh))))
     ny))
   nx)))

(define (for-each-range f i j)
 (if (> i j) #f (begin (f i) (for-each-range f (+ 1 i) j))))

(define (between? i a b) (and (< a i) (< i b)))

(define (current-time)
 (let ((time-file (string-append "/tmp/time-" (number->string (getpid)))))
  (system (format #f "date +'%s %N' > ~a" time-file))
  (let ((t (first (map fields (read-file time-file)))))
   (rm time-file)
   (string->number (string-append (first t) "." (second t))))))

(define (drop-if-possible n l) (if (> (length l) n) (drop n l) '()))
(define (take-if-possible n l) (if (> (length l) n) (take n l) l))

(define (split-by f a)
 (define (loop f a in out)
  (cond ((null? a) (list in out))
	((f (car a)) (loop f (cdr a) (cons (car a) in) out))
	(else (loop f (cdr a) in (cons (car a) out)))))
 (loop f a '() '()))

(define (split-into n l)
 (let ((len (inexact->exact (floor (/ (length l) n)))))
  (let loop ((l l) (mod (modulo (length l) n)))
   (cond ((null? l) '())
	 ((> mod 0) (cons (take (+ len 1) l)
			  (loop (drop (+ len 1) l) (- mod 1))))
	 ((= mod 0) (cons (take len l) (loop (drop len l) mod)))
	 (else (fuck-up))))))

(define (split-into-lists-of-n l n)
 (unless (= 0 (remainder (length l) n))
  (panic "Call to (length l) was not a multiple of n"))
 (let loop ((l l) (res '()))
  (if (null? l)
      (reverse res)
      (loop (drop n l) (cons (take n l) res)))))

(define (maximum-matrix t)
 (maximum (map (lambda (a) (maximum (vector->list a))) (vector->list t))))

(define (minimum-matrix t)
 (minimum (map (lambda (a) (minimum (vector->list a))) (vector->list t))))

(define (four-connected? p1 p2)
 (or (some (lambda (e) (equal? p1 e)) `(,(v+ p2 '#(1 0)) ,(v+ p2 '#(-1 0))
				   ,(v+ p2 '#(0 1)) ,(v+ p2 '#(0 -1))))
     (some (lambda (e) (equal? p2 e)) `(,(v+ p1 '#(1 0)) ,(v+ p1 '#(-1 0))
				   ,(v+ p1 '#(0 1)) ,(v+ p1 '#(0 -1))))))

(define (eight-connected? p1 p2)
 (or (four-connected? p1 p2)
     (some (lambda (e) (equal? p1 e)) `(,(v+ p2 '#(1 1)) ,(v+ p2 '#(-1 1))
					,(v+ p2 '#(1 -1)) ,(v+ p2 '#(-1 -1))))
     (some (lambda (e) (equal? p2 e)) `(,(v+ p1 '#(1 1)) ,(v+ p1 '#(-1 1))
					,(v+ p1 '#(1 -1)) ,(v+ p1 '#(-1 -1))))))

(define (replace-whole-extension pathname extension)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (string-append (strip-extension pathname) extension))

(define (neighbourhood-4 m i j)
 (map (lambda (p) (image-ref m p))
      (remove-if (lambda (a) (or (>= (x a) (matrix-columns m)) (< (x a) 0)
				 (>= (y a) (matrix-rows m)) (< (y a) 0)))
		 (list `#(,(+ i 1) ,j)
		       `#(,(- i 1) ,j)
		       `#(,i ,(+ j 1))
		       `#(,i ,(- j 1))))))

(define (neighbourhood-8 m i j)
 (map (lambda (p) (image-ref m p))
      (remove-if (lambda (a) (or (>= (x a) (matrix-columns m)) (< (x a) 0)
				 (>= (y a) (matrix-rows m)) (< (y a) 0)))
		 (list `#(,(+ i 1) ,j)
		       `#(,(- i 1) ,j)
		       `#(,i ,(+ j 1))
		       `#(,i ,(- j 1))
		       `#(,(+ i 1) ,(+ j 1))
		       `#(,(- i 1) ,(+ j 1))
		       `#(,(+ i 1) ,(- j 1))
		       `#(,(- i 1) ,(- j 1))))))

(define (most-common-4-neighbour m j i e)
 (first (maximump (remove-if (lambda (c) (equal? (first c) e))
			     (equivalence-classes (neighbourhood-4 m i j)))
		  length)))

(define (most-common-8-neighbour m j i e)
 (first (maximump (remove-if (lambda (c) (equal? (first c) e))
			     (equivalence-classes (neighbourhood-8 m i j)))
		  length)))

(define (fill-matrix-gaps m gap)
 (map-indexed-matrix
  (lambda (e i j) (if (equal? e gap)
		      (most-common-8-neighbour m i j gap)
		      e))
  m))

(define (safe-matrix-ref m y x default)
 (if (or (>= x (matrix-columns m)) (< x 0)
	 (>= y (matrix-rows m)) (< y 0))
     default
     (matrix-ref m y x)))

(define (string*-append . objs)
 (cond ((null? objs) "")
       ((string? (car objs))
	(string-append (car objs) (apply string*-append (cdr objs))))
       ((number? (car objs))
	(string-append (number->string (car objs)) (apply string*-append (cdr objs))))
       (else (fuck-up))))

;; Removes stragglers from a frame
;; A pixels label is changed if it has neighbors of a different
;; labelling and it has less than 4 neighbors of its own class.
;; The label is changed to the label that occurs most in the
;; pixel's 8 neighbors, or, if two labels occur the same number
;; of times, to the label that occurs most in the 4 bordering pixels
;; Returns a list containing the modified frame and boolean which
;; is true if changes were made
(define (frame-remove-stragglers8-converged f)
 (define flag #f)
 (list
  (map-indexed-matrix
   (lambda (e i j)
    (let* ((classes (equivalence-classes (neighbourhood-8 f j i)))
	   (own-class (or (find-if (lambda (c) (equal? (first c) e)) classes)
			  '()))
	   (sorted (sort (remove-if (lambda (c) (equal? (first c) e)) classes)
			 > length)))
     (if (or (and (> (length classes) 1) (< (length own-class) 4)
		  (< (length own-class)
		     (- (length (neighbourhood-8 f j i)) (length own-class))))
	     (null? own-class))
	 (begin (set! flag #t)
		(if (and (> (length sorted) 1)
			 (= (length (car sorted)) (length (cadr sorted))))
		    (first
		     (maximump
		      (equivalence-classes
		       (or (remove-if (lambda (c) (equal? c e))
				      (neighbourhood-4 f j i))
			   `(,e)))
		      length))
		    (first (first sorted))))
	 e)))
   f)
  flag))

(define (random-split-percentage l percentage)
 (let* ((l (deal l))
	(in (inexact->exact (* percentage (length l)))))
  (list (take in l) (drop in l))))

(define (random-subset l percentage)
 (let* ((l (deal l))
	(in (inexact->exact (* percentage (length l)))))
  (take in l)))

;; Pathological cases:
;;
;; | 0 0      | 0 0  This case was solved by the addition of (< (l own) (- ...
;; | 0 0 <--> | 1 0  That clause makes sure that if a pixel is surrounded by
;; | 1 2      | 2 2  more of it's own labels than other labels, it won't change
;; | 2 2      | 2 2
;;
;; 0 0 2 2      0 0 2 2 This is unresolved, but improbable enough, we're calling
;; 0 0 2 2 <--> 0 2 2 2 it good enough.
;; 1 1 2 2      1 0 0 2
;; 1 1 0 0      1 1 1 0

(define (frame-remove-stragglers8 f)
 (first (frame-remove-stragglers8-converged f)))

(define (frame-remove-stragglers8-converging f max-iterations)
 (let ((result (frame-remove-stragglers8-converged f)))
  (if (and (second result) (> max-iterations 0))
      (frame-remove-stragglers8-converging (first result) (- max-iterations 1))
      (first result))))

(define (viterbi merge-f weight-f transition-f nodes-per-frame)
 ;; weight-f :: a -> Num
 ;; transition-f :: a -> a -> Num
 ;; nodes-per-frame :: [[a]]
 (define (cost delta node)
  (merge-f (first delta)
	   (weight-f node)
	   (transition-f (first (second delta)) node)))
 (let* ((delta (map (lambda (node) (list (weight-f node) (list node))) (first nodes-per-frame)))
	(result
	 (minimump
	  (let loop ((delta delta) (nodes-per-frame (cdr nodes-per-frame)))
	   (if (null? nodes-per-frame)
	       delta
	       (loop
		(map
		 (lambda (node)
		  (let ((delta (minimump delta (lambda (previous-delta) (cost previous-delta node)))))
		   (list (cost delta node) (cons node (second delta)))))
		 (first nodes-per-frame))
		(cdr nodes-per-frame))))
	  first)))
  (list (first result) (reverse (second result)))))

(define (partition-reversed-if p l)
 (let loop ((l l) (r '()) (c '()))
  (cond ((null? l) (list c r))
	((p (first l)) (loop (rest l) r (cons (first l) c)))
	(else (loop (rest l) (cons (first l) r) c)))))

(define (bin l f n b e)
 (let* ((s (/ (- e b) n))
	(s/2 (/ s 2)))
  (let loop ((p b) (l l) (bins '()))
   (if (> p e)
       bins
       (let ((partition (partition-reversed-if
			 (lambda (a) (and (< (- p s/2) (f a)) (<= (f a) (+ p s/2))))
			 l)))
	(loop
	 (+ p s)
	 (second partition)
	 (cons (first partition) bins)))))))

(define-structure element name exists? output fv)

(define (merge-partitions f l1 l2)
 (map (lambda (a b) (unless (equal? (first a) (first b)) (fuck-up)) (f a b))
      l1 l2))

(define (e-true es) (remove-if-not element-exists? es))
(define (e-false es) (remove-if element-exists? es))

(define (e-true-positive t e) (and (>= (element-output e) t) (element-exists? e)))
(define (e-true-negative t e) (and (< (element-output e) t) (not (element-exists? e))))
(define (e-false-positive t e) (and (>= (element-output e) t) (not (element-exists? e))))
(define (e-false-negative t e) (and (< (element-output e) t) (element-exists? e)))

(define (e-true-positives thresh elements)
 (remove-if-not (lambda (a) (and (>= (element-output a) thresh) (element-exists? a))) elements))
(define (e-true-negatives thresh elements)
 (remove-if-not (lambda (a) (and (< (element-output a) thresh) (not (element-exists? a)))) elements))
(define (e-false-positives thresh elements)
 (remove-if-not (lambda (a) (and (>= (element-output a) thresh) (not (element-exists? a)))) elements))
(define (e-false-negatives thresh elements)
 (remove-if-not (lambda (a) (and (< (element-output a) thresh) (element-exists? a))) elements))

(define (t/f-vector->0/1 v) (map-vector (lambda (a) (if a 1 0)) v))

(define (e-all e)
 (list
  (t/f-vector->0/1
   (vector (e-true-positive (- infinity) e) (e-true-negative (- infinity) e)
	   (e-false-positive (- infinity) e) (e-false-negative (- infinity) e)))
  (t/f-vector->0/1
   (vector (e-true-positive infinity e) (e-true-negative infinity e)
	   (e-false-positive infinity e) (e-false-negative infinity e)))))

(define (true-positive a) (vector-ref a 0))
(define (true-negative a) (vector-ref a 1))
(define (false-positive a) (vector-ref a 2))
(define (false-negative a) (vector-ref a 3))

(define (e-all-sum->p-r a)
 (list
  (if (= (+ (true-positive a) (false-positive a)) 0)
      0
      (/ (true-positive a) (+ (true-positive a) (false-positive a))))
  (if (= (+ (true-positive a) (false-negative a)) 0)
      0
      (/ (true-positive a) (+ (true-positive a) (false-negative a))))))

(define (e-all-sum->tpr-fpr a)
 (list
  (if (= (+ (vector-ref a 0) (vector-ref a 3)) 0)
      0
      (/ (vector-ref a 0) (+ (vector-ref a 0) (vector-ref a 3))))
  (if (= (+ (vector-ref a 2) (vector-ref a 1)) 0)
      0
      (/ (vector-ref a 2) (+ (vector-ref a 2) (vector-ref a 1))))))

(define (sumv4 vs) (foldl v+ vs '#(0 0 0 0)))

(define (map-e-tpr-fpr-medial es)
 (let ((cs (map e-all (sort es < element-output))))
  (map
   e-all-sum->tpr-fpr
   (foldl (lambda (c l)
	   (cons (v+ (second l) (v- (first c) (first l))) c))
	  (zip (map first cs) (map second cs))
	  (list (sumv4 (map first cs)))))))

(define (map-all-pairs2 f l1 l2)
 (if (null? l1)
     '()
     (let ((a1 (car l1)))
      (append (map (lambda (a2) (f a1 a2)) l2)
	      (map-all-pairs2 f (cdr l1) l2)))))

(define (map-e-tpr-fpr-medial2 es1 es2)
 (define (compute cs)
  (foldl (lambda (c l)
	  (cons (v+ (second l) (v- (first c) (first l))) c))
	 (zip (map first cs) (map second cs))
	 (list (sumv4 (map first cs)))))
 (let ((data1 (compute (map e-all (sort es1 < element-output))))
       (data2 (compute (map e-all (sort es2 < element-output)))))
  (map-all-pairs2 (lambda (a b) (e-all-sum->tpr-fpr (v+ a b))) data1 data2)))

(define (map-e-tpr-fpr-medial2-fv es fv1 fv2)
 (define (compute cs)
  (foldl (lambda (c l)
	  (cons (v+ (second l) (v- (first c) (first l))) c))
	 (zip (map first cs) (map second cs))
	 (list (sumv4 (map first cs)))))
 (let
   ((data1
     (compute
      (map e-all
	   (sort (remove-if-not (lambda (e) (= (element-fv e) fv1)) es)
		 < element-output))))
    (data2
     (compute
      (map e-all
	   (sort (remove-if-not (lambda (e) (= (element-fv e) fv2)) es)
		 < element-output)))))
  (map-all-pairs2 (lambda (a b) (e-all-sum->tpr-fpr (v+ a b))) data1 data2)))

(define (e-accuracy thresh elements)
 (if (= (length elements) 0)
     0
     (/ (+ (length (e-true-positives thresh elements))
	   (length (e-true-negatives thresh elements)))
	(length elements))))
(define (e-accuracy-n all a b n)
 (map-n (lambda (v)
	 `(,(+ a (* v (/ (- b a) n)))
	   ,(e-accuracy all (+ a (* v (/ (- b a) n)))))) (+ 1 n)))

(define (e-pr thresh elements)
 (precision-recall-threshold-p thresh elements element-output element-exists?))
(define (e-prf-n all a b n)
 (map-n (lambda (v)
	 `(,(+ a (* v (/ (- b a) n)))
	   ,(e-pr (+ a (* v (/ (- b a) n))) all))) (+ 1 n)))
(define (e-tpr-fpr all t)
 (let* ((tp (e-true-positives t all))
	(tn (e-true-negatives t all))
	(fp (e-false-positives t all))
	(fn (e-false-negatives t all))
	(tpr (if (= (+ (length tp) (length fn)) 0)
		 0
		 (/ (length tp) (+ (length tp) (length fn)))))
	(fpr (if (= (+ (length fp) (length tn)) 0)
		 0
		 (/ (length fp) (+ (length fp) (length tn))))))
  `(,tpr ,fpr)))

(define (e-tpr-fpr2 all1 all2 t1 t2)
 (let* ((tp (+ (length (e-true-positives t1 all1)) (length (e-true-positives t2 all2))))
	(tn (+ (length (e-true-negatives t1 all1)) (length (e-true-negatives t2 all2))))
	(fp (+ (length (e-false-positives t1 all1)) (length (e-false-positives t2 all2))))
	(fn (+ (length (e-false-positives t1 all1)) (length (e-false-negatives t2 all2))))
	(tpr (if (= (+ tp fn) 0) 0 (/ tp (+ tp fn))))
	(fpr (if (= (+ fp tn) 0) 0 (/ fp (+ fp tn)))))
  `(,tpr ,fpr)))

(define (group-nth l n)
 (if (null? l)
     '()
     (cons (take-if-possible n l) (group-nth (drop-if-possible n l) n))))

(define (group-by f l)
 (transitive-equivalence-classesp (lambda (a b) (equal? (f a) (f b))) l))

(define (map-video-corpus-servers f corpus)
 (map (lambda (a) (f (field-ref a 1) (field-ref a 2)))
      (sort
       (remove-if-not
	(lambda (a) (equal? (field-ref a 0) corpus))
	(read-file
	 (string-append (getenv "HOME")
			"/darpa-collaboration/documentation/darpa-corpora-server.text")))
       string<?
       (lambda (a) (field-ref a 2)))))

(define (map-video-corpus-server f server corpus)
 (map (lambda (a) (f (field-ref a 1)))
      (remove-if-not
       (lambda (a) (and (equal? (field-ref a 0) corpus)
		   (equal? (field-ref a 2) server)))
       (read-file
	(string-append (getenv "HOME")
		       "/darpa-collaboration/documentation/darpa-corpora-server.text")))))

(define (swap-commas-and-spaces string)
 (list->string (map (lambda (char)
		     (cond ((char=? char #\,) #\space)
			   ((char=? char #\space) #\,)
			   (else char)))
		    (string->list string))))

(define (drop-while f l)
 (let loop ((l l))
  (if (or (null? l) (not (f (car l))))
      l
      (loop (cdr l)))))

(define (take-while f l)
 (let loop ((l l) (r '()))
  (if (or (null? l) (not (f (car l))))
      r
      (loop (cdr l) (cons (car l) r)))))

(define (partition-while f l)
 (let loop ((l l) (r '()))
  (if (or (null? l) (not (f (car l))))
      (list r l)
      (loop (cdr l) (cons (car l) r)))))

(define (bin-sorted l f n b e)
 (let* ((s (/ (- e b) n))
	(s/2 (/ s 2)))
  (let loop ((p b)
	     (l (drop-while (lambda (a) (<= (f a) (- b s/2))) (sort l < f)))
	     (bins '()))
   (if (> p e)
       bins
       (let ((partition (partition-while (lambda (a) (<= (f a) (+ p s/2))) l)))
	(loop
	 (+ p s)
	 (second partition)
	 (cons (first partition) bins)))))))

(define (graph-roc-and-bins bins)
 (mplot->>=*
  (apply mplot->>=*
	 (map (lambda (a) (mplot-line (map second a) (map first a))) bins))
  (mplot-line '(0 1) '(0 1) '("color" "'grey'") '("linestyle" "'solid'"))
  (mplot-x-label "false positive rate")
  (mplot-y-label "true positive rate")
  (mplot-square-axes)
  (mplot-return "plot.ylim([0,1])")
  (mplot-return "plot.xlim([0,1])")))

(define (graph-roc-and-bins-indexed verb-bins verb-colors)
 (mplot->>=*
  (apply mplot->>=*
	 (map-indexed
	  (lambda (vb i)
	   (mplot->>=*
	    (apply mplot-line-style
		   (map second (second vb))
		   (map first (second vb))
		   (cons
		    '("label" (string-append "'" (first vb) "'"))
		    (cdr (assoc (first vb) verb-colors))))
	    (mplot-return (format #f "p~a = p" i))))
	  verb-bins))
  (mplot-line '(0 1) '(0 1) '("color" "'grey'") '("linestyle" "'solid'"))
  (mplot-x-label "false positive rate")
  (mplot-y-label "true positive rate")
  (mplot-return (string-append "leg = plot.legend(["
			       (string-join "," (map-n (lambda (a) (format #f "p~a" a)) (length verb-bins)))
			       "],[" (string-join "," (map (lambda (a) (string-append "'" (first a) "'")) verb-bins)) "],
			       ncol=1,loc=(1.03,0))"))
  (mplot-square-axes)
  (mplot-return "plot.ylim([0,1])")
  (mplot-return "plot.xlim([0,1])")))

;;; Timing

(define *time-depth* 0)

(define (time-thunk format-string thunk)
 (let* ((start (current-time))
	(result (thunk))
	(end (current-time)))
  (unless *quiet-mode?*
   (format #t format-string
	   (number->string-of-length-and-precision (- end start) 8 2)))
  result))

;;; Drawing labels

(define (digit->matrix d)
 (case d
  ((0) '#(#(#T #T #T #T #T) #(#T #f #f #f #T) #(#T #f #f #f #T) #(#T #f #f #f #T) #(#T #T #T #T #T)))
  ((1) '#(#(#F #F #F #F #t) #(#F #F #F #F #t) #(#F #F #F #F #t) #(#F #F #F #F #t) #(#F #F #F #F #t)))
  ((2) '#(#(#t #t #t #t #t) #(#F #F #F #F #t) #(#t #t #t #t #t) #(#t #F #F #F #F) #(#t #t #t #t #t)))
  ((3) '#(#(#t #t #t #t #t) #(#F #F #F #F #t) #(#t #t #t #t #t) #(#F #F #F #F #t) #(#t #t #t #t #t)))
  ((4) '#(#(#t #F #F #F #F) #(#t #F #F #F #F) #(#t #t #t #t #t) #(#F #F #F #F #t) #(#F #F #F #F #t)))
  ((5) '#(#(#t #t #t #t #t) #(#t #F #F #F #f) #(#t #t #t #t #t) #(#f #F #F #F #t) #(#t #t #t #t #t)))
  ((6) '#(#(#t #t #t #t #t) #(#t #F #F #F #f) #(#t #t #t #t #t) #(#t #F #F #F #t) #(#t #t #t #t #t)))
  ((7) '#(#(#t #t #t #t #t) #(#F #F #F #F #t) #(#F #F #F #F #t) #(#F #F #F #F #t) #(#F #F #F #F #t)))
  ((8) '#(#(#t #t #t #t #t) #(#t #F #F #F #t) #(#t #t #t #t #t) #(#t #F #F #F #t) #(#t #t #t #t #t)))
  ((9) '#(#(#t #t #t #t #t) #(#t #F #F #F #t) #(#t #t #t #t #t) #(#f #F #F #F #t) #(#f #f #f #f #t)))))

(define (integer->digits i)
 (if (zero? i)
     '(0)
     (let loop ((i i) (d '()))
      (if (zero? i)
	  d
	  (loop (inexact->exact (floor (/ i 10)))
		(cons (modulo i 10) d))))))

(define (integer->matrix i)
 (let ((mats (map digit->matrix (integer->digits i))))
  (if (= (length mats) 1)
      (first mats)
      (let loop ((ms (rest mats)) (mat (first mats)))
       (if (null? ms)
	   mat
	   (loop (rest ms)
		 (map-vector (lambda (r1 r2) (vector-append r1 '#(#f) r2)) mat (first ms))))))))

(define (matrix-set-mat! mat m p)
 (let* ((h (matrix-rows mat)) (h/2 (inexact->exact (floor (/ (matrix-rows m) 2))))
	(w (matrix-columns mat)) (w/2 (inexact->exact (floor (/ (matrix-columns m) 2))))
	(xc (if (>= (- (x p) w/2) 0) (if (< (+ (x p) w/2) w) (x p) (- w w/2)) w/2))
	(yc (if (>= (- (y p) h/2) 0) (if (< (+ (y p) h/2) h) (y p) (- h h/2)) w/2)))
  (for-each-pixel-in-rectangle
   xc yc w/2 h/2
   (lambda (i j)
    (matrix-set! mat j i (matrix-ref m (+ (- j (y p)) h/2) (+ (- i (x p)) w/2)))))
  mat))

;;; Quantization

(define (quantize-coordinate x) (inexact->exact (round x)))

(define (quantize-point p)
 (vector (quantize-coordinate (x p)) (quantize-coordinate (y p))))

(define (quantize-points ps) (remove-duplicates (map quantize-point ps)))

(define (quantize-line-segment l)
 (make-line-segment (quantize-point (p l)) (quantize-point (q l))))

(define (quantize-line-segments ls)
 (remove-duplicates (map quantize-line-segment ls)))

;;; Models

;;; needs work: screen coordinates
(define (unit-theta theta) (vector (cos theta) (sin theta)))

(define (one-lsm-model u)
 ;; needs work: screen coordinates
 (let ((midpoint (vector (vector-ref u 0) (vector-ref u 1)))
       (theta (degrees->radians (vector-ref u 2)))
       (length (vector-ref u 3)))
  (list (make-line-segment
	 (v- midpoint (k*v (* 0.5 length) (unit-theta theta)))
	 (v+ midpoint (k*v (* 0.5 length) (unit-theta theta)))))))

(define (two-uncoupled-lsm-model u)
 ;; needs work: screen coordinates
 (let ((midpoint1 (vector (vector-ref u 0) (vector-ref u 1)))
       (theta1 (degrees->radians (vector-ref u 2)))
       (length1 (vector-ref u 3))
       (midpoint2 (vector (vector-ref u 4) (vector-ref u 5)))
       (theta2 (degrees->radians (vector-ref u 6)))
       (length2 (vector-ref u 7)))
  (list (make-line-segment
	 (v- midpoint1 (k*v (* 0.5 length1) (unit-theta theta1)))
	 (v+ midpoint1 (k*v (* 0.5 length1) (unit-theta theta1))))
	(make-line-segment
	 (v- midpoint2 (k*v (* 0.5 length2) (unit-theta theta2)))
	 (v+ midpoint2 (k*v (* 0.5 length2) (unit-theta theta2)))))))

(define (two-lsm-model u)
 ;; needs work: screen coordinates
 (let* ((midpoint (vector (vector-ref u 0) (vector-ref u 1)))
	(theta (degrees->radians (vector-ref u 2)))
	(length (vector-ref u 3))
	(l (make-line-segment
	    (v- midpoint (k*v (* 0.5 length) (unit-theta theta)))
	    (v+ midpoint (k*v (* 0.5 length) (unit-theta theta)))))
	(branch2 (/ (vector-ref u 4) 100.0))
	(p2 (v+ (p l) (k*v branch2 (v- (q l) (p l)))))
	(theta2
	 (degrees->radians (+ (line-segment-orientation l) (vector-ref u 5))))
	(length2 (vector-ref u 6)))
  (list l (make-line-segment p2 (v+ p2 (k*v length2 (unit-theta theta2)))))))

(define (linear-temporal-model model indices)
 (lambda (t)
  (lambda (u)
   (model (map-vector (lambda (index)
		       (if (pair? index)
			   (+ (* (vector-ref u (first index)) t)
			      (vector-ref u (second index)))
			   (vector-ref u index)))
		      indices)))))

;;; Chamfer Distance

;;; AD-friendly
(define (min2 a b) (if (< a b) a b))

(define (undirectional-chamfer-distance points1 points2)
 (map-reduce
  +
  0
  (lambda (point1)
   (map-reduce
    min2 infinity (lambda (point2) (distance point1 point2)) points2))
  points1))

(define (bidirectional-chamfer-distance points1 points2)
 (+ (undirectional-chamfer-distance points1 points2)
    (undirectional-chamfer-distance points2 points1)))

(define (cyclic-multivariate-argmin-R f x ds)
 (let ((g (gradient-R f)) (n (vector-length ds)))
  (let loop ((x x)
	     (fx (f x))
	     (gx (g x))
	     (etas (map-n (lambda (i) 1e-5) n)) ;hardwired
	     (is (map-n (lambda (i) 1) n))
	     (k 0)
	     (j 0))
   (let ((x-prime
	  (v- x (k*v (list-ref etas k) (map-vector * (vector-ref ds k) gx)))))
    (when *debugging?*
     (format #t "x: ~s~%||x-x'||=~s~%fx: ~s~%gx: ~s~%||gx||: ~s~%etas: ~s~%is: ~s~%k: ~s~%j: ~s~%~%"
	     x (distance x x-prime) fx gx (magnitude gx) etas is k j))
    ;; needs work: the termination criterion is a kludge and hardwired
    (if (or (and (<= (distance x x-prime) 1e-1) (<= (magnitude gx) 1e2))
	    (<= (distance x x-prime) 1e-3))
	(if (>= j n) x (loop x fx gx etas is (modulo (+ k 1) n) (+ j 1)))
	(let ((fx-prime (f x-prime)))
	 (if (< fx-prime fx)
	     (if (= (list-ref is k) 2)	;hardwired
		 (loop x-prime
		       fx-prime
		       (g x-prime)
		       ;; hardwired
		       (list-replace etas k (* 2.0 (list-ref etas k)))
		       (list-replace is k 0)
		       (modulo (+ k 1) n)
		       0)
		 (loop x-prime
		       fx-prime
		       (g x-prime)
		       etas
		       (list-replace is k (+ (list-ref is k) 1))
		       (modulo (+ k 1) n)
		       0))
	     (loop x
		   fx
		   gx
		   ;; hardwired
		   (list-replace etas k (/ (list-ref etas k) 2.0))
		   (list-replace is k 0)
		   k
		   0))))))))

(define (render-frame model u)
 (map-reduce append
	     '()
	     (lambda (l) (ellipse->points (line-segment->ellipse l)))
	     (model u)))

(define (render-frames n model u)
 (map-n (lambda (t)
	 (map-reduce append
		     '()
		     (lambda (l) (ellipse->points (line-segment->ellipse l)))
		     ((model t) u)))
	n))

(define (subsample-point points n)
 (map (lambda (point) (vector (* (x point) n) (* (y point) n)))
      (remove-duplicates
       (map (lambda (point)
	     (vector (quotient (x point) n) (quotient (y point) n)))
	    points))))

(define (fit-frame pbm model u ds)
 (let ((points (subsample-point (pbm->points pbm) 4))) ;hardwired
  (cyclic-multivariate-argmin-R
   (lambda (u) (bidirectional-chamfer-distance points (render-frame model u)))
   u
   ds)))

(define (a-restart-value low high n)
 (let ((i (an-integer-between 0 (- n 1))))
  (+ low (/ (* i (- high low)) (- n 1)))))

(define (with-restarts f g u)
 (let* ((i 0)
	(alist
	 (all-values
	  (let ((u1 (f (map-vector
			(lambda (u)
			 (a-restart-value (first u) (second u) (third u)))
			u))))
	   (when *debugging?*
	    (format #t "Restart ~s/~s~%"
		    i (reduce-vector * (map-vector third u) 1))
	    (set! i (+ i 1)))
	   (list (g u1) u1)))))
  (second (assv (map-reduce min infinity first alist) alist))))

(define (fit-frame-with-restarts pbm model u ds)
 (let ((points (subsample-point (pbm->points pbm) 4))) ;hardwired
  (with-restarts
   (lambda (u) (fit-frame pbm model u ds))
   (lambda (u) (bidirectional-chamfer-distance points (render-frame model u)))
   u)))

(define (fit-and-render-frame pbm model u ds)
 (render-frame model (fit-frame pbm model u ds)))

(define (fit-and-render-frame-with-restarts pbm model u ds)
 (render-frame model (fit-frame-with-restarts pbm model u ds)))

(define (fit-frames pbms model u ds)
 (let ((n (length pbms))
       ;; hardwired
       (points (map (lambda (pbm) (subsample-point (pbm->points pbm) 4)) pbms)))
  (cyclic-multivariate-argmin-R
   (lambda (u)
    (map-reduce
     + 0 bidirectional-chamfer-distance points (render-frames n model u)))
   u
   ds)))

(define (fit-frames-with-restarts pbms model u ds)
 (let ((n (length pbms))
       ;; hardwired
       (points (map (lambda (pbm) (subsample-point (pbm->points pbm) 4)) pbms)))
  (with-restarts
   (lambda (u) (fit-frames pbms model u ds))
   (lambda (u)
    (map-reduce
     + 0 bidirectional-chamfer-distance points (render-frames n model u)))
   u)))

(define (fit-and-render-frames pbms model u ds)
 (let ((n (length pbms))) (render-frames n model (fit-frames pbms model u ds))))

(define (fit-and-render-frames-with-restarts pbms model u ds)
 (let ((n (length pbms)))
  (render-frames n model (fit-frames-with-restarts pbms model u ds))))

(define (moving-edges-frame video-name frame)
 (define (motion video-name frame)
  (pgm->pbm (pgm-absolute-difference
	     (read-pnm (pgm-pathname video-name frame))
	     (read-pnm (pgm-pathname video-name (+ frame 1))))
	    ;; hardwired
	    20))
 (pbm-and
  (pbm-bloat (motion video-name frame) 3) ;hardwired
  (pgm->pbm
   (read-pnm (canny-pathname video-name frame)) 128)))

(define (moving-edges-frames
	 video-name starting-frame ending-frame)
 (let loop ((frame starting-frame) (pbms '()))
  (if (> frame ending-frame)
      (reverse pbms)
      (loop
       (+ frame 1)
       (cons (moving-edges-frame video-name frame)
	     pbms)))))

;;; Corpus

(define (standard-corpus-video corpus sequence person location n)
 (make-standard-video corpus sequence person location n))

(define (generic-full-pathname pathname video name)
 (cond
  ((standard-video? video)
   (string-append pathname
		  "/"
		  (standard-video-corpus video)
		  "/"
		  (standard-video-sequence video)
		  "-"
		  (standard-video-person video)
		  "-"
		  (standard-video-location video)
		  "-"
		  (number->string (standard-video-n video))
		  name))
  ((darpa-video? video)
   (string-append pathname
		  "/"
		  (darpa-video-corpus video)
		  "/"
		  (darpa-video->name video)
		  name))
  ((stand-alone-video? video)
   (string-append (strip-extension (stand-alone-video-pathname video)) name))
  (else (fuck-up))))

;; TODO Autodetect this number
;; This global is a hack while old file formats still exist
(define *frame-padding* 6)

(define (generic-pathname video frame name)
 (generic-full-pathname *video-pathname* video
			(string-append
			 "/"
			 (number->padded-string-of-length frame *frame-padding*)
			 "/"
			 name)))

(define (generic-root-pathname video name)
 (generic-full-pathname *video-pathname* video (string-append "/" name)))

(define (generic-directory-pathname video)
 (generic-full-pathname *video-pathname* video "/"))

(define (ppm-pathname video-name frame)
 (generic-pathname video-name frame "frame.ppm"))

(define (ppm-full-pathname video-name frame)
 (generic-pathname video-name frame "frame.ppm"))

(define (ppm-half-pathname video-name frame)
 (generic-pathname video-name frame "frame-half.ppm"))

(define (pgm-pathname video-name frame)
 (generic-pathname video-name frame "frame.pgm"))

(define (canny-pathname video-name frame)
 (generic-pathname video-name frame "frame-canny.pgm"))

(define (berkeley-pathname video-name frame)
 (generic-pathname video-name frame "frame-berkeley.pgm"))

(define (berkeley-chains-pathname video-name frame)
 (generic-pathname video-name frame "frame-berkeley-chains.sc"))

(define (foo-pathname video-name frame)
 (generic-pathname video-name frame "foo.pgm"))

(define (bar-pathname video-name frame)
 (generic-pathname video-name frame "bar.pgm"))

(define (motion-pathname video-name frame)
 (generic-pathname video-name frame "motion-opencv.pgm"))

(define (motion-weighted-berkeley-chains-pathname video-name frame)
 (generic-pathname video-name frame "motion-window-berkeley.weighted-chains"))

(define (motion-weighted-berkeley-image-pathname video-name frame)
 (generic-pathname video-name frame "motion-window-berkeley.pgm"))

(define (human-annotation-pathname video-name frame)
 (generic-pathname video-name frame "human-annotation.sc"))

(define (human-annotation-essa-pathname video-name frame level)
 (generic-pathname
  video-name
  frame
  (string-append "human-annotation-essa-" (number->padded-string-of-length level 3) ".sc")))

(define (human-annotation-video-quality-pathname video-name)
 (generic-full-pathname *video-pathname* video-name "/human-annotation-video-quality.sc"))

(define (regions-pathname video-name frame level)
 (generic-pathname video-name frame
		   (string-append "regions-" (number->padded-string-of-length level 4) ".sc")))

(define (regions-pathname-flat video-name frame level)
 (generic-pathname video-name frame
		   (string-append "regions-" (number->padded-string-of-length level 4) ".labels")))

(define (human-annotation-turbopixel-pathname video-name frame)
 (generic-pathname video-name frame
		   (string-append "human-annotation-turbopixel.sc")))

(define (human-track-annotation-pathname video-name)
 (generic-full-pathname *video-pathname* video-name "/human-track-annotation.sc"))

(define (rendered-tracker-video-pathname video-name)
 (generic-full-pathname *video-pathname* video-name "/tracked.avi"))

(define (description-pathname video-name)
 (generic-full-pathname *video-pathname* video-name "/description.srt"))

(define (scheme-optical-flow-pathname video-name frame)
 (generic-pathname video-name frame "optical-flow.sc"))

(define (optical-flow-pathname video-name)
 (generic-root-pathname video-name "optical-flow.integral_flo.zip"))

(define (voc4-pathname video-name frame)
 (generic-pathname video-name frame "voc4.boxes"))

(define (tmp-avi-pathname video-name)
 (generic-full-pathname "/tmp/" video-name ".avi"))

(define (frame-pathname video-name frame)
 (generic-pathname video-name frame ""))

(define (slic-dat-pathname video-name frame prefix)
 (generic-pathname video-name frame
		   (replace-whole-extension prefix "-slic.dat")))

(define (slic-pathname video-name frame prefix)
 (generic-pathname video-name frame
		   (replace-whole-extension prefix "-slic.sc")))

(define (essa-superpixel-pathname video-name frame prefix)
 (generic-pathname video-name frame
		   (replace-whole-extension prefix "-essa.sc")))

(define (turbopixel-pathname video-name frame prefix)
 (generic-pathname video-name frame
		   (replace-whole-extension prefix "-turbopixels.sc")))

(define (superpixel-pathname video-name frame prefix superpixel-type)
 (generic-pathname
  video-name frame
  (replace-whole-extension
   prefix
   (format #f "-~a.sc"
	   (superpixel-type->string superpixel-type)))))

(define (human-annotation-superpixel-pathname video-name frame superpixel-type)
 (generic-pathname
  video-name frame
  (format #f "human-superpixel-annotation-~a.sc"
	  (superpixel-type->string superpixel-type))))

(define (cached-blank-annotation-pathname video-name frame prefix superpixel-type)
 (generic-pathname
  video-name frame
  (format #f "~a-cached-blank-annotation-~a.ppm"
	  prefix (superpixel-type->string superpixel-type))))

(define (cached-ppm-annotation-pathname video-name frame prefix superpixel-type)
 (generic-pathname
  video-name frame
  (format #f "~a-cached-ppm-annotation-~a.ppm"
	  prefix (superpixel-type->string superpixel-type))))

(define (cached-superpixels-pathname video-name frame prefix superpixel-type)
 (generic-pathname
  video-name frame
  (format #f "~a-cached-superpixels-~a.superpixels"
	  prefix (superpixel-type->string superpixel-type))))

(define (cached-superpixel-map-pathname video-name frame prefix superpixel-type)
 (generic-pathname
  video-name frame
  (format #f "~a-cached-superpixel-map-~a.sc"
	  prefix (superpixel-type->string superpixel-type))))

(define (video-length-pathname video-name)
 (generic-root-pathname video-name "video-length"))

(define (features-pathname video-name number)
 (generic-root-pathname video-name (string*-append "features-" number ".text")))

(define (cached-superpixel-adjacency video-name frame prefix superpixel-type)
 (generic-pathname
  video-name frame
  (format #f "~a-cached-superpixel-adjacency-~a.sc"
	  prefix (superpixel-type->string superpixel-type))))

(define (superpixel-type->string type)
 (cond ((equal? type 'slic) "slic")
       ((equal? type 'essa) "essa")
       ((equal? type 'turbopixels) "turbopixels")
       (else (fuck-up))))

(define (modify-pathname p name)
 (string-append (strip-extension p) name "." (extension p)))

(define (cropped-ppm-pathname video-name frame)
 (generic-pathname video-name frame "frame-cropped.ppm"))

(define (cropped-pb-pathname video-name frame)
 (generic-pathname video-name frame "frame-cropped-pb.pgm"))

(define (cropped-lines-pathname video-name frame)
 (generic-pathname video-name frame "frame-cropped-lines.sc"))

(define (frame-limbs-pb-pathname video-name frame)
 (generic-pathname video-name frame "frame-limbs-pb.pgm"))

(define (human-limb-annotation-pathname video-name frame)
 (generic-pathname video-name frame "human-limb-annotation.sc"))

(define (frame-distance-pathname video-name frame video-name2 frame2)
 (generic-pathname video-name frame
		   (format #f "frame-distance-~a-~a.sc"
			   (darpa-video->string video-name2)
			   (number->padded-string-of-length frame2 4))))

;;; Pathname for a single or pairwise features cache file
(define (cached-all-features-pathname video type lookahead)
 (let ((filename (format #f "/cached-all-~a-features-lh=~a.sc" type lookahead)))
  (generic-full-pathname *video-pathname* video filename)))

;;; Pathname to a pedro-score file, exported from matlab
(define (frame-model-pedro-scores-pathname video frame model-name)
 (let ((filename (format #f "scores_~a.sc.gz" model-name)))
  (generic-pathname video frame filename)))

(define (darpa-video-live-action? v)
 (member (darpa-video-background v) *live-darpa-backgrounds*))
(define (darpa-video-composite? v) (not (darpa-video-live-action? v)))

;;; (any-video->string video)
;;;     ==> "Run4_A1_C2_Act1_URBAN7_MR_AFTN_48e13f4a-c5af-11df-99e4-e80688cb869a"
(define (any-video->string video)
 (third (reverse (remove-if (lambda (e) (equal? e ""))
			    (pregexp-split "/"  (generic-pathname video 10 "foo"))))))

;;; Gives the corpus of any video
(define (any-video->corpus video)
 (cond
  ((darpa-video? video) (darpa-video-corpus video))
  ((standard-video? video) (standard-video-corpus video))
  (else (panic "This type of video has not yet been implemented"))))

;;; Information on every video in the corpus
(define (corpora-manifest)
 (read-file
  (string-append (getenv "HOME") "/darpa-collaboration/documentation/darpa-corpora.text")))

;;; Lists the distinct corpora(s) availabe
(define (corpora)
 (remove-duplicates (map first (map fields (corpora-manifest)))))

;;; The amazon turk data from C-D1
(define *amz-cd1-data* #f)
(define (amz-cd1)
 (unless *amz-cd1-data*
  (set! *amz-cd1-data* (read-object-from-file
			(string-append (getenv "HOME")
				       "/darpa-collaboration/documentation/amz-cd1.sc"))))
 *amz-cd1-data*)

;;; The Amazon Turk data for C-E1
(define *amz-ce1-data* #f)
(define (amz-ce1)
 (unless *amz-ce1-data*
  (set! *amz-ce1-data* (read-object-from-file
			(string-append (getenv "HOME")
				       "/darpa-collaboration/documentation/amz-ce1.sc"))))
 *amz-ce1-data*)

;;; Returns all videos for a given corpus
(define (videos-by-corpus corpus)
 (unless *darpa-corpora* (load-darpa-corpora!))
 (let* ((corpus-fields  *darpa-corpora*)
	(corpus-matches (remove-if-not (lambda (l) (equal? (first l) corpus)) corpus-fields)))
  (map (lambda (v) (string->darpa-video-from-corpus v corpus))
       (map second corpus-matches))))

;;; Returns all darpa-videos for a given corpus and verb
(define (videos-by-verb corpus verb)
 (define (search-cached-amz-data id amz-data)
  (let* ((verb-downcase (string-downcase verb))
	 (verb-dc
	  (cond
	   ((equal? verb-downcase "pick") "pick up")
	   ((equal? verb-downcase "put") "put down")
	   (else verb-downcase)))
	 (dirname "/tmp/cached-corpus-verb-lists")
	 (filename (format #f "~a/amz_~a_~a.sc" dirname id verb-downcase)))
   (system (format #f "mkdir -p ~a" dirname))
   (unless (file-exists? filename)
    (let* ((verb-video-names (remove-if-not (lambda (l) (equal? (first l) verb-dc)) amz-data))
	   (all-video-names (if verb-video-names (second (first verb-video-names)) '()))
	   (video-names (map first (remove-if-not (lambda (p) (second p)) all-video-names))))
     (write-object-to-file
      (map (lambda (v) (string->darpa-video-from-corpus v corpus)) video-names)
      filename)))
   (read-object-from-file filename)))
 (cond
  ((equal? "C-D1/recognition" corpus) (search-cached-amz-data "C-D1-recognition" (amz-cd1)))
  ((equal? "C-E1/recognition" corpus) (search-cached-amz-data "C-E1-recognition" (amz-ce1)))
  ((remove-if-not (lambda (v) (equal? verb (darpa-video-verb v)))
		  (videos-by-corpus corpus)))))

;;; Returns all darpa-videos for a given corpus, which do _not_ match verb
(define (videos-by-not-verb corpus verb)
 (let* ((verb-dc (string-downcase verb))
	(dirname "/tmp/cached-corpus-verb-lists")
	(filename (format #f "~a/~a_not_~a.sc"
			  dirname (pregexp-replace* "/" corpus "-")  verb-dc)))
  (unless (file-exists? filename)
   (write-object-to-file
    (set-differencep (lambda (v1 v2) (equal? (any-video->string v1) (any-video->string v2)))
		     (videos-by-corpus corpus)
		     (videos-by-verb corpus verb))
    filename))
  (read-object-from-file filename)))

(define (compute-video-length video-name)
 (with-ffmpeg-video
  video-name
  (lambda (video)
   (let loop ((i 1))
    (if (ffmpeg-video-finished? video)
	i
	(begin (ffmpeg-next-frame! video) (loop (+ i 1))))))))

(define (mkdir-p pathname)
 (unless (zero? (system (format #f "mkdir -p ~a 2>/dev/null" (quotify pathname))))
  (format #t "~a~%" pathname)
  (panic "MKDIR failed")))

(define (video-length video-name)
 (unless (file-exists? (video-length-pathname video-name))
  (mkdir-p (generic-directory-pathname video-name))
  (write-object-to-file (compute-video-length video-name)
			(video-length-pathname video-name)))
 (read-object-from-file (video-length-pathname video-name)))

(define (video-last-frame video-name)
 (- (video-length video-name) 1))

(define (for-each-frame f v)
 (for-each-m-n f (video-first-frame v) (video-last-frame v)))
(define (map-frame f v)
 (map-m-n f (video-first-frame v) (video-last-frame v)))

(define (map-frame-indexed f v)
 (let ((first-frame (video-first-frame v)))
  (map-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (video-last-frame v))))

(define (for-each-frame-indexed f v)
 (let ((first-frame (video-first-frame v)))
  (for-each-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (video-last-frame v))))

(define (map-frame-indexed-but-last f v)
 (let ((first-frame (video-first-frame v)))
  (map-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (- (video-last-frame v) 1))))

(define (for-each-frame-indexed-but-last f v)
 (let ((first-frame (video-first-frame v)))
  (for-each-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (- (video-last-frame v) 1))))

(define (for-each-frame-but-last f v)
 (for-each-m-n f (video-first-frame v) (- (video-last-frame v) 1)))
(define (map-frame-but-last f v)
 (map-m-n f (video-first-frame v) (- (video-last-frame v) 1)))

(define (map-frame-pair individual-f pair-f video-name)
 (let ((first-frame (video-first-frame video-name))
       (last-frame (video-last-frame video-name)))
  (let loop ((n (+ first-frame 1))
	     (prev (individual-f first-frame))
	     (result '()))
   (if (> n last-frame)
       (reverse result)
       (let ((next (individual-f n)))
	(loop (+ n 1) next (cons (pair-f prev next) result)))))))
(define (for-each-frame-pair individual-f pair-f video-name)
 (let ((first-frame (video-first-frame video-name))
       (last-frame (video-last-frame video-name)))
  (let loop ((n (+ first-frame 1))
	     (prev (individual-f first-frame)))
   (unless (> n last-frame)
    (let ((next (individual-f n)))
     (pair-f prev next)
     (loop (+ n 1) next))))))

(define (motion-window-rc-metadata-pathname video-name frame alpha cycle)
 (format #f
	 (generic-pathname video-name frame "motion-window-rc-~a-~a.metadata")
	 alpha
	 cycle))

(define (motion-window-rc-cycle-pathname video-name frame alpha cycle)
 (format #f
	 (generic-pathname video-name frame "motion-window-rc-~a-~a-0.cycle")
	 alpha
	 cycle))

(define (motion-window-rc-chains-pathname video-name frame alpha cycle)
 (format #f
	 (generic-pathname video-name frame "motion-window-rc-~a-~a.chains")
	 alpha
	 cycle))

(define (essa-pathname video-name frame)
 (generic-pathname video-name frame "essa.sc"))

(define (turbopixel-mat-pathname video-name)
 (generic-full-pathname *video-pathname* video-name "/turbopixels.mat"))

(define (human-annotation-statistics-pathname video-name . frame)
 (if (null? frame)
     (generic-full-pathname *video-pathname*
			    video-name
			    "/human-annotation-statistics.sc")
     (generic-pathname video-name frame "human-annotation-statistics.sc")))

(define (verb-objects-pathname)
 (string-append
  (getenv "HOME") "/darpa-collaboration/documentation/verb-objects.sc"))

(define (mov-pathname video-name)
 (generic-full-pathname *video-pathname* video-name ".mov"))

(define (avi-pathname video-name)
 (generic-full-pathname *video-pathname* video-name ".avi"))

(define (mp4-pathname video-name)
 (generic-full-pathname *video-pathname* video-name ".mp4"))

(define (symlink-exists? filename)
 (equal? 0 (system (format #f "[ -L \"~a\" ]" filename))))

(define (video-exists? filename)
 (or (file-exists? (mov-pathname filename))
     (file-exists? (avi-pathname filename))
     (file-exists? (mp4-pathname filename))
     (symlink-exists? (mov-pathname filename))
     (symlink-exists? (avi-pathname filename))
     (symlink-exists? (mp4-pathname filename))))

(define (video-pathname filename)
 (cond ((file-exists? (mov-pathname filename)) (mov-pathname filename))
       ((file-exists? (avi-pathname filename)) (avi-pathname filename))
       ((file-exists? (mp4-pathname filename)) (mp4-pathname filename))
       ((symlink-exists? (mov-pathname filename)) (mov-pathname filename))
       ((symlink-exists? (avi-pathname filename)) (avi-pathname filename))
       ((symlink-exists? (mp4-pathname filename)) (mp4-pathname filename))
       (else
	(panic "Could not find any of the following files:~%+ ~a~%+ ~a~%+ ~a~% Do you have the symlink ~~/video-datasets pointing to the correct place?~%" (mov-pathname filename) (avi-pathname filename) (mp4-pathname filename)))))

(define (documentation-pathname filename)
 (string-append
  (getenv "HOME") "/darpa-collaboration/documentation/" filename))

(define (klt-pathname video-name)
 (generic-full-pathname *video-pathname* video-name "/klt.zip"))

;;; in-zip pathnames

(define (per-frame-optical-flow-in-zip-pathname frame)
 (format #f "~a/optical-flow.integral_flo" (number->padded-string-of-length frame 6)))

(define (per-frame-klt-in-zip-pathname frame)
 (format #f "~a/klt.text" (number->padded-string-of-length frame 6)))

(define (per-frame-boxes-in-zip-pathname frame type label num ext)
 (format #f "~a/~a-~a.~a"
	 (number->padded-string-of-length frame 6)
	 type
	 (if num (string*-append label "-" num) label)
	 ext))

(define (per-video-box-pathname video-name type label num ext)
 (generic-full-pathname
  *video-pathname* video-name
  (format #f "/~a-~a.~a"
	  type (if num (string*-append label "-" num) label)
	  (string-append ext ".zip"))))

;;; DARPA corpus

;; single verb only

;; Throw4_A2_C3_Act1_PARK3_ML_MIDD_DARK_44b04e02-c5af-11df-a1a7-e80688cb869a.mov
;;  actionSubtype_?_?_actor_background_location_time-of-day_uuid
;; Approach2_A1_C1_Act1_2_Park3_MC_AFTN_47ff4ac2-c5af-11df-a99b-e80688cb869a.mov

(define (string->darpa-lighting s)
 (let ((conditions
	(cond ((equal? (car s) "MIDD") 'midday)
	      ((equal? (car s) "AFTN") 'afternoon)
	      ((equal? (car s) "EVEN") 'evening)
	      ((equal? (car s) "MORN") 'morning)
	      ;; FIXME Perhaps this is just misspelled morning?
	      ((equal? (car s) "NORM") 'normal)
	      (else (fuck-up)))))
  (cond ((= (length s) 1) (list conditions))
	((= (length s) 2) (list conditions
				(cond ((equal? (cadr s) "DARK") 'dark)
				      (else (fuck-up)))))
	(else (fuck-up)))))

(define (darpa-lighting->string c)
 (let ((a (cond ((equal? (first c) 'midday) "MIDD")
		((equal? (first c) 'afternoon) "AFTN")
		((equal? (first c) 'evening) "EVEN")
		((equal? (first c) 'morning) "MORN")
		((equal? (first c) 'normal) "NORM")
		(else (fuck-up)))))
  (if (= (length c) 2)
      (string-append a "_" (cond ((equal? (second c) 'dark) "DARK")
				 (else (fuck-up))))
      a)))

(define *darpa-corpora* #f)
(define (load-darpa-corpora!)
 (set! *darpa-corpora*
       (map fields
	    (read-file
	     (string-append
	      (getenv "HOME")
	      "/darpa-collaboration/documentation/darpa-corpora.text" )))))

(define (video-name->darpa-video-corpus video)
 (unless *darpa-corpora* (load-darpa-corpora!))
 (let ((v (find-if (lambda (a)
                    (if (null? a)
                        #f
                        (equal? (second a) video))) *darpa-corpora*)))
  (if v (first v) v)))

;;; Returns a 'darpa' video (accessed through the ~/video-datasets
;;; symlink), or a stand-alone video (from a  fully pathed video name).
(define (load-darpa-video video-name)
 (let ((video
	(cond
	 ((video-name->darpa-video-corpus video-name) (string->darpa-video video-name))
	 (else (make-stand-alone-video video-name)))))
  (if (video-first-frame video)
      video
      (dtrace (format #f "Failed to load video: '~a'" video-name) '()))))

;; workaround for the insane darpa naming scheme
;; _DARK shouldn't be separated from the name of the 'lighting' conditions
;;  MIDD_DARK and MIDD are both time-of-day
;; Act can have arbitrary lists separated by underscores, like Act1_2
(define (string->darpa-video-from-corpus full-name corpus)
 (define (strict-string->number s)
  (unless (string->number s) (fuck-up))
  (string->number s))
 (let* ((name (strip-extension full-name))
	(name (pregexp-replace "_DARK" name "-DARK"))
	(name (pregexp-replace* "\(\[B0-9\]+_\)+|_" name "|\\0|"))
	(name (pregexp-split "\\|" name))
	(name (map (lambda (s) (pregexp-split "_" s)) name)))
  (make-darpa-video
   corpus
   (car (list-ref name 0))
   ;; note this intentionally can be #f
   ;; Since not all actions have sense numbers after them
   (car (list-ref name 1))
   (strict-string->number (car (list-ref name 3)))
   (strict-string->number (car (list-ref name 5)))
   (begin
    (cond ((equal? (list-ref name 6) '("ActV"))
	   (unless (= (length (list-ref name 7)) 1)
	    (fuck-up))
	   (map (lambda (s) (string-append "V" s)) (list-ref name 7)))
	  ((equal? (list-ref name 6) '("ActNA"))
	   (unless (equal? '("") (list-ref name 7))
	    (panic "ActNA with an actor!"))
	   "NA")
	  (else (list-ref name 7))))
   (list (car (list-ref name 8))
	 ;; note this intentionally can be #f
	 ;; Since not all backgrounds have numbers after them
	 (string->number (car (list-ref name 9))))
   (car (list-ref name 10))
   (string->darpa-lighting (pregexp-split "-" (car (list-ref name 12))))
   (car (list-ref name 14)))))

(define (string->darpa-video full-name)
 (string->darpa-video-from-corpus
  full-name
  (let ((corpus (video-name->darpa-video-corpus full-name)))
   (unless
     corpus
    (panic "Could not figure out the corpus for your DARPA video, see documentation/darpa-corpora.text"))
   corpus)))

(define (string->demo-video name)
 (let ((video (string->darpa-video name)))
  (set-darpa-video-corpus! video "demo")
  video))

(define (string->darpa-video-name name)
 (let ((v (string->darpa-video name)))
  (set-darpa-video-corpus! v "")
  v))

(define (list->darpa-string l) (if (car l) (string-join "_" l) ""))

(define (darpa-video->name video)
 ;; I'm old, I'm deprecated, don't call me
 (darpa-video->string video))

(define (darpa-video-verb video)
 (let ((l (string->list (darpa-video-action video))))
  (list->string (cons (char-upcase (first l)) (map char-downcase (rest l))))))

(define (darpa-video->string video)
 (format #f "~a~a_A~a_C~a_Act~a_~a~a_~a_~a_~a"
	 (darpa-video-action video)
	 (darpa-video-subtype video)
	 (darpa-video-unknown1 video)
	 (darpa-video-unknown2 video)
	 (if (string? (darpa-video-actors video))
	     (darpa-video-actors video)
	     (list->darpa-string (darpa-video-actors video)))
	 (first (darpa-video-background video))
	 (list->darpa-string (cdr (darpa-video-background video)))
	 (darpa-video-location video)
	 (darpa-lighting->string (darpa-video-time-of-day video))
	 (darpa-video-uuid video)))

(define (darpa-video-same-foreground? a b)
 (and (equal? (darpa-video-corpus a)
	      (darpa-video-corpus b))
      (equal? (darpa-video-verb a)
	      (darpa-video-verb b))
      (equal? (darpa-video-subtype a)
	      (darpa-video-subtype b))
      (equal? (darpa-video-actors a)
	      (darpa-video-actors b))
      (equal? (darpa-video-unknown1 a)
	      (darpa-video-unknown1 b))
      (equal? (darpa-video-unknown2 a)
	      (darpa-video-unknown2 b))
      (cond ((and (darpa-video-live-action? a) (darpa-video-live-action? b))
	     (and
	      (equal? (darpa-video-background a)
		      (darpa-video-background b))
	      (equal? (darpa-video-location a)
		      (darpa-video-location b))
	      ;; Only first since AFTERNOON DARK is still the same as AFTERNOON
	      (equal? (first (darpa-video-time-of-day a))
		      (first (darpa-video-time-of-day a)))))
	    ((and (darpa-video-composite? a) (darpa-video-composite? b)) #t)
	    (else #f))))

(define (darpa-video-partition-into-unique-foregrounds videos)
 (equivalence-classesp (lambda (a b) (darpa-video-same-foreground? a b)) videos))

;;; Closed splines

(define (read-number-fields-file filename)
 (map (lambda (l) (list->vector (map string->number (fields l)))) (read-file filename)))

(define (order-edges start l)
 (if (or (null? l) (= (length l) 1))
     (cons start l)
     (let ((e (next-edge start l)))
      (cons start (order-edges e (remove e l))))))

(define (order-cycle-edges cycle)
 (order-edges (first cycle) (cdr cycle)))

(define (flip-solid-edges-to-minimize-length l)
 (let ((flips (list->vector (map (lambda (a) #f) l))))
  (define (flip! i t) (vector-set! flips i t))
  (define (points)
   (map-reduce
    append
    '()
    (lambda (a b)
     (if b
	 (reverse a)
	 a))
    (map solid-edge-pixels l)
    (vector->list flips)))
  (for-each-indexed
   (lambda (p i)
    (flip! i #f)
    (let ((f (perimeter-of-polygon (points))))
     (flip! i #t)
     (let ((t (perimeter-of-polygon (points))))
      (if (<= f t)
	  (flip! i #f)
	  (flip! i #t)))))
   l)
  (map
   (lambda (e f)
    (if f
	(make-solid-edge (solid-edge-u e)
			 (solid-edge-v e)
			 (solid-edge-w1 e)
			 (solid-edge-w2 e)
			 (reverse (solid-edge-pixels e)))
	e))
   l
   (vector->list flips))))

(define (next-edge s l)
 (find-if (lambda (e)
	   (or (equal? (any-edge-v s) (any-edge-v e))
	       (equal? (any-edge-u s) (any-edge-v e))
	       (equal? (any-edge-v s) (any-edge-u e))
	       (equal? (any-edge-u s) (any-edge-u e))))
	  l))

(define (cycle->ordered-points cycle)
 (map-reduce append '() solid-edge-pixels
	     (flip-solid-edges-to-minimize-length
	      (remove-if dashed-edge? (order-cycle-edges cycle)))))

(define (fit-spline-to-cycle cycle knots)
 (fit-spline-to-ordered-points (cycle->ordered-points cycle) knots))

(define (spline-points->coordinates points)
 (let ((points (append (map-reduce append '() vector->list points))))
  (map vector
       (take (/ (length points) 2) points)
       (drop (/ (length points) 2) points))))

(define (fit-spline-to-ordered-points ordered-points knots)
 (let ((temp-filename (format #f "/tmp/~a~a" "darpa_fit_spline_" (getpid))))
  (matlab-matrix-output (list->vector ordered-points) (string-append temp-filename ".m"))
  (system (format #f "matlab -nodesktop -nosplash -r \"addpath('~~/darpa-collaboration/bsplines/'); read_fit_spline_and_write('~a',~a,'~a')\" < /dev/null > /dev/null"
		  (string-append temp-filename ".m")
		  knots
		  temp-filename))
  (let ((result (make-spline
		 (spline-points->coordinates
		  (read-number-fields-file (string-append temp-filename "-spline.coefs")))
		 (spline-points->coordinates
		  (read-number-fields-file (string-append temp-filename "-spline.points")))
		 (string->number (car (read-file (string-append temp-filename "-spline.error")))))))
   (rm (string-append temp-filename ".m"))
   (rm (string-append temp-filename "-spline.coefs"))
   (rm (string-append temp-filename "-spline.points"))
   (rm (string-append temp-filename "-spline.error"))
   result)))

;;; Open splines

(define (fit-spline-to-points points breaks degree samples)
 (let ((temp-filename (format #f "/tmp/~a~a" "darpa_fit_spline_" (getpid))))
  (matlab-matrix-output (list->vector points) (string-append temp-filename ".m"))
  (system (format #f "matlab -nodesktop -nosplash -r \"addpath('~~/darpa-collaboration/splines/'); read_fit_open_spline_and_write('~a',~a,~a,~a,'~a')\" < /dev/null" ;; > /dev/null"
		  (string-append temp-filename ".m")
		  breaks
		  degree
		  samples
		  temp-filename))
  (let ((result (list
		 (read-number-fields-file (string-append temp-filename "-spline.coefs"))
		 (spline-points->coordinates
		  (read-number-fields-file (string-append temp-filename "-spline.points"))))))
   (rm (string-append temp-filename ".m"))
   (rm (string-append temp-filename "-spline.coefs"))
   (rm (string-append temp-filename "-spline.points"))
   result)))

;; TODO ANDREI Convert me to the new directory layout
;; (define (merge-splines prefix size upto motion-threshold?)
;;  (map-n (lambda (frame)
;; 	 (let ((frame-str (number->padded-string-of-length frame 4))
;; 	       (images
;; 		(map
;; 		 first
;; 		 (remove-if
;; 		  null?
;; 		  (map-n
;; 		   (lambda (n)
;; 		    (if (or (not motion-threshold?)
;; 			    (second (fifth (second
;; 					    (read-object-from-file
;; 					     (format #f "~a-~a-1.0-~a.metadata"
;; 						     prefix
;; 						     frame-str
;; 						     (number->string n)))))))
;; 			(directory-list (format #f "~a~a-*-~a-spline.pgm"
;; 						prefix
;; 						frame-str
;; 						(number->string n)))
;; 			'()))
;; 		   upto)))))
;; 	  ;; debugging
;; 	  (write frame)(newline)
;; 	  (cond ((null? images) (format #t "Frame ~a, has no splines!~%" frame))
;; 		(else
;; 		 (system
;; 		  (format #f "cp ~a ~a"
;; 			  (first images)
;; 			  (format #f "~a-~a-~a-merged.pgm" prefix frame-str upto)))
;; 		 (foldl
;; 		  (lambda (a b)
;; 		   (system (format #f  "composite -compose plus ~a ~a ~a &>> /tmp/s~%" a b a)) a)
;; 		  images
;; 		  (format #f "~a-~a-~a-merged.pgm" prefix frame-str upto))))))
;; 	size))

;;; Polygons

(define (polygon-area points)
 (abs (/ (map-reduce
	  +
	  0
	  (lambda (p2 p1) (- (* (x p1) (y p2)) (* (x p2) (y p1))))
	  (cdr points) points)
	 2)))

(define (area-between-pixels u v)
 (polygon-area `(,`#(,(x u) 0)
		 ,u
		 ,v
		 ,`#(,(x v) 0)
		 ,`#(,(x u) 0))))

(define (compute-non-simple-polygon-area polygon)
 (let* ((polygon (map (lambda (p) `#(,(exact->inexact (x p)) ,(exact->inexact (y p)))) polygon))
	(edges (map (lambda (p2 p1) (make-line-segment p1 p2))
		    (cdr polygon) polygon)))
  (define (pixel->exact p) `#(,(inexact->exact (round (* 2 (x p))))
			      ,(inexact->exact (round (* 2 (y p))))))
  (define (line-segment->exact l) (make-line-segment
				   (pixel->exact (line-segment-p l))
				   (pixel->exact (line-segment-q l))))
  (map-reduce3
   +
   0
   (lambda (p2 p1)
    (let* ((median-point `#(,(/ (+ (x p2) (x p1)) 2) ,(/ (+ (y p2) (y p1)) 2)))
	   (test-line (make-line-segment median-point `#(,(x median-point) 9999999.)))
	   (crossings (length
		       (remove-if
			(lambda (p) (or (not p) (<= (y p) (y median-point))))
			(map (lambda (e)
			      (let ((w (intersection-point test-line e)))
			       (if (and w
					(point-on-line-segment? (pixel->exact w) (line-segment->exact e))
					(point-on-line-segment? (pixel->exact w) (line-segment->exact test-line)))
				   w
				   #f)))
			     (remove (make-line-segment p1 p2) edges))))))
     (* (abs(area-between-pixels p1 p2)) (expt -1 crossings))))
   (cdr polygon)
   polygon)))

(define (polygon-op operation polygons)
 (let ((temp-filename (format #f "/tmp/~a~a" "darpa_polygon_" (getpid))))
  (rm (string-append temp-filename "-*-polygon.points"))
  (for-each-indexed
   (lambda (p i)
    (matlab-matrix-output (list->vector p) (string-append temp-filename (number->string i) ".m")))
   polygons)
  (system (format #f "matlab -nodesktop -nosplash -r \"addpath('~~/darpa-collaboration/polygons/'); read_polygon_op_and_write('~a',~a,~a,'~a')\" < /dev/null" ;;> /dev/null"
		  temp-filename
		  (length polygons)
		  (cond ((equal? operation 'difference) 0)
			((equal? operation 'and) 1)
			((equal? operation 'xor) 2)
			((equal? operation 'union) 3)
			(else (fuck-up)))
		  temp-filename))
  (for-each-n
   (lambda (n) (rm (string-append temp-filename (number->string n) ".m")))
   (length polygons))
  (map (lambda (filename) (let ((result (read-file filename)))
		      (rm filename)
		      `(,(= 0 (string->number (first result))) ;; not-empty?
			,(map (lambda (a b) (vector (string->number a) (string->number b)))
			      (take (/ (length (cdr result)) 2) (cdr result))
			      (drop (/ (length (cdr result)) 2) (cdr result))))))
       (directory-list (string-append temp-filename "-*-polygon.points")))))

(define (polygon-union p1 p2 . p3) (polygon-op 'union (cons p1 (cons p2 p3))))
(define (polygon-and p1 p2 . p3) (polygon-op 'and (cons p1 (cons p2 p3))))
(define (polygon-xor p1 p2 . p3) (polygon-op 'xorg (cons p1 (cons p2 p3))))
(define (polygon-difference p1 p2 . p3) (polygon-op 'difference (cons p1 (cons p2 p3))))

(define (polygon->pbm polygon h w)
 (points->pbm-of-size
  (map
   quantize-point
   (map-reduce
    append
    '()
    (lambda (p2 p1) (line-segment->points (make-line-segment p1 p2)))
    (cdr (close-polygon polygon))
    (close-polygon polygon)))
  h
  w))

(define (polygons-minimum-distance-points polygon1 polygon2)
 (let ((d `(,(distance (car polygon1) (car polygon2))
	    ,(car polygon1)
	    ,(car polygon2))))
  (for-each
   (lambda (p1)
    (for-each
     (lambda (p2)
      (when (< (distance p1 p2) (car d))
       (set! d `(,(distance p1 p2) ,p1 ,p2))))
     polygon2))
   polygon1)
  (cdr d)))

(define (close-polygon polygon) (cons (last polygon) polygon))

(define (line-segment-intersect? l1 l2)
 (let ((point (intersection-point l1 l2)))
  (and point
       (or (<= (x (q l1)) (x point) (x (p l1)))
	   (>= (x (q l1)) (x point) (x (p l1))))
       (or (<= (x (q l2)) (x point) (x (p l2)))
	   (>= (x (q l2)) (x point) (x (p l2)))))))

(define (join-non-overlapping-polygons-by-bridging-area polygon1 polygon2)
 (let* ((pa (polygons-minimum-distance-points polygon1 polygon2))
	(pb (polygons-minimum-distance-points (remove (first pa) polygon1)
					      (remove (second pa) polygon2)))
	(p1a (first pa))
	(p1b (first pb))
	(p2a (second pa))
	(p2b (second pb))
	(r1a (ring-forward-between polygon1 p1a p1b))
	(r1b (ring-forward-between polygon1 p1b p1a))
	(r2a (ring-forward-between polygon2 p2a p2b))
	(r2b (ring-forward-between polygon2 p2b p2a)))
  (second
   (maximump
    (map-reduce
     append
     '()
     (lambda (a) (map (lambda (b)
		  `(,(if (line-segment-intersect? (make-line-segment (first a) (last b))
						  (make-line-segment (last a) (first b)))
			 (- infinity)
			 (length (append a b)))
		    ,(append a b)))
		 `(,r2a ,r2b ,(reverse r2a) ,(reverse r2b))))
     `(,r1a ,r1b ,(reverse r1a) ,(reverse r1b)))
    first))))

(define (join-non-overlapping-polygons-by-quadrilateral polygon1 polygon2)
 (define (union p1 p2)
  (let ((r (second (polygon-union p1 p2))))
   (unless (= (length r) 1) (fuck-up))
   (second (first r))))
 (let* ((p1 (polygons-minimum-distance-points polygon1 polygon2))
	(p2 (polygons-minimum-distance-points (remove (first p1) polygon1)
					      (remove (second p1) polygon2))))
  (union
   (union `(,(first p1) ,(second p1) ,(second p2) ,(first p2)) polygon1)
   polygon2)))

(define (union-n-polygons polygons)
 (remove-if
  (lambda (p)
   (and (< (length p) 5) (< (compute-non-simple-polygon-area (close-polygon p)) 2)))
  (map (lambda (p) (map quantize-point (second p))) (polygon-op 'union polygons))))

(define (union-n-polygons-by-bridging polygons)
 (let loop ((polygons (union-n-polygons polygons)))
  (cond ((< (length polygons) 2) polygons)
  	(else
	 (loop (cons
		(join-non-overlapping-polygons-by-bridging-area (first polygons) (second polygons))
		(drop 2 polygons)))))))

(define (find-closest-polygon polygon polygons)
 (minimump
  polygons
  (lambda (p)
   (let ((points (polygons-minimum-distance-points polygon p)))
    (distance (first points) (second points))))))

(define (union-n-polygons-by-bridging-closest polygons)
 (let loop ((polygons (union-n-polygons polygons)))
  (cond ((< (length polygons) 2) polygons)
  	(else
	 (let ((closest (find-closest-polygon (car polygons) (cdr polygons))))
	  (loop (cons
		 (join-non-overlapping-polygons-by-bridging-area (first polygons) closest)
		 (removeq closest (cdr polygons)))))))))

(define (union-n-polygons-by-bridging-closest-keep polygons)
 (let loop ((polygons (union-n-polygons polygons)))
  (cond ((< (length polygons) 2) polygons)
  	(else
	 (let* ((closest (find-closest-polygon (car polygons) (cdr polygons)))
		(result (join-non-overlapping-polygons-by-bridging-area (first polygons) closest)))
	  (list (list result (first polygons) closest)
		(loop (cons result (removeq closest (cdr polygons))))))))))

(define (join-rc-cycles cycles)
 (union-n-polygons-by-bridging-closest (map cycle->ordered-points cycles)))

(define (read-cycles-from-frame video-name frame upto)
 (remove
  #f
  (map-n
   (lambda (i)
    (let ((metadata (motion-window-rc-metadata-pathname video-name frame "1.0" i)))
     (if (and (file-exists? metadata)
	      (second (find-if (lambda (a) (equal? (first a) 0.25))
			       (second (read-object-from-file metadata)))))
	 (read-cycle-chains-from-file
	  (read-object-from-file (motion-window-rc-chains-pathname video-name frame "1.0" i))
	  (motion-window-rc-cycle-pathname video-name frame "1.0" i))
	 #f)))
   upto)))

(define (merge-rc-cycles video-name frame upto)
 (let ((cycles (read-cycles-from-frame video-name frame upto)))
  (join-rc-cycles cycles)))

(define (points-in-cycle points cycle)
 (define (read-matlab-inpoints file)
  (define (read-matlab-inpoints port)
   (let loop ((l '()))
    (let ((line (read-line port)))
     (if (eof-object? line)
	 (reverse l)
	 (loop (cons (string->number line) l))))))
  (call-with-input-file file read-matlab-inpoints))
 (define (matlab-external-command command)
  (system (format #f "matlab -nodesktop -nosplash -r \"~a\" < /dev/null" command)))
 (let ((pid (number->string (getpid))))
  (matlab-matrix-output (list->vector (cycle->ordered-points cycle))
			(string-append "/tmp/cycle-points-" pid ".m"))
  (matlab-matrix-output (list->vector points)
			(string-append "/tmp/test-points-" pid ".m"))
  (matlab-external-command
   (string-append
    "cpoints = importdata('/tmp/cycle-points-" pid ".m',' '); "
    "tpoints = importdata('/tmp/test-points-" pid ".m',' '); "
    "[inpoints, onpoints] = inpolygon(tpoints(:,1),tpoints(:,2),cpoints(:,1),cpoints(:,2)); "
    "file1 = fopen('/tmp/inpoints-" pid "','w'); for i = 1:length(inpoints); fprintf(file1, '%d\\n',inpoints(i)); end; fclose(file1); "
    "file2 = fopen('/tmp/onpoints-" pid "','w'); for i = 1:length(onpoints); fprintf(file2, '%d\\n',onpoints(i)); end; fclose(file2); "
    "quit;"))
  (let ((result (remove-if boolean? (map (lambda (p in on) (if (and (zero? in) (zero? on)) #f p))
					 points
					 (read-matlab-inpoints (string-append "/tmp/inpoints-" pid))
					 (read-matlab-inpoints (string-append "/tmp/onpoints-" pid))))))
   (system
    (format #f "cd /tmp; rm inpoints-~a onpoints-~a cycle-points-~a.m test-points-~a.m" pid pid pid pid))
   result)))

;;; Writing out coefficients

(define (write-coefficients-to-file video-name)
 (call-with-output-file "/tmp/coefficients.text"
  (lambda (port)
   (for-each-n
    (lambda (frame)
     (if (file-exists?
	  (generic-pathname video-name
			    frame "joined-polygons-spline.coefficients"))
	 (for-each
	  (lambda (p) (format #t "~a  ~a" (x (quantize-point p)) (y (quantize-point p))))
	  (spline-points->coordinates
	   (read-object-from-file
	    (generic-pathname video-name
			      frame "joined-polygons-spline.coefficients")))))
     (format port "~%"))
    2))))

(define (write-24bit-coefficients-to-file video-name)
 (call-with-output-file "/tmp/coefficients.text"
  (lambda (port)
   (for-each-frame
    (lambda (frame)
     (if (file-exists?
	  (generic-pathname video-name frame "joined-polygons-spline.coefficients"))
	 (for-each
	  (lambda (p)
	   (write-24-bit-integer (+ (x (quantize-point p)) (bit-lsh (y (quantize-point p)) 9))
				 port))
	  (drop-if-possible
	   3
	   (spline-points->coordinates
	    (read-object-from-file
	     (generic-pathname video-name frame "joined-polygons-spline.coefficients")))))))
    video-name))))

(define (write-32bit-coefficients-to-file video-name)
 (call-with-output-file "/tmp/coefficients.text"
  (lambda (port)
   (for-each-frame
    (lambda (frame)
     (if (file-exists?
	  (generic-pathname video-name
			    frame "joined-polygons-spline.coefficients"))
	 (for-each
	  (lambda (p) (write-16-bit-integer (x (quantize-point p)) port)
	     (write-16-bit-integer (y (quantize-point p)) port))
	  (drop-if-possible
	   3
	   (read-object-from-file
	    (generic-pathname video-name
			      frame "joined-polygons-spline.coefficients")))))
     (format port "~%"))
    video-name))))

(define (show-polygon polygon)
 (let ((polygon (close-polygon polygon))
       (run-file "/tmp/matlabgraph.m"))
  (matlab-matrix-output (list->vector polygon) "/tmp/test.m")
  (rm run-file)
  (call-with-output-file "/tmp/matlabgraph.m"
   (lambda (port)
    (format port "T = importdata('/tmp/test.m', ' ');~%")
    (format port "X = T(:,1);~%")
    (format port "Y = T(:,2);~%")
    (format port "plot(X,Y);~%")
    (format port "set(gca,'YDir','reverse')~%")
    (format port "axis([0 320 0 240])~%")))
  (system (format #f "(cd ~a;matlab -r ~a -nodesktop -nosplash < /dev/ttyS1 &> /dev/null) &"
		  (directory run-file)
		  (strip-directory (strip-extension run-file))))))

(define (cycle->dense-points cycle)
 (reduce
  append
  (map (lambda (p2 p1)
	(quantize-points (line-segments->points (list (make-line-segment p1 p2)))))
       (cdr (close-polygon cycle))
       (close-polygon cycle))
  '()))

;;; GUI

(define (draw-clickable-pixmap-from-pnm pixmap pnm x y scale handler)
 (draw-pixmap pixmap x y)
 (define-region x y (* (pnm-width pnm) scale) (* (pnm-height pnm) scale)
  (lambda (x1 y1)
   (let ((x1 (quantize-coordinate (/ x1 scale)))
	 (y1 (quantize-coordinate (/ y1 scale))))
    (when (pnm-pixel? pnm (- x1 x) (- y1 y))
     (handler (- x1 x) (- y1 y)))))))

(define (define-spinner-buttons c r name f-up f-down f-print)
 (define-button c r (string-append "-  " name) #f
  (lambda () (message "")
     (f-down)
     (redraw-buttons)))
 (define-button (+ c 1) r (lambda () (string-append (f-print) "  +")) #f
  (lambda () (message "")
     (f-up)
     (redraw-buttons))))

;;; Generic IO

;;; Why, you ask? This format can be read by a custom OpenCV
;;; function--need to create the corresponding read function.
(define (matrix->flatfile mat file)
 (call-with-output-file file
  (lambda (port)
   (for-each-n
    (lambda(i)
     (for-each-n
      (lambda(j)
       (format port "~a" (matrix-ref mat i j ))
       (if (not (= j (- (matrix-columns mat) 1)))
	   (format port " " )))
      (matrix-columns mat))
     (if (not (= i (- (matrix-rows mat) 1)))
	 (format port "~%")))
    (matrix-rows mat)))))

;;; Curl

(define (uuid)
 (let ((t (tmp (string-append "uuid-" (number->string (getpid))))))
  (system (format #f "uuidgen |perl -p -i -e 's/-//g' &> ~a" t))
  (first (read-file t))))

(define (curl useragent forms url)
 (let ((t (tmp (string-append "curl-" (number->string (getpid))))))
  (system (format #f "curl -A '~a' ~a ~a &> ~a"
		  useragent
		  (string-join " " (map (lambda (a)(string-append "-F '" (first a) "=" (second a) "'")) forms))
		  url
		  t))
  (read-file t)))

;;; Optical flow
;;; http://perception.inrialpes.fr/~chari/myweb/Software/
;;; High accuracy optical flow estimation based on a theory for warping
;;; Thomas Brox, Andrés Bruhn, Nils Papenberg, Joachim Weickert
;;; European Conference on Computer Vision (ECCV), Prague, Czech Republic, May 2004.

(define (read-flo-from-file-to-scheme filename)
 (with-file-stream read-flo-from-stream-to-scheme filename "r"))

(define (read-flo-from-buffer-to-scheme buffer size)
 (with-buffer-stream read-flo-from-stream-to-scheme buffer size "r"))

(define (read-flo-from-stream-to-scheme file)
 (let* ((size-ptr
	 ((c-function pointer ("read_flo_size_from_stream" pointer)) file))
	(size (vector (c-int-ref size-ptr 0) (c-int-ref size-ptr 4)))
	(matrix-ptr
	 ((c-function pointer ("read_flo_from_stream" string)) file)))
  (let ((xs (map-n-vector
	     (lambda (ho)
	      (map-n-vector
	       (lambda (wo) (c-float-ref matrix-ptr
					 (* (+ (* (+ (* ho (x size)) wo) 2) 0) 4)))
	       (x size)))
	     (y size)))
	(ys (map-n-vector
	     (lambda (ho)
	      (map-n-vector
	       (lambda (wo) (c-float-ref matrix-ptr
					 (* (+ (* (+ (* ho (x size)) wo) 2) 1) 4)))
	       (x size)))
	     (y size))))
   (free matrix-ptr)
   (free size-ptr)
   (list xs ys))))

(define (read-flo-from-file filename)
 (let ((size (with-file-stream read-flo-size-from-stream filename "r")))
  (with-file-stream (lambda (file) (read-flo-from-stream file size)) filename "r")))

(define (read-flo-from-buffer buffer buffer-size)
 (let ((size (with-buffer-stream read-flo-size-from-stream
				 buffer buffer-size "r")))
  (with-buffer-stream (lambda (file) (read-flo-from-stream file size))
		      buffer buffer-size "r")))

(define (read-flo-size-from-stream file)
 (let* ((size-ptr
	 ((c-function pointer ("read_flo_size_from_stream" pointer)) file))
	(size (vector (c-int-ref size-ptr 0) (c-int-ref size-ptr 4))))
  (free size-ptr)
  size))

(define (read-flo-from-stream file size)
 (make-c-optical-flow ((c-function pointer ("read_flo_from_stream" pointer)) file)
		      (x size) (y size)))

(define (write-flo-to-file flow filename)
 (with-file-stream (lambda (file) (write-flo-to-stream flow file))
		   filename "w"))

(define (write-flo-to-buffer flow buffer size)
 (with-buffer-stream (lambda (file) (write-flo-to-stream flow file)) buffer size "w"))

(define (write-flo-to-stream flow file)
 ((c-function void ("write_flo_to_stream" pointer unsigned unsigned pointer))
  (c-optical-flow-handle flow) (c-optical-flow-width flow)
  (c-optical-flow-height flow) file))

(define (integral-optical-flow-from-c optical-flow width height)
 ((c-function pointer ("integral_optical_flow" pointer unsigned unsigned))
  optical-flow height width))

(define c-integral-optical-flow-area
 (c-function double ("integral_optical_flow_area"
		     pointer unsigned unsigned
		     unsigned unsigned unsigned unsigned)))

;; Note that this internally clips to the border of the image
(define (average-integral-optical-flow-in-c
	 optical-flow x1 y1 x2 y2)
 (k*v 2 (vector
	 (/ ((c-function double ("integral_optical_flow_area"
				 pointer unsigned unsigned
				 unsigned unsigned unsigned unsigned))
	     (c-optical-flow-handle optical-flow)
	     (c-optical-flow-height optical-flow)
	     (c-optical-flow-width  optical-flow)
	     x1 y1 x2 y2)
	    (* (+ (- x2 x1) 1) (+ (- y2 y1) 1)))
	 (/ ((c-function double ("integral_optical_flow_area"
				 pointer unsigned unsigned
				 unsigned unsigned unsigned unsigned))
	     (+ (c-optical-flow-handle optical-flow)
		(* (c-optical-flow-height optical-flow)
		   (c-optical-flow-width  optical-flow)
		   c-sizeof-double))
	     (c-optical-flow-height optical-flow)
	     (c-optical-flow-width  optical-flow)
	     x1 y1 x2 y2)
	    (* (+ (- x2 x1) 1) (+ (- y2 y1) 1))))))

(define (average-optical-flow-non-integral-c
	 ssv height width x1 y1 x2 y2)
 (let ((v ((c-function pointer ("average_optical_flow_ssv_from_c"
				pointer unsigned unsigned
				unsigned unsigned unsigned unsigned))
	   ssv height width x1 x2 y1 y2)))
  (vector (c-double-ref v 0) (c-double-ref v c-sizeof-double))))

(define (average-flow-in-box box flow-transformation)
 (k*v 2 (average-integral-optical-flow-in-c
	 flow-transformation
	 (bound (exact-round (/ (voc4-detection-x1 box) 2)) 0
		(- (c-optical-flow-width flow-transformation) 1))
	 (bound (exact-round (/ (voc4-detection-y1 box) 2)) 0
		(- (c-optical-flow-height flow-transformation) 1))
	 (bound (exact-round (/ (voc4-detection-x2 box) 2)) 0
		(- (c-optical-flow-width flow-transformation) 1))
	 (bound (exact-round (/ (voc4-detection-y2 box) 2)) 0
		(- (c-optical-flow-height flow-transformation) 1)))))

(define (integral-matrix-rectangle i x1 y1 x2 y2)
 (+ (matrix-ref i y1 x1)
    (- (safe-matrix-ref i y1 (+ x2 1) 0))
    (- (safe-matrix-ref i (+ y2 1) x1 0))
    (safe-matrix-ref i (+ y2 1) (+ x2 1) 0)))

(define (optical-flow image-prev-filename image-next-filename)
 (matlab "addpath('~/darpa-collaboration/optical-flow/sand/')")
 (matlab (format #f "[u, v] = optic_flow_sand(imread('~a'),imread('~a'));"
		 image-prev-filename
		 image-next-filename))
 (list (matlab-get-variable "u")
       (matlab-get-variable "v")))

(define (average-optical-flow transformation x-low x-high y-low y-high)
 ;; The Matlab code has no scaling but we believe it should be scaled by 2.0
 ;; since the optical flow is downsampled by a factor of 2.0.
 (k*v 2.0
      (map-vector /
		  (integral-matrix-rectangle transformation
					     x-low x-high y-low y-high)
		  (vector (- x-high x-low) (- y-high y-low)))))

(define (show-quiver u v)
 (scheme->matlab! "u" u)
 (scheme->matlab! "v" v)
 (matlab "quiver(u,v,0)"))

(define (read-optical-flow-in-c video-name frame)
 (with-zip-file
  (lambda (zip)
   (let ((buffer (zip:read-file-to-buffer
		  zip
		  (per-frame-optical-flow-in-zip-pathname frame))))
    (read-flo-from-buffer (x buffer) (y buffer))))
  (optical-flow-pathname video-name)
  *zip:mode-open*))

(define (read-optical-flow-movie-in-c video-name)
 (with-zip-file
  (lambda (zip)
   (map-frame-but-last
    (lambda (frame)
     (let ((buffer (zip:read-file-to-buffer
		    zip
		    (per-frame-optical-flow-in-zip-pathname frame))))
      (read-flo-from-buffer (x buffer) (y buffer))))
    video-name))
  (optical-flow-pathname video-name)
  *zip:mode-open*))

;;; reading mat files in C
;;; http://www.mathworks.com/help/techdoc/apiref/bqoqnz0.html#bqoqobe-1
(define (read-mat-double-variable pathname name)
 (let ((c-threshold
	((c-function pointer ("read_mat_double_variable" string string))
	 pathname name)))
  (let ((threshold (if (zero? c-threshold) #f (c-double-ref c-threshold 0))))
   (free c-threshold)
   threshold)))

(define (read-mat-double-from-struct pathname struct-name field-name)
 (let ((c-threshold
	((c-function pointer ("read_mat_double_from_struct" string string string))
	 pathname struct-name field-name)))
  (let ((threshold (if (zero? c-threshold) #f (c-double-ref c-threshold 0))))
   (free c-threshold)
   threshold)))

(define (read-voc4-model-threshold model-pathname)
 (unless (file-exists? model-pathname)
  (panic "read-voc4-model-threshold: File doesn't exist"))
 (let ((v1 (read-mat-double-from-struct model-pathname "model" "thresh"))
       (v2 (read-mat-double-from-struct model-pathname "csc_model" "thresh")))
  (cond ((and v1 v2)
	 (format #t "read-voc4-model-threshold: Threshold exists for both types~%"))
	(v1 v1)
	(v2 v2)
	(else
	 (format #t "read-voc4-model-threshold: Threshold exists for neither type~%")))))

;;; Bits & Bytes

(define (read-32-bit-integer port)
 (let* ((a (char->integer (read-char port)))
	(b (bit-lsh (char->integer (read-char port)) 8))
	(c (bit-lsh (char->integer (read-char port)) 16))
	(d (bit-lsh (char->integer (read-char port)) 24)))
  (+ a b c d)))

(define (read-16-bit-integer port)
 (let* ((a (char->integer (read-char port)))
	(b (bit-lsh (char->integer (read-char port)) 8)))
  (+ a b)))

(define (write-32-bit-integer i port)
 (write-char (integer->char (bit-and i 255)) port)
 (write-char (integer->char (bit-and (bit-rsh i 8) 255)) port)
 (write-char (integer->char (bit-and (bit-rsh i 16) 255)) port)
 (write-char (integer->char (bit-and (bit-rsh i 24) 255)) port))

(define (write-16-bit-integer i port)
 (write-char (integer->char (bit-and i 255)) port)
 (write-char (integer->char (bit-and (bit-rsh i 8) 255)) port))

(define (write-24-bit-integer i port)
 (write-char (integer->char (bit-and i 255)) port)
 (write-char (integer->char (bit-and (bit-rsh i 8) 255)) port)
 (write-char (integer->char (bit-and (bit-rsh i 16) 255)) port))

;;; imlib2
;;; TODO wrap up handles, add ffi support

(define imlib-load-image!
 (c-function imlib-image ("imlib_load_image" string)))
(define imlib-load-image-immediately!
 (c-function imlib-image ("imlib_load_image_immediately" string)))
(define imlib-get-data-ptr-read-only
 (c-function pointer ("imlib_image_get_data_for_reading_only")))
(define imlib-context-set-display!
 (c-function void ("imlib_context_set_display" pointer)))
(define imlib-context-set-visual!
 (c-function void ("imlib_context_set_visual" pointer)))
(define imlib-context-set-colormap!
 (c-function void ("imlib_context_set_colormap" int)))
(define imlib-context-set-drawable!
 (c-function void ("imlib_context_set_drawable" int)))
(define imlib-context-set-image!
 (c-function void ("imlib_context_set_image" imlib-image)))
(define imlib-context-set-color!
 (c-function void ("imlib_context_set_color" int int int int)))
(define imlib-render-image-on-drawable
 (c-function void ("imlib_render_image_on_drawable" int int)))
(define imlib-image-draw-pixel
 (c-function pointer ("imlib_image_draw_pixel" int int int)))
(define imlib-image-set-has-alpha!
 (c-function void ("imlib_image_set_has_alpha" int)))
(define imlib-image-draw-rectangle
 (c-function void ("imlib_image_draw_rectangle" int int int int)))
(define imlib-save-image
 (c-function void ("imlib_save_image" string)))
(define imlib-blend-image-onto-image
 (c-function void ("imlib_blend_image_onto_image" imlib-image int int int int int int int int int)))
(define imlib-create-cropped-scaled-image
 (c-function imlib-image ("imlib_create_cropped_scaled_image" int int int int int int)))
;; The memory for this image is uninitialized!
(define imlib-create-image
 (c-function imlib-image ("imlib_create_image" int int)))
(define imlib-clone-image
 (c-function imlib-image ("imlib_clone_image")))
(define imlib-free-image
 (c-function void ("imlib_free_image")))
(define imlib-free-image-and-decache
 (c-function void ("imlib_free_image_and_decache")))
(define imlib-get-image-width
 (c-function int ("imlib_image_get_width")))
(define imlib-get-image-height
 (c-function int ("imlib_image_get_height")))
(define imlib-image-set-format
 (c-function void ("imlib_image_set_format" string)))
(define imlib-load-font!
 (c-function pointer ("imlib_load_font" string)))
(define imlib-context-set-font!
 (c-function void ("imlib_context_set_font" pointer)))
(define imlib-text-draw
 (c-function void ("imlib_text_draw" int int pointer)))
(define imlib-get-text-dimension
 (c-function pointer ("imlib_get_text_dimension" string)))
(define imlib-free-font
 (c-function void ("imlib_free_font")))
(define imlib-add-path-to-font-path
 (c-function void ("imlib_add_path_to_font_path" string)))
;;; Despite the claims of the documentation imlib will never recreate
;;; GCs, even when the display changes. It must be disconnected
;;; first. It's safe to call disconnect when not connected to a
;;; display.
(define imlib-context-disconnect-display
 (c-function void ("imlib_context_disconnect_display")))
(define (imlib-image-draw-line x1 y1 x2 y2)
 ((c-function pointer ("imlib_image_draw_line" int int int int int))
  x1 y1 x2 y2 0))
(define imlib-image-fill-rectangle
 (c-function void ("imlib_image_fill_rectangle" int int int int)))
(define imlib-image-draw-ellipse
 (c-function void ("imlib_image_draw_ellipse" int int int int)))
(define imlib-image-fill-ellipse
 (c-function void ("imlib_image_fill_ellipse" int int int int)))
(define imlib-image-fill-polygon
 (c-function void ("imlib_image_fill_polygon" pointer)))
(define imlib-polygon-free
 (c-function void ("imlib_polygon_free" pointer)))
(define imlib-polygon-add-point
 (c-function void ("imlib_polygon_add_point" pointer int int)))
(define imlib-polygon-new
 (c-function pointer ("imlib_polygon_new")))
(define imlib-image-flip-vertical
 (c-function void ("imlib_image_flip_vertical")))
(define imlib-context-set-direction
 (c-function void ("imlib_context_set_direction" int)))
(define imlib-context-set-angle
 (c-function void ("imlib_context_set_angle" double)))

(define imlib-text-to-right (c-value int "IMLIB_TEXT_TO_RIGHT"))
(define imlib-text-to-left (c-value int "IMLIB_TEXT_TO_LEFT"))
(define imlib-text-to-down (c-value int "IMLIB_TEXT_TO_DOWN"))
(define imlib-text-to-up (c-value int "IMLIB_TEXT_TO_UP"))
(define imlib-text-to-angle (c-value int "IMLIB_TEXT_TO_ANGLE"))

(define (draw-imlib-pixmap image x y)
 (imlib-context-set-display! (cdr *display*))
 (imlib-context-set-visual! (cdr (xdefaultvisual *display* *screen*)))
 (imlib-context-set-colormap! (xdefaultcolormap *display* *screen*))
 (imlib-context-set-drawable! *display-pane*)
 (imlib-context-set-image! image)
 (imlib-render-image-on-drawable x y))

;;; Rendering boxes on images

(define (bloat-box box p)
 (let ((del-w (exact-round (* p (- (third box) (first box)))))
       (del-h (exact-round (* p (- (fourth box) (second box))))))
  `(,(- (first box) del-w) ,(- (second box) del-h)
    ,(+ (third box) del-w) ,(+ (fourth box) del-h)
    (fifth box) (sixth box))))

(define (grow-box box max-height max-width)
 `(,((if (= (first box) 0) + -) (first box) 1)
   ,((if (= (second box) 0) + -) (second box) 1)
   ,((if (= (third box) (- max-width 1)) - +) (third box) 1)
   ,((if (= (fourth box) (- max-height 1)) - +) (fourth box) 1)))

(define (grow-contour points max-height max-width)
 (pbm->points
  (pbm-bloat (points->pbm points max-height max-width) 1)))

(define (draw-imlib-circle image center radius colour fill?)
 (imlib-context-set-image! image)
 (imlib-context-set-color! (x colour) (y colour) (z colour) 255)
 (if fill?
     (imlib-image-fill-ellipse (x center) (y center) radius radius)
     (imlib-image-draw-ellipse (x center) (y center) radius radius)))

(define (draw-imlib-line image pt1 pt2 thickness colour)
 ;; Note: if the thickness is greater than the lenght of the line, then it's
 ;; set to the lenght of the line (essentially draing a square) however, the
 ;; minimum thickness is 1
 (imlib-context-set-image! image)
 (imlib-context-set-color! (x colour) (y colour) (z colour) 255)
 (let* ((v (vector (- (x pt2) (x pt1)) (- (y pt2) (y pt1))))
	(v-magnitude (magnitude v))
	(thickness (max 1 (floor (if (> thickness v-magnitude)
				     v-magnitude thickness)))))
  (if (= thickness 1)
      (imlib-image-draw-line (x pt1) (y pt1) (x pt2) (y pt2))
      (let* ((polygon-context (imlib-polygon-new))
	     (perp-v (vector (- (y v)) (x v)))
	     (half-thick (/ thickness 2))
	     (unit-perp-v (vector (/ (x perp-v) v-magnitude)
				  (/ (y perp-v) v-magnitude)))
	     (scaled-perp-v (vector (* half-thick (x unit-perp-v))
				    (* half-thick (y unit-perp-v)))))
       (imlib-polygon-add-point polygon-context
				(- (x pt1) (x scaled-perp-v))
				(- (y pt1) (y scaled-perp-v)))
       (imlib-polygon-add-point polygon-context
				(+ (x pt1) (x scaled-perp-v))
				(+ (y pt1) (y scaled-perp-v)))
       (imlib-polygon-add-point polygon-context
				(+ (x pt2) (x scaled-perp-v))
				(+ (y pt2) (y scaled-perp-v)))
       (imlib-polygon-add-point polygon-context
				(- (x pt2) (x scaled-perp-v))
				(- (y pt2) (y scaled-perp-v)))
       (imlib-image-fill-polygon polygon-context)
       (imlib-polygon-free polygon-context)))))

(define (draw-imlib-rectangle image box colour)
 (imlib-context-set-image! image)
 (imlib-context-set-color! (x colour) (y colour) (z colour) 255)
 (imlib-image-draw-rectangle
  (exact-round (first box)) (exact-round (second box))
  (exact-round (- (third box) (first box)))
  (exact-round (- (fourth box) (second box)))))

(define (voc4-detection->coordinates box)
 (list (voc4-detection-x1 box) (voc4-detection-y1 box)
       (voc4-detection-x2 box) (voc4-detection-y2 box)))

(define (draw-box-on-image image box thickness colour)
 ;; Assumes that box is scaled correctly
 (let loop ((b box) (t 0))
  (when (< t thickness)
   (draw-imlib-rectangle image b colour)
   (loop (grow-box b (imlib-get-image-height) (imlib-get-image-width)) (+ t 1)))))


(define (*default-font-path*)
 (cond ((file-exists? "/usr/share/fonts/truetype/ttf-dejavu")
	"/usr/share/fonts/truetype/ttf-dejavu")
       ((file-exists? "/usr/share/fonts/dejavu")
	"/usr/share/fonts/dejavu")
       (else (panic "Can't find a font directory"))))

(define (draw-box-name-on-image image box name colour font-size)
 (imlib-context-set-image! image)
 (imlib-add-path-to-font-path (*default-font-path*))
 (let ((font (imlib-load-font! (string*-append "DejaVuSans/" font-size))))
  (imlib-context-set-font! font)
  (imlib-context-set-color! (x colour) (y colour) (z colour) 255)
  (imlib-text-draw (exact-round (voc4-detection-x1 box))
		   (exact-round (voc4-detection-y1 box)) name)
  (imlib-free-font)))

(define (grade-colour colour gradation)
 (let* ((hsv (rgb->hsv colour))
	(hsv-graded `#(,(vector-ref hsv 0)
		       ,(vector-ref hsv 1)
		       ,(* (vector-ref hsv 2) (- 1.0 gradation)))))
  (hsv->rgb hsv-graded)))

(define (render-sentence-frames video sentence frame-name rank-box-colors?)
 (for-each-imlib-frame-from-video-indexed-but-last
  (lambda (frame index image)
   (unless *quiet-mode?*
    (format #t "Rendering caption frame ~a~%" frame))
   (imlib-context-set-image! image)
   (let* ((ori-width (imlib-get-image-width))
          (ori-height (imlib-get-image-height))
          (scale (/ ori-width 1280))
          (scaled-width (/ ori-width scale))
          (scaled-height (/ ori-height scale))
          ;; NOTE: ffmpeg crashes if image height is not even:
          (scaled-height (if (even? scaled-height) scaled-height
                             (+ -1 scaled-height)))
          (scaled-image (imlib-create-cropped-scaled-image
                         0 0 ori-width ori-height scaled-width scaled-height)))
    (imlib-context-set-image! image)
    (imlib-free-image-and-decache)
    (draw-single-f
     (fourth sentence)
     ;; video image frame index
     video scaled-image frame index
     3
     #t
     '(#(255 0 0) #(0 128 255) #(128 255 0) #(0 255 128))
     #f
     frame-name
     (lambda ()
      (imlib-draw-text-on-image
       scaled-image (first sentence) '#(0 0 0)
       (max 2 (/ scaled-height 35)) 1 1 '#(255 255 255)))
     rank-box-colors?)
    (imlib-context-set-image! scaled-image)
    (imlib-free-image-and-decache)))
  video))

(define (draw-legend-on-image image names colours font-size)
 (when (< (length colours) (length names)) (panic "Too few colours"))
 (unless (null? names)
  (imlib-context-set-image! image)
  (imlib-add-path-to-font-path (*default-font-path*))
  (let ((font (imlib-load-font! (string*-append "DejaVuSans/" font-size))))
   (imlib-context-set-font! font)
   (let loop ((names names)
	      (colours colours)
	      (i 0))
    (let ((k 0))
     (imlib-context-set-color! (x (car colours))
			       (y (car colours))
			       (z (car colours)) 255)
     (imlib-text-draw font-size (+ font-size (* (* 2 font-size) i)) (car names))
     (if (eq? (cdr names) '())
	 '()
	 (loop (cdr names) (cdr colours) (+ (+ i 1) k)))))
   (imlib-free-font))))

(define (draw-points-on-image image points colour)
 (imlib-context-set-image! image)
 (imlib-context-set-color! (x colour) (y colour) (z colour) 255)
 (for-each (lambda (p) (imlib-image-draw-pixel (x p) (y p) 0)) points))

(define (generate-colours-n n)
 (map-n (lambda (_) (vector (exact-round (* 255 (random-real)))
			    (exact-round (* 255 (random-real)))
			    (exact-round (* 255 (random-real)))))
	n))

(define (imlib-free-images-and-decache images)
 (for-each (lambda (image)
	    (imlib-context-set-image! image)
	    (imlib-free-image-and-decache))
	   images))

(define (write-out-frame video-name frame frame-name)
 (imlib-image-set-format "png")
 (let ((filename (generic-full-pathname
		  *video-pathname* video-name
		  (format #f "/~a/~a.png"
			  (number->padded-string-of-length frame 6)
			  frame-name))))
  ;; qobi: See my email dated w11may2011.
  (rm-if-necessary filename)
  ;; Make sure the destination directory exists
  (system (format #f "mkdir -p \"$(dirname \"~a\")\"" filename))
  (imlib-save-image filename)))

(define (map-imlib-frame-from-video-indexed f video-name)
 (with-ffmpeg-video
  video-name
  (lambda (ffmpeg-video)
   (when (= ffmpeg-video 0)
    (display (video-pathname video-name)) (newline)
    (panic "Couldn't open ffmpeg video"))
   (map-frame-indexed
    ;; Frame-nr is the current frame number -- the first frame may not be zero
    ;; Index is the frame offset (starting at 0) in the video file
    (lambda (frame-nr index)
     (when (ffmpeg-video-finished? ffmpeg-video)
      (panic "ffmpeg video finished prematurely"))
     (let ((result (f frame-nr index (ffmpeg-video-frame-data-as-imlib ffmpeg-video))))
      (ffmpeg-next-frame! ffmpeg-video)
      result))
    video-name))))

(define (for-each-imlib-frame-from-video-indexed f video)
 (let ((ffmpeg-video (ffmpeg-open-video (video-pathname video))))
  (when (= ffmpeg-video 0)
   (display (video-pathname video)) (newline)
   (panic "Couldn't open ffmpeg video"))
  (for-each-frame-indexed
   ;; Frame-nr is the current frame number -- the first frame may not be zero
   ;; Index is the frame offset (starting at 0) in the video file
   (lambda (frame-nr index)
    (when (ffmpeg-video-finished? ffmpeg-video)
     (panic "ffmpeg video finished prematurely"))
    (f frame-nr index (ffmpeg-video-frame-data-as-imlib ffmpeg-video))
    (ffmpeg-next-frame! ffmpeg-video))
   video)
  (ffmpeg-close-video ffmpeg-video)))

(define (for-each-imlib-frame-from-video-indexed-but-last f video)
 (let ((ffmpeg-video (ffmpeg-open-video (video-pathname video))))
  (when (= ffmpeg-video 0)
   (display (video-pathname video)) (newline)
   (panic "Couldn't open ffmpeg video"))
  (for-each-frame-indexed-but-last
   ;; Frame-nr is the current frame number -- the first frame may not be zero
   ;; Index is the frame offset (starting at 0) in the video file
   (lambda (frame-nr index)
    (when (ffmpeg-video-finished? ffmpeg-video)
     (panic "ffmpeg video finished prematurely"))
    (f frame-nr index (ffmpeg-video-frame-data-as-imlib ffmpeg-video))
    (ffmpeg-next-frame! ffmpeg-video))
   video)
  (ffmpeg-close-video ffmpeg-video)))

(define (map-imlib-frame-pair-from-video individual-f pair-f video)
 ;; Note that you do not have to free the imlib data here!
 ;; unlike the non-pair version
 ;; individual-f :: frame-nr -> imlib -> a
 ;; pair-f :: a -> a -> b
 (let ((ffmpeg-video (ffmpeg-open-video (video-pathname video))))
  (when (= ffmpeg-video 0)
   (display (video-pathname video)) (newline)
   (panic "Couldn't open ffmpeg video"))
  (let* ((free-list '())
	 (result
	  (map-frame-pair
	   (lambda (frame-nr)
	    (let ((frame-data (ffmpeg-video-frame-data-as-imlib ffmpeg-video)))
	     (set! free-list (cons frame-data free-list))
	     (ffmpeg-next-frame! ffmpeg-video)
	     (individual-f frame-nr frame-data)))
	   pair-f
	   video)))
   (for-each
    (lambda (image)
     (imlib-context-set-image! image)
     (imlib-free-image))
    free-list)
   (ffmpeg-close-video ffmpeg-video)
   result)))

(define (for-each-imlib-frame-pair-from-video-indexed individual-f pair-f video)
 ;; Note that you do not have to free the imlib data here!
 ;; unlike the non-pair version
 ;; individual-f :: frame-nr -> index -> imlib -> a
 ;; pair-f :: a -> a -> b
 (define (for-each-frame-pair-indexed individual-f pair-f video-name)
  (let ((first-frame (video-first-frame video-name))
	(last-frame (video-last-frame video-name)))
   (let loop ((n (+ first-frame 1))
	      (i 1)
	      (prev (individual-f first-frame 0)))
    (unless (> n last-frame)
     (let ((next (individual-f n i)))
      (pair-f prev next)
      (loop (+ n 1) (+ i 1) next))))))
 (let ((ffmpeg-video (ffmpeg-open-video (video-pathname video))))
  (when (= ffmpeg-video 0)
   (display (video-pathname video)) (newline)
   (panic "Couldn't open ffmpeg video"))
  (let ((free-list '()))
   (for-each-frame-pair-indexed
    (lambda (frame-nr index)
     (let ((frame-data (ffmpeg-video-frame-data-as-imlib ffmpeg-video)))
      (set! free-list (cons frame-data free-list))
      (ffmpeg-next-frame! ffmpeg-video)
      (individual-f frame-nr index frame-data)))
    pair-f
    video)
   (for-each
    (lambda (image)
     (imlib-context-set-image! image)
     (imlib-free-image))
    free-list))
  (ffmpeg-close-video ffmpeg-video)))

(define (for-each-imlib-frame-pair-from-video individual-f pair-f video)
 (for-each-imlib-frame-pair-from-video-indexed
  (lambda (frame-nr index frame-data)
   (individual-f frame-nr frame-data))
  pair-f
  video))

(define (draw-tiled detected-boxes predicted-boxes tracked-boxes smooth-tracked-boxes
		    video-name image frame index thickness name? legend? colours frame-name
		    video-caption rank-box-colors?)
 (define (draw image boxes font-size)
  (draw-on-image-from-boxes-details
   image thickness (map first boxes) (map (lambda (a) (list-ref (second a) index)) boxes)
   colours name? font-size #f rank-box-colors?))
 (imlib-context-set-image! image)
 (let* ((height (imlib-get-image-height))
	(width (imlib-get-image-width))
        (font-size (/ height 20))
	(images (map-n (lambda _ (imlib-clone-image)) 4)))
  (map draw
       images
       (list detected-boxes predicted-boxes tracked-boxes smooth-tracked-boxes)
       (list font-size font-size font-size font-size))
  (when legend?
   (draw-legend-on-image
    ;; (first images) (map second (map first detected-boxes)) colours 30))
    (first images) (map second (map first detected-boxes)) colours font-size))
  (let ((new-images
	 (map
	  (lambda (image)
	   (imlib-context-set-image! image)
	   (imlib-create-cropped-scaled-image 0 0 width height (/ width 2) (/ height 2)))
	  images)))
   (imlib-context-set-image! image)
   (imlib-blend-image-onto-image
    (first new-images) 0 0 0 (/ width 2) (/ height 2) 0 0 (/ width 2) (/ height 2))
   (imlib-blend-image-onto-image
    (second new-images) 0 0 0 (/ width 2) (/ height 2) (/ width 2) 0 (/ width 2) (/ height 2))
   (imlib-blend-image-onto-image
    (third new-images) 0 0 0 (/ width 2) (/ height 2) 0 (/ height 2) (/ width 2) (/ height 2))
   (imlib-blend-image-onto-image
    (fourth new-images) 0 0 0 (/ width 2) (/ height 2) (/ width 2) (/ height 2) (/ width 2) (/ height 2))
   ;; caption line:
   (let* ((caption video-caption))
    (imlib-context-set-image! image)
    (imlib-draw-text-on-image image caption '#(255 0 0) 15 45 (- height 45) #f)
    (imlib-context-set-image! image))
   (write-out-frame video-name frame frame-name)
   (imlib-free-images-and-decache (append images new-images)))))

(define (draw-klt image klt thickness color)
 (imlib-context-set-image! image)
 (for-each (lambda (klt-pair)
	    (let* ((p1 (klt-pair-current klt-pair))
		   (p2 (klt-pair-next klt-pair)))
	     (unless (and (= (x p1) (x p2)) (= (y p1) (y p2)))
	      ;; (draw-imlib-line image p1 p2 thickness (vector 0 255 0))
	      (draw-imlib-line image p1 p2 thickness color)
	      (draw-imlib-circle image p1 (* 0.5 thickness)
				 (vector (- 255 (r color))
					 (- 255 (g color))
					 (- 255 (b color)))
				 #t)))) klt))

(define (draw-optical-flow image optical-flow
			   rectangle-width rectangle-height
			   thickness color scale)
 (imlib-context-set-image! image)
 (for-each-approximate-rectangle
  (lambda (x1 y1 x2 y2)
   (let* ((tail (vector (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))
	  (head
	   (v+ tail
	       (average-integral-optical-flow-in-c
		optical-flow
		(* x1 scale) (* y1 scale)
		(* x2 scale) (* y2 scale)))))
    (draw-imlib-line image tail head thickness color)
    (draw-imlib-circle image tail (* 0.5 thickness)
        	       (vector (- 255 (r color))
        		       (- 255 (g color))
        		       (- 255 (b color)))
        	       #t)))
  (- (imlib-get-image-width) 1)
  (- (imlib-get-image-height) 1)
  rectangle-width
  rectangle-height))

(define (topn-boxes-movies n boxes-movies)
 (map (lambda (boxes-movie)
       (map (lambda (boxes)
	     (take-if-possible n (sort boxes > voc4-detection-strength)))
	    boxes-movie))
      boxes-movies))

(define (speak string)
 (system (format #f "flite '~a' play" string)))

(define (create-padded-image image padded-width padded-height)
 (imlib-context-set-image! image)
 (let* ((width (imlib-get-image-width))
        (height (imlib-get-image-height))
        (w-ratio (/ padded-width width))
        (h-ratio (/ padded-height height))
        (ratio (if (< w-ratio h-ratio) w-ratio h-ratio))
        (scaled-width (* ratio width))
        (scaled-height (* ratio height))
        (padded-image (imlib-create-image padded-width padded-height)))
  (imlib-context-set-image! padded-image)
  (imlib-context-set-color! 0 0 0 255)  ; black padded
  (imlib-image-fill-rectangle
   0 0 (imlib-get-image-width) (imlib-get-image-height))
  (imlib-context-set-image! padded-image)
  (imlib-blend-image-onto-image
   image 0 0 0 width height
   (/ (- padded-width scaled-width) 2)
   (/ (- padded-height scaled-height) 2)
   scaled-width scaled-height)
  padded-image))

;;; Condor

(define (create-jobs class object->name object->script object-names)
 (system (format #f "mkdir -p ~a/jobs/~a/scripts" (getenv "HOME") class))
 (system (format #f "mkdir -p ~a/jobs/~a/counters" (getenv "HOME") class))
 (system (format #f "mkdir -p ~a/jobs/~a/condor" (getenv "HOME") class))
 (system (format #f "mkdir -p ~a/jobs/~a/logs" (getenv "HOME") class))
 (for-each (lambda (object)
	    (write-file
	     (vanilla-condor-job class (object->name object) 'x86-64)
	     (format #f "~a/jobs/~a/condor/~a" (getenv "HOME") class (object->name object)))
	    (system (format #f "chmod ug+x ~a/jobs/~a/scripts/~a"
			    (getenv "HOME") class (object->name object)))
	    (write-file
	     (append
	      (list
	       (format #f "#!/bin/sh")
	       (format #f "echo ~a" (object->name object))
	       (format #f "echo `date +%s` >> ~a/jobs/~a/counters/~a"
		       (getenv "HOME") class (object->name object)))
	      (object->script object))
	     (format #f "~a/jobs/~a/scripts/~a" (getenv "HOME") class (object->name object))))
	   object-names))

(define (reset-counters class object-names)
 (for-each (lambda (object)
	    (system (format #f "rm ~a/jobs/~a/counters/~a" (getenv "HOME") class object)))
	   object-names))

(define home-pathname (getenv "HOME"))

(define (vanilla-condor-job class job architecture)
 (list "Universe       = vanilla"
       (format #f "Executable     = ~a/jobs/~a/scripts/~a" home-pathname class job)
       "Notification   = Never"
       ""
       (cond
	((equal? architecture 'any)
	 "Requirements   = (Arch ==\"INTEL\" && OpSys == \"LINUX\") || (Arch ==\"X86_64\" && OpSys == \"LINUX\")")
	((equal? architecture 'i686)
	 "Requirements   = (Arch ==\"INTEL\" && OpSys == \"LINUX\")")
	((equal? architecture 'x86-64)
	 "Requirements   = (Arch ==\"X86_64\" && OpSys == \"LINUX\")")
	(else (fuck-up)))
       ""
       "input   = /dev/null"
       (format #f "output  = ~a/jobs/~a/logs/~a.stdout" home-pathname class job)
       (format #f "error   = ~a/jobs/~a/logs/~a.stderr" home-pathname class job)
       (format #f "log     = ~a/jobs/~a/logs/~a.log" home-pathname class job)
       ""
       "Queue"))

(define (submit-jobs class object-names)
 (for-each (lambda (job) (system (format #f "condor_submit ~a/jobs/~a/condor/~a"
					 (getenv "HOME") class job)))
	   object-names))

(define (show-job-commands class object-names)
 (for-each (lambda (job) (format #t "condor_submit ~a/jobs/~a/condor/~a~%"
				 (getenv "HOME") class job))
	   object-names))

;;; Classifiction

(define (every-other-tail list)
 (define (every-other in list)
  (cond ((null? list) in)
	((null? (rest list)) (cons (car list) in))
	(else (every-other (cons (first list) in) (rest (rest list))))))
 (reverse (every-other '() list)))

(define (every-other-nth-o l nth offset)
 (map second
      (remove-if-not
       (lambda (e) (first e))
       (map-indexed
	(lambda (elem i)
	 (list (if (equal? (remainder i nth) offset) #t #f) elem)) l))))

(define (remove-every-other-nth-o l nth offset)
 (map second
      (remove-if
       (lambda (e) (first e))
       (map-indexed
	(lambda (elem i)
	 (list (if (equal? (remainder i nth) offset) #t #f) elem)) l))))

(define (videos-for-verb classification verb)
 (remove-if-not (lambda (a) (prefix? verb (result-verb a))) classification))

;;; Feature vectors

;;; single-object-features structure
(define-structure single-object-features
 position-x position-y
 aspect aspect-derivative
 velocity velocity-orientation
 acceleration acceleration-orientation
 area area-derivative
 pose-index)

;;; For when an invalid single-object-features struct is required
(define invalid-single-object-features
 (make-single-object-features -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))

;;; pairwise-object-features structure
(define-structure pairwise-object-features
 center-distance center-velocity center-orientation overlap)

;;; For when an invalid pairwise-object-features struct is required
(define invalid-pairwise-object-features (make-pairwise-object-features -1 -1 -1 -1))

;;; #t iff the passed feature contains valid data
(define (valid-single-object-feature? feature)
 (not (= -1 (single-object-features-position-x feature))))

;;; (stable-pairs '(a b c d)) ==> ((A B) (A C) (A D) (B C) (B D) (C D))
;;; (stable-pairs '#(a b c d)) ==> #((A B) (A C) (A D) (B C) (B D) (C D))
(define (stable-pairs l)
 (let* ((v (if (list? l) (list->vector l) l))
	(len (- (vector-length v) 1))
					; indexes (for len=3): ((0 1) (0 2) (0 3) (1 2) (1 3) (2 3))
	(indexes (join (map-n (lambda (i) (map-n (lambda (j) (list i (+ 1 i j))) (- len i))) len)))
	(result (map
		 (lambda (i j) (list (vector-ref v i) (vector-ref v j)))
		 (map first indexes)
		 (map second indexes))))
  (if (not (list? l)) (list->vector result) result)))

;;; Pose index is 1, 2 or 3 for people models (stand, crouch, down), or 0 otherwise.
(define (get-pose-index box)
 (cond ((equal? (voc4-detection-model box) "person") 1)
       ((equal? (voc4-detection-model box) "person-crawl") 2)
       ((equal? (voc4-detection-model box) "person-crouch") 3)
       ((equal? (voc4-detection-model box) "person-down") 4)
       ((equal? (voc4-detection-model box) "person-wheelbarrow") 5)
       (else 0)))

(define (split-box-name name)
 (let ((split (pregexp-split "-" name)))
  (list "voc4"
	(string-join "-" (but-last split))
	(last split)
	"smooth-tracked-box")))

;;; (list-of-lists->transposed-matrix ((1 2 3) (4 5 6))) ==> #(#(1 4) #(2 5) #(3 6))
;;; (list-of-lists->transposed-matrix ((1 2 3) (4 5 6)) 'a) ==> #(#(A 1 4) #(A 2 5) #(A 3 6))
(define (list-of-lists->transposed-matrix list-of-lists . symbol)
 (if (null? list-of-lists)
     '#()
     (let ((augmented-list
	    (if (null? symbol)
		list-of-lists
		(let* ((n-structs (length (first list-of-lists)))
		       (symbol-list (map-n (lambda (i) (first symbol)) n-structs)))
		 (cons symbol-list list-of-lists)))))
      (transpose (list->vector (map list->vector augmented-list))))))

(define (polar->rect p)
 (vector (* (x p) (cos (y p))) (* (x p) (sin (y p)))))

(define (rect->polar r)
 (vector (magnitude r) (vector-orientation r)))

(define (drop-last-n-if-possible n l)
 (reverse (drop-if-possible n (reverse l))))

;;; (pairwise-track-features person-boxes suv-boxes 4)
;;;     ==> #(#(pairwise-features-person-1-suv-1 for frame 1) ...)
(define (pairwise-track-features boxes1 boxes2 lookahead)
 (when (or (some dropped-box? boxes1) (some dropped-box? boxes2))
  (panic "Passing in a padding box to the feature vector computation"))
 (pairwise-track-features-depraved (map voc4-detection-center boxes1)
				   (map voc4-detection-center boxes2)
				   lookahead))

;;; -------------------------------------------------------------------- Verb-fun Features
(define (verb-single-track-features boxes lookahead)
 (let* ((position (map voc4-detection-center boxes))
	(position-x (map x position))
	(position-y (map y position))
	(aspect-ratio (map voc4-detection-aspect-ratio boxes))
	(aspect-ratio-derivative (feature-finite-difference aspect-ratio lookahead))
	(velocity (feature-finite-difference (map vector position-x position-y) lookahead))
	(velocity-magnitude (map magnitude velocity))
	(velocity-direction (map vector-orientation velocity))
	(acceleration (feature-finite-difference velocity lookahead))
	(acceleration-magnitude (map magnitude acceleration))
	(acceleration-direction (map vector-orientation acceleration))
	(area (map voc4-detection-area boxes))
	(area-derivative (feature-finite-difference area lookahead))
	(pose-index (map get-pose-index boxes))
	;; Convert features from column major format
	(feature-matrix (list-of-lists->transposed-matrix
			 `(,position-x ,position-y ,aspect-ratio ,aspect-ratio-derivative
				       ,velocity-magnitude ,velocity-direction
				       ,acceleration-magnitude ,acceleration-direction
				       ,area ,area-derivative
				       ,pose-index))))
  ;; Some feature vectors will have invalid entries, they are replaced by -1s
  (map-vector-with-lookahead
   (lambda (window)
    (if (or (some (lambda (feature) (= -1 (vector-ref feature 0))) window)
	    (< (length window) lookahead))
	(make-vector (vector-length (first window)) -1)
	(first window)))
   lookahead
   feature-matrix)))

;;; (pairwise-track-features person-boxes suv-boxes 4)
;;;     ==> #(#(pairwise-features-person-1-suv-1 for frame 1) ...)
(define (verb-pairwise-track-features boxes1 boxes2 lookahead)
 (let* (;; #t iff the x-coord of either boxes1 or boxes2 is -1
	(position-x-invalid (map
			     (lambda (x1 x2) (if (or (= -1 x1) (= -1 x2)) #t #f))
			     (map x (map voc4-detection-center boxes1))
			     (map x (map voc4-detection-center boxes2))))
	(center-distance
	 (map
	  (lambda (a b) (distance (voc4-detection-center a) (voc4-detection-center b)))
	  boxes1
	  boxes2))
	(center-velocity (feature-finite-difference center-distance lookahead))
	(center-orientation
	 (map
	  vector-orientation
	  (map
	   (lambda (a b) (v- (voc4-detection-center a) (voc4-detection-center b)))
	   boxes1
	   boxes2)))
	(overlap
	 (map (lambda (box1 box2)
	       (let ((intersect-area (voc4-detection-intersection-area box1 box2)))
		(if (> 0 intersect-area)
		    0
		    (/ intersect-area
		       (min (voc4-detection-area box1) (voc4-detection-area box2))))))
	      boxes1 boxes2))
	(feature-matrix (list-of-lists->transposed-matrix
			 (list center-distance center-velocity
			       center-orientation overlap))))
  (map-vector-with-lookahead ; (see note on single-track-features)
   (lambda (window invalid-rows)
    (if (or (some identity invalid-rows) ; An invalid row exists in the lookahead window
	    (< (length window) lookahead))
	(make-vector (vector-length (first window)) -1)
	(first window)))
   lookahead
   feature-matrix
   (list->vector position-x-invalid))))

;;; Gets overgenerated-smooth-tracked boxes for C-D1/recognition and C-E1/recognition
(define (available-smooth-tracks video)
 (if (or (equal? "C-D1/recognition" (any-video->corpus video))
	 (equal? "C-E1/recognition" (any-video->corpus video)))
     (video-voc4-overgenerated-smooth-tracked-boxes-available video)
     (video-voc4-tracked-boxes-available video)))

;;; (available-track-names '(("voc4" "person" 1 ...) ...)) ==> ("person-1" "suv-1" "suv-2")
;;; Gets overgenerated-smooth-tracked boxes for C-D1/recognition and C-E1/recognition
(define (available-track-names voc4-tracks)
 (map (lambda (t) (format #f "~a-~a" (second t) (third t))) voc4-tracks))

;;; Returns the box-tracks for the given video and set of tracks
;;; (read-named-smooth-box-tracks video 2 '("person-1" "person-2")) ==> ((voc4-detection ...) ...)
;;; Gets overgenerated-smooth-tracked boxes for C-D1/recognition and C-E1/recognition
(define (read-overgenerated-smooth-box-track-set video tracks)
 (let ((read-lambda (if (or (equal? "C-D1/recognition" (any-video->corpus video))
			    (equal? "C-E1/recognition" (any-video->corpus video)))
			read-voc4-overgenerated-smooth-tracked-boxes
			read-voc4-smooth-tracked-boxes )))
  (map (lambda (track) (read-lambda video (second track) (third track))) tracks)))

;;; Returns something like:
;;; (("person-1" #(#())) ("suv-1" #(#())))
(define (recalculate-all-verb-single-features video lookahead)
 (display (string-append "Recalculating single features: " (any-video->string video))) (newline)
 (let* ((tracks (available-smooth-tracks video))
	(track-names (available-track-names tracks))
	(box-tracks (read-overgenerated-smooth-box-track-set video tracks))
	(single-features-set
	 (map (lambda (boxes)
	       (matrix->structured-matrix
		(verb-single-track-features boxes lookahead) 'SINGLE-OBJECT-FEATURES))
	      box-tracks)))
  (zip track-names single-features-set)))

;;; Returns something like
;;; (("person-1 suv-1" #(#())) ("person-1_suv-2" #(#())) ("suv-1_suv-2" #(#())))
(define (recalculate-all-verb-pairwise-features video lookahead)
 (display (string-append "Recalculating pairwise features: " (any-video->string video))) (newline)
 (let* ((tracks (available-smooth-tracks video))
	(track-names (available-track-names tracks))
	(box-tracks (read-overgenerated-smooth-box-track-set video tracks))
	(track-pair-names (stable-pairs track-names))
	(track-pair-boxes (stable-pairs box-tracks))
	(pairwise-features-set
	 (map
	  (lambda (boxes-1 boxes-2)
	   (matrix->structured-matrix
	    (verb-pairwise-track-features boxes-1 boxes-2 lookahead)
	    'PAIRWISE-OBJECT-FEATURES))
	  (map first track-pair-boxes)
	  (map second track-pair-boxes))))
  (zip
   (map (lambda (p) (string-append (first p) "_" (second p))) track-pair-names)
   pairwise-features-set)))

;;; True when the 'features' looks like either pairwise or single features
(define (is-cached-features features)
 (if (not (list? features))
     #f
     (if (null? features)
	 #t ; There are no features for this video
	 (foldl (lambda (a b) (and a b))
		(map (lambda (f)
		      (and (equal? 2 (length f))
			   (string? (first f))
			   (vector? (second f))
			   (> (vector-length (second f)) 0)
			   (> (vector-length (vector-ref (second f) 0)) 0)))
		     features)
		#t))))

;;; Loades the cachable features, and forces a recalculation if the result looks fishy
(define (read-all-verb-features filename regen-fun recalculate)
 (let ((features (read-cachable-result filename regen-fun recalculate)))
  (if (is-cached-features features)
      features
      (read-cachable-result filename regen-fun #t)))) ; The file was probably corrupted

;;; See (recalculate-all-single-features video lookahead)
(define (all-verb-single-features video lookahead recalculate)
 (read-all-verb-features (cached-all-features-pathname video "verb-single" lookahead)
			 (lambda () (recalculate-all-verb-single-features video lookahead))
			 recalculate))

;;; See (recalculate-all-pairwise-features video lookahead)
(define (all-verb-pairwise-features video lookahead recalculate)
 (read-all-verb-features (cached-all-features-pathname video "verb-pairwise" lookahead)
			 (lambda () (recalculate-all-verb-pairwise-features video lookahead))
			 recalculate))

;;; Limbs

(define (limbs->lines limbs)
 (define (make-limb-segment a b)
  (if (and a b) (make-line-segment (limb-position a) (limb-position b)) #f))
 (removeq
  #f
  (list
   (make-limb-segment (limb-annotation-l-arm limbs)
		      (limb-annotation-l-elbow limbs))
   (make-limb-segment (limb-annotation-l-elbow limbs)
		      (limb-annotation-l-shoulder limbs))
   (make-limb-segment (limb-annotation-r-arm limbs)
		      (limb-annotation-r-elbow limbs))
   (make-limb-segment (limb-annotation-r-elbow limbs)
		      (limb-annotation-r-shoulder limbs))
   (make-limb-segment (limb-annotation-l-shoulder limbs)
		      (limb-annotation-r-shoulder limbs))
   (when (and (limb-annotation-l-shoulder limbs)
	      (limb-annotation-r-shoulder limbs)
	      (limb-annotation-l-pelvis limbs)
	      (limb-annotation-r-pelvis limbs))
    (make-line-segment
     (list-mean (list (limb-position (limb-annotation-l-shoulder limbs))
		      (limb-position (limb-annotation-r-shoulder limbs))))
     (list-mean (list (limb-position (limb-annotation-l-pelvis limbs))
		      (limb-position (limb-annotation-r-pelvis limbs))))))
   (make-limb-segment (limb-annotation-l-pelvis limbs)
		      (limb-annotation-r-pelvis limbs))
   (make-limb-segment (limb-annotation-l-pelvis limbs)
		      (limb-annotation-l-knee limbs))
   (make-limb-segment (limb-annotation-l-knee limbs)
		      (limb-annotation-l-foot limbs))
   (make-limb-segment (limb-annotation-r-pelvis limbs)
		      (limb-annotation-r-knee limbs))
   (make-limb-segment (limb-annotation-r-knee limbs)
		      (limb-annotation-r-foot limbs)))))

(define (limbs->limbs-bb limb-annotation bb)
 (define (limb-bb limb)
  (make-limb (vector (- (x (limb-position limb)) (x bb))
		     (- (y (limb-position limb)) (y bb)))
	     (limb-visible? limb)))
 (make-limb-annotation
  (limb-bb (limb-annotation-l-arm limb-annotation))
  (limb-bb (limb-annotation-r-arm limb-annotation))
  (limb-bb (limb-annotation-l-elbow limb-annotation))
  (limb-bb (limb-annotation-r-elbow limb-annotation))
  (limb-bb (limb-annotation-l-shoulder limb-annotation))
  (limb-bb (limb-annotation-r-shoulder limb-annotation))
  (limb-bb (limb-annotation-l-pelvis limb-annotation))
  (limb-bb (limb-annotation-r-pelvis limb-annotation))
  (limb-bb (limb-annotation-l-knee limb-annotation))
  (limb-bb (limb-annotation-r-knee limb-annotation))
  (limb-bb (limb-annotation-l-foot limb-annotation))
  (limb-bb (limb-annotation-r-foot limb-annotation))))

(define (limbs->lines-bb limbs bb)
 (limbs->lines (limbs->limbs-bb limbs bb)))

(define (limbs-bounding-box limbs)
 (points-bounding-box
  (map limb-position (limbs->annotated-limbs limbs))))

(define (limbs->annotated-limbs limbs)
 (removeq #f (list (limb-annotation-l-arm limbs)
		   (limb-annotation-r-arm limbs)
		   (limb-annotation-l-elbow limbs)
		   (limb-annotation-r-elbow limbs)
		   (limb-annotation-l-shoulder limbs)
		   (limb-annotation-r-shoulder limbs)
		   (limb-annotation-l-pelvis limbs)
		   (limb-annotation-r-pelvis limbs)
		   (limb-annotation-l-knee limbs)
		   (limb-annotation-r-knee limbs)
		   (limb-annotation-l-foot limbs)
		   (limb-annotation-r-foot limbs))))

(define (points->limbs l)
 (apply make-limb-annotation
	(map (lambda (a) (make-limb a #t)) l)))

(define (limbs->selected limbs)
 (define (make-selected-limb limb get set name) (make-selected (limb-position limb) get set name))
 (remove-if
  (lambda (a) (equal? (selected-point a) #f))
  (list
   (make-selected-limb
    (limb-annotation-l-arm limbs) limb-annotation-l-arm set-limb-annotation-l-arm! "l-arm")
   (make-selected-limb
    (limb-annotation-l-elbow limbs) limb-annotation-l-elbow set-limb-annotation-l-elbow! "l-elbow")
   (make-selected-limb
    (limb-annotation-l-shoulder limbs) limb-annotation-l-shoulder set-limb-annotation-l-shoulder! "l-shoulder")
   (make-selected-limb
    (limb-annotation-r-arm limbs) limb-annotation-r-arm set-limb-annotation-r-arm! "r-arm")
   (make-selected-limb
    (limb-annotation-r-elbow limbs) limb-annotation-r-elbow set-limb-annotation-r-elbow! "r-elbow")
   (make-selected-limb
    (limb-annotation-r-shoulder limbs) limb-annotation-r-shoulder set-limb-annotation-r-shoulder! "r-shoulder")
   (make-selected-limb
    (limb-annotation-l-pelvis limbs) limb-annotation-l-pelvis set-limb-annotation-l-pelvis! "l-pelvis")
   (make-selected-limb
    (limb-annotation-l-knee limbs) limb-annotation-l-knee set-limb-annotation-l-knee! "l-knee")
   (make-selected-limb
    (limb-annotation-l-foot limbs) limb-annotation-l-foot set-limb-annotation-l-foot! "l-foot")
   (make-selected-limb
    (limb-annotation-r-pelvis limbs) limb-annotation-r-pelvis set-limb-annotation-r-pelvis! "r-pelvis")
   (make-selected-limb
    (limb-annotation-r-knee limbs) limb-annotation-r-knee set-limb-annotation-r-knee! "r-knee")
   (make-selected-limb
    (limb-annotation-r-foot limbs) limb-annotation-r-foot set-limb-annotation-r-foot! "r-foot"))))

(define (limbs->joint-angles limbs)
 (define (joint-angle a b c)
  (acos (dot (unit (v- (limb-position a) (limb-position b)))
	     (unit (v- (limb-position c) (limb-position b))))))
 (list (joint-angle (limb-annotation-l-arm limbs)
		    (limb-annotation-l-elbow limbs)
		    (limb-annotation-l-shoulder limbs))
       (joint-angle (limb-annotation-l-elbow limbs)
		    (limb-annotation-l-shoulder limbs)
		    (limb-annotation-r-shoulder limbs))
       (joint-angle (limb-annotation-r-arm limbs)
		    (limb-annotation-r-elbow limbs)
		    (limb-annotation-r-shoulder limbs))
       (joint-angle (limb-annotation-r-elbow limbs)
		    (limb-annotation-r-shoulder limbs)
		    (limb-annotation-l-shoulder limbs))
       (joint-angle (limb-annotation-l-foot limbs)
		    (limb-annotation-l-knee limbs)
		    (limb-annotation-l-pelvis limbs))
       (joint-angle (limb-annotation-l-knee limbs)
		    (limb-annotation-l-pelvis limbs)
		    (limb-annotation-r-pelvis limbs))
       (joint-angle (limb-annotation-r-foot limbs)
		    (limb-annotation-r-knee limbs)
		    (limb-annotation-r-pelvis limbs))
       (joint-angle (limb-annotation-r-knee limbs)
		    (limb-annotation-r-pelvis limbs)
		    (limb-annotation-l-pelvis limbs))))

(define (limbs-distance limbs1 limbs2)
 (magnitude
  (list->vector
   (map radial-distance (limbs->joint-angles limbs1) (limbs->joint-angles limbs2)))))

(define (limbs->points limbs)
 (map limb-position (limbs->annotated-limbs limbs)))

(define (draw-limb-annotation annotation)
 (let ((radius 2))
  (for-each (lambda (l) (xdrawline *display* *display-pane* *orange-gc*
				   (x (p l)) (y (p l)) (x (q l)) (y (q l))))
	    (limbs->lines annotation))
  (for-each (lambda (l) (xdrawarc *display*
				  *display-pane*
				  (if (limb-visible? l)
				      *green-gc*
				      *red-gc*)
				  (- (x (limb-position l)) radius)
				  (- (y (limb-position l)) radius)
				  (+ (* 2 radius) 1)
				  (+ (* 2 radius) 1)
				  (* 0 360)
				  (* 64 360)))
	    (limbs->annotated-limbs annotation))))

(define (read-distances video-name frame videos-frames)
 (removeq
  #f
  (map
   (lambda (v-f)
    (let ((filename
	   (frame-distance-pathname
	    video-name frame
	    (string->darpa-video (first v-f)) (second v-f))))
     (if (file-exists? filename)
	 (list (string->darpa-video (first v-f))
	       (second v-f)
	       (read-object-from-file filename)))))
   videos-frames)))

(define (read-distances-and-limbs video start-frame end-frame videos-frames)
 (map-m-n
  (lambda (frame)
   (map
    (lambda (d)
     (append d (list (read-object-from-file
		      (human-limb-annotation-pathname (first d) (second d))))))
    (read-distances video frame videos-frames)))
  start-frame
  end-frame))

;;; GUI Statistics

(define *statistics-time-stamp* #f)
(define *statistics-number-of-clicks* #f)

(define (statistics-collect-and-write! video-name . other)
 (let* ((type (if (= (length other) 1) (first other) (second other)))
	(frame-number (if (= (length other) 2) (second other) #f))
	(t (current-time))
	(diff (- t *statistics-time-stamp*))
	(pathname
	 (if frame-number
	     (human-annotation-statistics-pathname video-name frame-number)
	     (human-annotation-statistics-pathname video-name)))
	(stats
	 (if (file-exists? pathname)
	     (read-object-from-file pathname)
	     '())))
  (write-object-to-file
   (cons (make-human-annotation-statistics
	  (current-time) *statistics-number-of-clicks* diff type)
	 stats)
   pathname)
  (statistics-reset!)
  diff))

(define (statistics-time-elapsed)
 (- (current-time) *statistics-time-stamp*))

(define (statistics-reset!)
 (set! *statistics-number-of-clicks* 0)
 (if *statistics-time-stamp*
     (let ((t (current-time))) (set! *statistics-time-stamp* t) t)
     (begin (set! *statistics-time-stamp* (current-time)) 0)))

(define (statistics-click!)
 (set! *statistics-number-of-clicks* (+ *statistics-number-of-clicks* 1)))

;;; ffmpeg
;;; TODO wrap up handles, add ffi support

(define (ffmpeg-open-video filename)
 (let ((r ((c-function pointer ("ffmpeg_open_video" string)) filename)))
  (if (= r 0) #f r)))

(define ffmpeg-close-video (c-function void ("ffmpeg_close_and_free_video" pointer)))

(define (ffmpeg-video-finished? video)
 (= ((c-function int ("ffmpeg_video_finished" pointer)) video) 1))

(define (ffmpeg-next-frame! video)
 (if (ffmpeg-video-finished? video)
     #f
     (begin ((c-function void ("ffmpeg_next_frame" pointer)) video)
	    #t)))

(c-define-struct-field "ffmpeg_video_t" "frame" int)

(define (ffmpeg-video-frame-index video)
 (ffmpeg-video-t-frame video))

(define ffmpeg-video-width (c-function unsigned ("ffmpeg_video_width" pointer)))
(define ffmpeg-video-height (c-function unsigned ("ffmpeg_video_height" pointer)))

(define ffmpeg-video-frame-rate
 (c-function double ("ffmpeg_video_frame_rate" pointer)))

(define (ffmpeg-video-frame-data video)
 (let* ((data ((c-function pointer ("ffmpeg_get_frame" pointer)) video))
	(width (ffmpeg-video-width video))
	(height (ffmpeg-video-height video))
	(ppm (ppm-constant width height 0 0 0))
	(red (ppm-red ppm))
	(green (ppm-green ppm))
	(blue (ppm-blue ppm))
	(char-ref (c-sized-int-ptr-ref 1 #f)))
  (for-each-n
   (lambda (h)
    (for-each-n
     (lambda (w)
      (let ((index (* (+ (* h width) w) 4)))
       (matrix-set! red h w (char-ref data (+ index 2)))
       (matrix-set! green h w (char-ref data (+ index 1)))
       (matrix-set! blue h w (char-ref data index))))
     width))
   height)
  (free data)
  ppm))

(define ffmpeg-video-frame-data-as-imlib
 (c-function imlib-image ("ffmpeg_get_frame_as_imlib" pointer)))

(define (with-ffmpeg-video video-name f)
 (let* ((video (ffmpeg-open-video (video-pathname video-name)))
	(return (f video)))
  (ffmpeg-close-video video)
  return))

(define (draw-box gc x1 y1 x2 y2)
 (xdrawline *display* *display-pane* gc x1 y1 x1 y2)
 (xdrawline *display* *display-pane* gc x1 y1 x2 y1)
 (xdrawline *display* *display-pane* gc x2 y2 x2 y1)
 (xdrawline *display* *display-pane* gc x2 y2 x1 y2))

;;; HMM

;; Should at some point go into hmm-wbm but until the FFI is swapped
;; out that isn't possible
(define *hmm-maximum-likelihood-training* (c-value int "HMM_ML"))
(define *hmm-discriminative-training* (c-value int "HMM_DT"))
(define *hmm-dt-after-ml-training* (+ *hmm-discriminative-training* 1))

;;; iRobot's CUDA Felzenszwalb

(define (make-cuda-felzenszwalb! model-files pca-file
				 final-cascade-adjustment part-threshold-adjustment
				 quiet? max-detections)
 (when (null? model-files) (panic "Must specify at least 1 model file"))
 (let ((cupedro (cupedro-new
		 ;; We reverse the list of model files here because
		 ;; cupero will return the results in reverse order
		 ;; for some reason
		 (reverse model-files)
		 pca-file)))
  (cupedro-set-quiet! cupedro quiet?)
  (cupedro-set-global-final-cascade-threshold! cupedro final-cascade-adjustment)
  (cupedro-set-global-threshold-adjustment! cupedro part-threshold-adjustment)
  (make-cuda-felzenszwalb cupedro max-detections)))

(define (cuda-felzenszwalb-free cuda-felzenszwalb)
 (cupedro-delete! (cuda-felzenszwalb-handle cuda-felzenszwalb)))

(define (roi-width roi) (- (roi-x2 roi) (roi-x1 roi)))
(define (roi-height roi) (- (roi-y2 roi) (roi-y1 roi)))

(define (cuda-felzenszwalb-detect-rois cuda-felzenszwalb image rois)
 ;; WARNING -- BUG
 ;; Felzenszwalb output must be scaled... use the image-width
 ;; to determine the amount
 (map-reduce
  (lambda (a b) (map append a b))
  (map-n (const '()) (length rois))
  (lambda (roi)
   (imlib-context-set-image! image)
   (let ((cropped-image
	  (imlib-create-cropped-scaled-image (roi-x1 roi)
					     (roi-y1 roi)
					     (roi-width roi)
					     (roi-height roi)
					     (roi-width roi)
					     (roi-height roi))))
    (imlib-context-set-image! cropped-image)
    (when (cuda-felzenszwalb-next-max-detections cuda-felzenszwalb)
     (cupedro-detect! (cuda-felzenszwalb-handle cuda-felzenszwalb)
		      (imlib-get-data-ptr-read-only)
		      (roi-width roi) (roi-height roi))
     (cupedro-set-max-detections! (cuda-felzenszwalb-handle cuda-felzenszwalb)
				  (cuda-felzenszwalb-next-max-detections cuda-felzenszwalb))
     (set-cuda-felzenszwalb-next-max-detections! cuda-felzenszwalb #f))
    (cupedro-detect! (cuda-felzenszwalb-handle cuda-felzenszwalb)
		     (imlib-get-data-ptr-read-only)
		     (roi-width roi) (roi-height roi))
    (imlib-context-set-image! cropped-image)
    (imlib-free-image-and-decache)
    (map-n (lambda (i)
	    (map (lambda (box)
		  (voc4-scale-and-shift box
					'#(1 1)
					(vector (roi-x1 roi) (roi-y1 roi))
					0))
		 (cupedro-last-detection-results
		  (cuda-felzenszwalb-handle cuda-felzenszwalb) i)))
	   (cupedro-number-of-models (cuda-felzenszwalb-handle cuda-felzenszwalb)))))
  rois))

(define (run-felzenszwalb-cuda video
			       model-files
			       pca-file
			       final-cascade-adjustment
			       threshold-adjustment
			       max-detections
			       post-process-f)
 ;; WARNING This function uses globals and is not reentrant
 ;;
 ;; Note, the output boxes are scaled to 1280x720 box coordinates
 (when (null? model-files) (panic "Must specify at least 1 model file"))
 (set! *detector-boxes-movies*
       (map-n (lambda (_) '()) (length model-files)))
 (let ((cupedro (cupedro-new
		 ;; We reverse the list of model files here because
		 ;; cupero will return the results in reverse order
		 ;; for some reason
		 (reverse model-files)
		 pca-file)))
  (set! *detector-handle* cupedro)
  (cupedro-set-quiet! cupedro *quiet-mode?*)
  (cupedro-set-global-final-cascade-threshold! cupedro final-cascade-adjustment)
  (cupedro-set-global-threshold-adjustment! cupedro threshold-adjustment)
  (let
    ((result
      (map-imlib-frame-from-video-indexed
       (lambda (video-frame-number index imlib-image-handle)
	(imlib-context-set-image! imlib-image-handle)
	(let ((width (imlib-get-image-width))
	      (scale (/ (imlib-get-image-width) 1280))
	      (height (imlib-get-image-height))
	      (raw-data (imlib-get-data-ptr-read-only)))
	 (unless *quiet-mode?* (format #t "frame=~a " index))
	 (when (= index 0)
	  (cupedro-detect! cupedro raw-data width height)
	  (cupedro-set-max-detections! cupedro max-detections))
	 (cupedro-detect! cupedro raw-data width height)
	 (imlib-free-image)
	 (let* ((unscaled-result (map-n
				  (lambda (model-index)
				   (post-process-f
				    (cupedro-last-detection-results
				     cupedro model-index)
				    model-index))
				  (cupedro-number-of-models cupedro)))
                (result (map (lambda (list-voc4-boxes)
			      (map (lambda (voc4-box)
				    (voc4-scale-abs voc4-box (/ 1 scale)))
				   list-voc4-boxes))
			     unscaled-result)))
	  (when *callback-function*
	   (call-with-current-continuation
	    (lambda (k)
	     (set! *detector-continuation* k)
	     (set! *detector-boxes-movies*
		   (map (lambda (m r) (append m (list r)))
			*detector-boxes-movies* result))
	     (*callback-function* #f))))
	  result)))
       video)))
   (set! *detector-continuation* #f)
   (when *detector-handle*
    (cupedro-delete! *detector-handle*)
    (set! *detector-handle* #f))
   (when *callback-function* (*callback-function* #f))
   (map-n (lambda (m) (map (lambda (r) (list-ref r m)) result))
	  (cupedro-number-of-models cupedro)))))

(define *voc4-models-root*
 (string-append (getenv "HOME") "/video-datasets/C-D1/voc4-models/"))

(define (cuda-model-pathname model)
 (string-append *voc4-models-root* "/CUDA-1.1/" model ".irobot-felz-model"))
(define (cuda-pca-coefficients-pathname)
 (string-append *voc4-models-root* "/CUDA-1.1/felz_pca_coeff.csv"))

(define (run-felzenszwalb-detector
	 video models
         final-cascade-adjustment threshold-offset
         nms-threshold top-n max-detections)
 (run-felzenszwalb-cuda
  video
  (map cuda-model-pathname models)
  (cuda-pca-coefficients-pathname)
  final-cascade-adjustment
  threshold-offset
  max-detections
  (lambda (boxes model-index)
   (top-n-non-maximal-suppression-with-ringing boxes nms-threshold top-n))))

;;; Commands

;;; Top Level

;;; ---------------------------------------------------------------------------- Plotting Features
;;; (chiefly: Aaron Michaux)

;;; Returns TRUE iff the passed file can be safely read by read-object-from-file
;;; Must make sure that 'is-well-formed-sc-object-file' has been built.
(define (is-well-formed-object-file filename)
 (if (not (file-exists? filename))
     #f
     (let* ((cmd (format #f "$HOME/darpa-collaboration/bin/darpa-wrap $HOME/darpa-collaboration/ideas/$(architecture-path)/is-well-formed-sc-object-file ~a 2>/dev/null | grep -q -E \"If you can see this text, then the file was well-formed\\.$\"" filename)))
      (equal? 0 (system cmd)))))

;;; Attempts to read filename using read-object-from-file, returning default
;;; if there is some sort of error, or if the file is empty
;;; See (is-well-formed-object-file)
(define (safe-read-object-from-file filename default)
 (if (not (is-well-formed-object-file filename))
     default
     (let ((res (read-object-from-file filename)))
      (if (eof-object? res) default res))))

;;; In general, try setting this value to control recalculation when using (read-cachable-result)
(define *use-cached-results* #t)

;;; Generic function for reading cacheable calculations.
;;; Returns contents of cache-file if it exists, and force-recalculate is #f
;;; Otherwise returns the result of (recalculate-fun) after saving the result to file
;;; See (use-cached-features)
(define (read-cachable-result cache-file recalculate-fun force-recalculate)
 (if (and (not force-recalculate) (is-well-formed-object-file cache-file))
     (read-object-from-file cache-file)
     (let ((result (recalculate-fun)))
      (system (format #f "mkdir -p \"$(dirname \"~a\")\"" cache-file))
      (write-object-to-file result cache-file)
      result)))

;;; The available track namds for a c-d1 video only. See (available-track-names video)
(define (available-track-names-c-d1-video video)
 (let ((filename (human-track-annotation-pathname video)))
  (unless (equal? "C-D1/recognition" (any-video->corpus video)) (panic "Wrong corpus"))
  (if (not (file-exists? filename))
      (begin (display (string-append "Failed to find file " filename)) (newline) '())
      (let* ((raw-tracks (safe-read-object-from-file filename '())))
       (if (and (list? raw-tracks) (> (length raw-tracks) 0) (list? (first raw-tracks)))
	   (let ((track-annotations (remove-if-not y (first raw-tracks))))
	    (map (lambda (track)
		  (string-append (second track) "-" (third track)))
		 (map z track-annotations)))
	   '())))))

;;; Prepends symbol to every row of matrix
(define (matrix->structured-matrix matrix symbol)
 (map-vector (lambda (row) (vector-append `#(,symbol) row)) matrix))

(define (map2 f l1 l2)
 (unless (= (length l1) (length l2)) (panic "map2"))
 (map f l1 l2))

(define (fit-line xs ys)
 (let ((inverse (left-pseudo-inverse
		 (list->vector (map (lambda (x) (vector x 1)) xs)))))
  (if inverse (m*v inverse (list->vector ys)) #f)))

(define (eval-line line v) (+ (* (x line) v) (y line)))

(define (bound v low high) (max (min v high) low))

(define (video? transformation-movie-or-video)
 (or (standard-video? transformation-movie-or-video)
     (darpa-video? transformation-movie-or-video)
     (stand-alone-video? transformation-movie-or-video)))

(define (matlab-threshold-otsu l)
 (if (null? l)
     minus-infinity
     (begin
      (scheme->matlab! "l" l)
      (matlab-eval-strings "min_l=min(l);"
			   "t = graythresh(l - min_l) + min_l;")
      (matlab-get-double "t"))))

(define (chop-rectangle-by-block-size box block-width block-height)
 (let* ((box-width (+ 1 (- (third box) (first box))))
        (box-height (+ 1 (- (fourth box) (second box))))
        (x-coordinates
         (cons (first box)
               (map-m-n (lambda (i) (+ (first box) (- (* block-width i) 1)))
                        1 (floor (/ box-width block-width)))))
        (x-coordinates (if (= 0 (modulo box-width block-width))
                           x-coordinates
                           (append x-coordinates (list (third box)))))
        (y-coordinates
         (cons (second box)
               (map-m-n (lambda (i) (+ (second box) (- (* block-height i) 1)))
                        1 (floor (/ box-height block-height)))))
        (y-coordinates (if (= 0 (modulo box-height block-height))
                           y-coordinates
                           (append y-coordinates (list (fourth box)))))
        (n-columns (- (length x-coordinates) 1))
        (n-rows (- (length y-coordinates) 1))
        (all-corners (all-values (nondeterministic-map
                                  a-member-of (list
                                               y-coordinates x-coordinates))))
        (all-boxes (map (lambda (up-left down-right) (list up-left down-right))
                        (sublist all-corners 0
                                 (- (length all-corners) (+ 2 n-columns)))
                        (sublist all-corners (+ 2 n-columns)
                                 (length all-corners))))
        (all-good-boxes (remove-every-other-nth-o
                         all-boxes (+ 1 n-columns) n-columns)))
  (map (lambda (box-as-a-list)
        (list (second (first box-as-a-list)) (first (first box-as-a-list))
	      (second (second box-as-a-list)) (first (second box-as-a-list))))
       all-good-boxes)))

;;; KLT
(define-structure klt-pair current next)

(define (klt-pair->string klt)
 (string*-append (x (klt-pair-current klt)) " "
		 (y (klt-pair-current klt)) " "
		 (x (klt-pair-next klt)) " "
		 (y (klt-pair-next klt))))

(define (string->klt-pair string)
 (let ((klt (map string->number (fields string))))
  (make-klt-pair (vector (first klt) (second klt))
		 (vector (third klt) (fourth klt)))))

(define (read-klt-movie video-name)
 (with-zip-file
  (lambda (zip)
   (map-frame-but-last
    (lambda (frame)
     (map string->klt-pair
	  (zip:read-file zip (per-frame-klt-in-zip-pathname frame))))
    video-name))
  (klt-pathname video-name)
  *zip:mode-open*))

(define (write-klt-movie video-name klt-movie)
 (rm-if-necessary (klt-pathname video-name))
 (with-zip-file
  (lambda (zip2)
   (for-each-frame-indexed-but-last
    (lambda (frame offset)
     (zip:add-directory zip2 (number->padded-string-of-length frame 6))
     (zip:add-file zip2
		   (per-frame-klt-in-zip-pathname frame)
		   (map klt-pair->string (list-ref klt-movie offset))))
    video-name))
  (klt-pathname video-name) *zip:mode-create-new*))

(define (klt-movie-available? video-name)
 (file-exists? (klt-pathname video-name)))

(define cuklt-new (c-function pointer ("cuklt_new")))

(define cuklt-delete (c-function void ("cuklt_delete" pointer)))

(define cuklt-init!
 ;; Initialize cuklt as follows:
 ;;   (cuklt-init! cuklt width height)
 (c-function int ("cuklt_init" pointer int int)))

(define cuklt-init-long!
 ;; Initialize cuklt with more parameteres as follows:
 ;;   (cuklt-init-long! cuklt width height max-num-features
 ;;    pyramid-levels mask-size template-size)
 ;; Calling (cuklt-init width height) is same as calling
 ;;   (cuklt-init-long! width height 1000 3 15 11 3)
 (c-function int ("cuklt_init_long" pointer int int int int int int int)))

(define (cuklt-quiet?)
 ;; Only errors are printed when in quiet mode
 (define c-cuklt-quiet (c-function int ("cuklt_is_quiet")))
 (if (= 1 (c-cuklt-quiet)) #t #f))

(define (cuklt-set-quiet! quiet?)
 (define c-cuklt-set-quiet!
  (c-function void ("cuklt_set_quiet" int)))
 (c-cuklt-set-quiet! (if quiet? 1 0)))

(define cuklt-detect!
 ;; Process a single image/frame and return the processed frame number.
 ;;    (cupklt-detect cuklt pointer-to-argb-pixels)
 ;; The frame number should match the video frame number (frame starts from 0).
 (c-function int ("cuklt_detect" pointer pointer)))

(define cuklt-n-frames
 ;; Get the total number of frames processed so far.
 (c-function int ("cuklt_n_frames" pointer)))

(define cuklt-n-features
 ;; Get number of features from a previous processed frame as follows:
 ;;   (cuklt-n-features cuklt frame-idx)
 (c-function int ("cuklt_n_features" pointer int)))

(define cuklt-id-features
 ;; Get features' id field from a previous processed frame as follows:
 ;;   (cuklt-id-features cuklt frame-idx)
 (c-function pointer ("cuklt_id_features" pointer int)))

(define cuklt-x-features
 ;; Get features' x field from a previous processed frame as follows:
 ;;   (cuklt-x-features cuklt frame-idx)
 (c-function pointer ("cuklt_x_features" pointer int)))

(define cuklt-y-features
 ;; Get features' y field from a previous processed frame as follows:
 ;;   (cuklt-y-features cuklt frame-idx)
 (c-function pointer ("cuklt_y_features" pointer int)))

(define (with-imlib-image-context! f image)
 (imlib-context-set-image! image)
 (let ((result (f image)))
  (imlib-free-image)
  result))

(define (video-dimensions video)
 (with-ffmpeg-video
  video
  (lambda (ffmpeg-video)
   (with-imlib-image-context!
    (lambda (image) (vector (imlib-get-image-width)
			    (imlib-get-image-height)))
    (ffmpeg-video-frame-data-as-imlib ffmpeg-video)))))

(define (video-scale video)
 (/ (x (video-dimensions video)) 1280))

(define (video-info video)
 (with-ffmpeg-video
  video
  (lambda (ffmpeg-video)
   (list (ffmpeg-video-width ffmpeg-video)
         (ffmpeg-video-height ffmpeg-video)
         (ffmpeg-video-frame-rate ffmpeg-video)))))

(define (with-cuklt f
		    width height max-num-features
		    pyramid-levels mask-size template-size smooth-size)
 (let ((cuklt (cuklt-new)))
  (cuklt-init-long! cuklt width height max-num-features
		    pyramid-levels mask-size template-size smooth-size)
  (let ((result (f cuklt)))
   (cuklt-delete cuklt)
   result)))

(define (get-tracked-cuklt-features current-frame-features
				    next-frame-features)
 (removeq #f (map (lambda (next)
		   (let ((current
			  (find-if
			   (lambda (current)
			    (= (first current) (first next)))
			   current-frame-features)))
		    (if current
                        (make-klt-pair (second current) (second next))
			#f)))
		  next-frame-features)))

(define (run-cuklt video max-num-features pyramid-levels
		   mask-size template-size smooth-size)
 (let* ((dimensions (video-dimensions video)))
  (with-cuklt
   (lambda (cuklt)
    (for-each-imlib-frame-from-video-indexed
     (lambda (video-frame-number index imlib-image-handle)
      (with-imlib-image-context!
       (lambda (image) (cuklt-detect! cuklt (imlib-get-data-ptr-read-only)))
       imlib-image-handle))
     video)
    (let ((features-movie (cuklt-video-features cuklt video)))
     (map (lambda (currents nexts)
	   (get-tracked-cuklt-features currents nexts))
	  (but-last features-movie)
	  (rest features-movie))))
   (x dimensions) (y dimensions) max-num-features
   pyramid-levels mask-size template-size smooth-size)))

(define (run-cuklt-with-defaults video) (run-cuklt video 1000 3 15 11 23))

(define (cuklt-frame-features cuklt frame)
 (if (< frame 0)
     (list (list -1 (vector 0 0)))  ; TC: a hack..
     (map
      list
      (c-exact-array->list (cuklt-id-features cuklt frame)
                           c-sizeof-int
                           (cuklt-n-features cuklt frame)
                           #t)
      (map
       vector
       (c-inexact-array->list (cuklt-x-features cuklt frame)
                              c-sizeof-double
                              (cuklt-n-features cuklt frame)
                              #t)
       (c-inexact-array->list (cuklt-y-features cuklt frame)
                              c-sizeof-double
                              (cuklt-n-features cuklt frame)
                              #t)))))

(define (cuklt-video-features cuklt video)
 (map-frame-indexed (lambda (_ index) (cuklt-frame-features cuklt index)) video))

(define (run-klt-cuda video width height max-num-features
                      pyramid-levels mask-size template-size smooth-size)
 (let ((cuklt (cuklt-new)))
  (cuklt-init-long! cuklt width height max-num-features
		    pyramid-levels mask-size template-size smooth-size)
  (let
    ((result
      (map-imlib-frame-from-video-indexed
       (lambda (video-frame-number index imlib-image-handle)
        (with-imlib-image-context!
         (lambda (image) (cuklt-detect! cuklt (imlib-get-data-ptr-read-only)))
         imlib-image-handle)
        (let* ((current (cuklt-frame-features cuklt (- index 1)))
               (next (cuklt-frame-features cuklt index))
               (result (get-tracked-cuklt-features current next)))
         ;; (when *callback-function*
         (when (and (> index 0) *callback-function*) ; hack to do first 2 frames
          (call-with-current-continuation
           (lambda (k)
            (set! *klt-continuation* k)
            (set! *klt-movie*
                  (append *klt-movie* (list result)))
            (*callback-function* #f))))
         result))
       video)))
   (set! *klt-continuation* #f)
   (cuklt-delete cuklt)
   (when *callback-function* (*callback-function* #f))
   result)))


(define (get-model-names-list video)
 (transitive-equivalence-classesp
  (lambda (a b) (if (and (prefix? "person" a) (prefix? "person" b)) #t #f))
  (map second (video-voc4-detector-boxes-available video))))

(define (smooth-box-movie box-movie ncx ncy nw nh)
 (scheme->matlab! "boxes" (list->vector (map box->vector box-movie)))
 (scheme->matlab!
  "skip"
  (list->vector
   (map (lambda (box)
	 (if (and (nondropped-box? box) (voc4-detection-model box)) 0 1))
	box-movie)))
 (matlab-eval-strings
  "addpath('~/darpa-collaboration/splines');"
  "box_centre = @(x) [(x(1,1)+x(1,3))/2 (x(1,2)+x(1,4))/2];"
  "centres = cell2mat(cellfun(box_centre,num2cell(boxes,2),'UniformOutput',false));"
  (format #f "ts = [1:~s];" (length box-movie))
  "new_ts = ts(skip()==0);"
  "new_centres = double(centres(skip()==0,:));"
  "widths = abs(boxes(:,1)-boxes(:,3));"
  "heights = abs(boxes(:,2)-boxes(:,4));"
  "new_widths = double(widths(skip()==0));"
  "new_heights = double(heights(skip()==0));"
  (format #f "ppx = splinefit(new_ts,new_centres(:,1),~s,4);" ncx)
  (format #f "ppy = splinefit(new_ts,new_centres(:,2),~s,4);" ncy)
  "smx = ppval(ppx,ts);"
  "smy = ppval(ppy,ts);"
  (format #f "ppw = splinefit(new_ts,new_widths(:),~s,4);" nw)
  (format #f "pph = splinefit(new_ts,new_heights(:),~s,4);" nh)
  "smw = ppval(ppw,ts);"
  "smh = ppval(pph,ts);")
 (map (lambda (x y w h box)
       (if (nondropped-box? box)
	   (make-voc4-detection (- x (/ w 2))
				(- y (/ h 2))
				(+ x (/ w 2))
				(+ y (/ h 2))
				(voc4-detection-parts box)
				(voc4-detection-filter box)
				(voc4-detection-strength box)
				(voc4-detection-delta box)
				(voc4-detection-ringing box)
				(voc4-detection-model box))
	   box))
      (vector->list (x (matlab-get-variable "smx")))
      (vector->list (x (matlab-get-variable "smy")))
      (vector->list (x (matlab-get-variable "smw")))
      (vector->list (x (matlab-get-variable "smh")))
      box-movie))

;;; zip high-level API

(define (zip:read-file zip name-or-index)
 (let* ((buffer (zip:read-file-to-buffer zip name-or-index))
	(string (c-string->string (x buffer))))
  (free (x buffer))
  (lines string)))

(define (zip:read-file-to-buffer zip name-or-index)
 ;; TODO for now this must be freed, finalizers
 (let* ((index (zip:index zip name-or-index))
	(stat (zip:stat-index zip index))
	(zip-file (zip:fopen-index zip index))
	(data (zip:fread-to-buffer zip-file (zip-file-stat-size stat))))
  (zip:fclose zip-file)
  (vector data (zip-file-stat-size stat))))

(define (zip:get-archive-comment archive)
 ((c-function string ("zip_get_archive_comment" zip pointer int))
  archive 0 0))

;; only in the new libzip
;; (define (zip:set-archive-comment zip string)
;;  (when
;;    (= -1 ((c-function int ("zip_set_archive_comment" zip string int))
;; 	  zip string (string-length string)))
;;   (fuck-up)))

(define zip:error-clear (c-function void ("zip_error_clear" zip)))

(define (zip:error-get zip)
 (with-alloc
  c-sizeof-int
  (lambda (zip-error-ptr)
   (with-alloc
    c-sizeof-int
    (lambda (system-error-ptr)
     ((c-function void ("zip_error_get" pointer pointer pointer))
      zip zip-error-ptr system-error-ptr)
     (cons (c-int-ref zip-error-ptr 0) (c-int-ref system-error-ptr 0)))))))

(define (zip:delete zip name-or-index)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function int ("zip_delete" zip long)) zip
    (zip:index zip name-or-index)))
  zip))

(define (zip:rename zip name-or-index destination-name)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function int ("zip_rename" zip long string)) zip
    (zip:index zip name-or-index) destination-name))
  zip))

;; TODO zip:write-* which either adds or replaces

(define (zip:add-file-from-buffer zip name buffer size free?)
 (zip:add-source zip name (zip:source-buffer zip buffer size free?)))
(define (zip:add-file-from-pathname zip name source-name)
 (zip:add-source zip name (zip:source-file zip source-name)))
(define (zip:add-file-from-zip zip zip-source name source-index)
 (zip:add-source zip name (zip:source-zip zip zip-source source-index)))
(define (zip:add-file zip name strings)
 (let* ((string (unlines strings))
	(buffer (malloc (string-length string))))
  (memcpy buffer string (string-length string))
  (zip:add-source
   zip name
   (zip:source-buffer zip buffer (string-length string) #t))))

(define (zip:replace-file-from-buffer zip name buffer size free?)
 (zip:replace-source zip name (zip:source-buffer zip buffer size free?)))
(define (zip:replace-file-from-pathname zip name source-name)
 (zip:replace-source zip name (zip:source-file zip source-name)))
(define (zip:replace-file-from-zip zip zip-source name source-index)
 (zip:replace-source zip name (zip:source-zip zip zip-source source-index)))
(define (zip:replace-file zip name strings)
 (let ((string (unlines strings)))
  (zip:replace-source
   zip name
   (zip:source-buffer zip string (string-length string) #t))))

(define (zip:add-directory zip name)
 ;; TODO should return #F when already exists
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function long ("zip_add_dir" zip string)) zip name))
  zip))

(define (zip:ls zip)
 (map-n (lambda (n) (zip:get-name zip n)) (zip:get-num-entries zip)))

(define *zip:mode-create-if-necessary* (c-value int "ZIP_CREATE"))
(define *zip:mode-open* 0)
(define *zip:mode-create-new* (bit-or (c-value int "ZIP_CREATE") (c-value int "ZIP_EXCL")))
(define *zip:mode-extra-checks* (c-value int "ZIP_CHECKCONS"))

(define (zip:open filename mode)
 (with-alloc
  c-sizeof-int
  (lambda (error-ptr)
   (let ((zip ((c-function pointer ("zip_open" string int pointer))
	       filename mode error-ptr))
	 (errno (c-value int "errno")))
    (when (zero? zip)
     (let ((buffer-size
	    (+ (zip:error-to-str 0 0 (c-int-ref error-ptr 0) errno) 1)))
      (with-alloc
       buffer-size
       (lambda (buffer)
	(zip:error-to-str buffer buffer-size (c-int-ref error-ptr 0) errno)
	(panic "zip-open error ~a ~a~%" filename (c-string->string buffer))))))
    (make-zip-archive zip)))))

(define (zip:close zip)
 (zip:with-fail-on-error!
  (lambda (zip)
   ;; TODO zip_close doesn't free up the memory if it fails
   ((c-function int ("zip_close" zip)) zip))
  zip)
 (set-zip-archive-handle! zip #f)
 zip)

(define (zip:stat zip name-or-index)
 (zip:stat-index zip (zip:index zip name-or-index)))

;;; zip low level API

;; flags for zip_open, ORed together
(define zip:const-create (c-value int "ZIP_CREATE"))
(define zip:const-excl (c-value int "ZIP_EXCL"))
(define zip:const-checkcons (c-value int "ZIP_CHECKCONS"))
;; flags for zip_name_locate, zip_fopen, zip_stat, ...
(define zip:const-fl-nocase (c-value int "ZIP_FL_NOCASE"))
(define zip:const-fl-nodir (c-value int "ZIP_FL_NODIR"))
(define zip:const-fl-compressed (c-value int "ZIP_FL_COMPRESSED"))
(define zip:const-fl-unchanged (c-value int "ZIP_FL_UNCHANGED"))
(define zip:const-fl-recompress (c-value int "ZIP_FL_RECOMPRESS"))
;; only in the new libzip
;; (define zip:const-fl-encrypted (c-value int "ZIP_FL_ENCRYPTED"))
;; archive global flags flags
(define zip:const-afl-torrent (c-value int "ZIP_AFL_TORRENT"))
;; only in the new libzip
;; (define zip:const-afl-rdonly (c-value int "ZIP_AFL_RDONLY"))
;; flags for compression and encryption sources
;; only in the new libzip
;; (define zip:const-codec-encode (c-value int "ZIP_CODEC_ENCODE"))
;; libzip error codes
(define zip:const-er-ok (c-value int "ZIP_ER_OK"))
(define zip:const-er-multidisk (c-value int "ZIP_ER_MULTIDISK"))
(define zip:const-er-rename (c-value int "ZIP_ER_RENAME"))
(define zip:const-er-close (c-value int "ZIP_ER_CLOSE"))
(define zip:const-er-seek (c-value int "ZIP_ER_SEEK"))
(define zip:const-er-read (c-value int "ZIP_ER_READ"))
(define zip:const-er-write (c-value int "ZIP_ER_WRITE"))
(define zip:const-er-crc (c-value int "ZIP_ER_CRC"))
(define zip:const-er-zipclosed (c-value int "ZIP_ER_ZIPCLOSED"))
(define zip:const-er-noent (c-value int "ZIP_ER_NOENT"))
(define zip:const-er-exists (c-value int "ZIP_ER_EXISTS"))
(define zip:const-er-open (c-value int "ZIP_ER_OPEN"))
(define zip:const-er-tmpopen (c-value int "ZIP_ER_TMPOPEN"))
(define zip:const-er-zlib (c-value int "ZIP_ER_ZLIB"))
(define zip:const-er-memory (c-value int "ZIP_ER_MEMORY"))
(define zip:const-er-changed (c-value int "ZIP_ER_CHANGED"))
(define zip:const-er-compnotsupp (c-value int "ZIP_ER_COMPNOTSUPP"))
(define zip:const-er-eof (c-value int "ZIP_ER_EOF"))
(define zip:const-er-inval (c-value int "ZIP_ER_INVAL"))
(define zip:const-er-nozip (c-value int "ZIP_ER_NOZIP"))
(define zip:const-er-internal (c-value int "ZIP_ER_INTERNAL"))
(define zip:const-er-incons (c-value int "ZIP_ER_INCONS"))
(define zip:const-er-remove (c-value int "ZIP_ER_REMOVE"))
(define zip:const-er-deleted (c-value int "ZIP_ER_DELETED"))
;; only in the new libzip
;; (define zip:const-er-encrnotsupp (c-value int "ZIP_ER_ENCRNOTSUPP"))
;; (define zip:const-er-rdonly (c-value int "ZIP_ER_RDONLY"))
;; (define zip:const-er-nopasswd (c-value int "ZIP_ER_NOPASSWD"))
;; (define zip:const-er-wrongpasswd (c-value int "ZIP_ER_WRONGPASSWD"))

;; type of system error value
(define zip:const-et-none (c-value int "ZIP_ET_NONE"))
(define zip:const-et-sys (c-value int "ZIP_ET_SYS"))
(define zip:const-et-zlib (c-value int "ZIP_ET_ZLIB"))
;; compression methods
(define zip:const-cm-default (c-value int "ZIP_CM_DEFAULT"))
(define zip:const-cm-store (c-value int "ZIP_CM_STORE"))
(define zip:const-cm-shrink (c-value int "ZIP_CM_SHRINK"))
(define zip:const-cm-reduce-1 (c-value int "ZIP_CM_REDUCE_1"))
(define zip:const-cm-reduce-2 (c-value int "ZIP_CM_REDUCE_2"))
(define zip:const-cm-reduce-3 (c-value int "ZIP_CM_REDUCE_3"))
(define zip:const-cm-reduce-4 (c-value int "ZIP_CM_REDUCE_4"))
(define zip:const-cm-implode (c-value int "ZIP_CM_IMPLODE"))
(define zip:const-cm-deflate (c-value int "ZIP_CM_DEFLATE"))
(define zip:const-cm-deflate64 (c-value int "ZIP_CM_DEFLATE64"))
(define zip:const-cm-pkware-implode (c-value int "ZIP_CM_PKWARE_IMPLODE"))
(define zip:const-cm-bzip2 (c-value int "ZIP_CM_BZIP2"))
(define zip:const-cm-lzma (c-value int "ZIP_CM_LZMA"))
(define zip:const-cm-terse (c-value int "ZIP_CM_TERSE"))
(define zip:const-cm-lz77 (c-value int "ZIP_CM_LZ77"))
(define zip:const-cm-wavpack (c-value int "ZIP_CM_WAVPACK"))
(define zip:const-cm-ppmd (c-value int "ZIP_CM_PPMD"))
;; encryption methods
(define zip:const-em-none (c-value int "ZIP_EM_NONE"))
(define zip:const-em-trad-pkware (c-value int "ZIP_EM_TRAD_PKWARE"))
(define zip:const-em-unknown (c-value int "ZIP_EM_UNKNOWN"))
;;
(define zip:const-source-open (c-value int "ZIP_SOURCE_OPEN"))
(define zip:const-source-read (c-value int "ZIP_SOURCE_READ"))
(define zip:const-source-close (c-value int "ZIP_SOURCE_CLOSE"))
(define zip:const-source-stat (c-value int "ZIP_SOURCE_STAT"))
(define zip:const-source-error (c-value int "ZIP_SOURCE_ERROR"))
(define zip:const-source-free (c-value int "ZIP_SOURCE_FREE"))

;; only in the new libzip
;; (define zip:const-source-err-lower (c-value int "ZIP_SOURCE_ERR_LOWER"))

;; only in the new libzip
;; (define zip:const-stat-name (c-value int "ZIP_STAT_NAME"))
;; (define zip:const-stat-index (c-value int "ZIP_STAT_INDEX"))
;; (define zip:const-stat-size (c-value int "ZIP_STAT_SIZE"))
;; (define zip:const-stat-comp-size (c-value int "ZIP_STAT_COMP_SIZE"))
;; (define zip:const-stat-mtime (c-value int "ZIP_STAT_MTIME"))
;; (define zip:const-stat-crc (c-value int "ZIP_STAT_CRC"))
;; (define zip:const-stat-comp-method (c-value int "ZIP_STAT_COMP_METHOD"))
;; (define zip:const-stat-encryption-method (c-value int "ZIP_STAT_ENCRYPTION_METHOD"))
;; (define zip:const-stat-flags (c-value int "ZIP_STAT_FLAGS"))

;; TODO this should be uint64_t, only in the new libzip
;; (c-define-struct-field "zip_stat" "valid" long)
;; TODO should add string support
(c-define-struct-field "zip_stat" "name"  pointer)
;; TODO this should be unit64_t
(c-define-struct-field "zip_stat" "index" long)
;; TODO this should be unit64_t
(c-define-struct-field "zip_stat" "size"  long)
;; TODO this should be unit64_t
(c-define-struct-field "zip_stat" "comp_size"  long)
;; TODO this is time_t
;; (c-define-struct-field "zip_stat" "mtime"  time)
(c-define-struct-field "zip_stat" "crc"  int)
;; TODO this should be unit64_t
(c-define-struct-field "zip_stat" "comp_method"  long)
;; TODO this should be unit64_t
(c-define-struct-field "zip_stat" "encryption_method"  long)
;; TODO this should be unit64_t, only in the new libzip
;; (c-define-struct-field "zip_stat" "flags"  long)

;; TODO long should be uint64_t or int64_t all over
(define (zip:add-source zip name zip-source)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function long ("zip_add" zip string zip-source)) zip name zip-source))
  zip))
(define (zip:replace-source zip name-or-index zip-source)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function int ("zip_replace" zip long zip-source))
    zip (zip:index zip name-or-index) zip-source))
  zip))

(define zip:error-get-sys-type (c-function int ("zip_error_get_sys_type" int)))
(define zip:error-to-str (c-function int ("zip_error_to_str" pointer long int int)))

(define zip:file-error-clear (c-function void ("zip_file_error_clear" pointer)))
(define zip:file-error-get (c-function void ("zip_file_error_get" pointer pointer pointer)))
(define zip:file-strerror (c-function string ("zip_file_strerror" pointer)))

;; only in the new libzip
;; (define zip:fopen-encrypted (c-function pointer ("zip_fopen_encrypted" pointer string int string)))
;; (define zip:fopen-index-encrypted (c-function pointer ("zip_fopen_index_encrypted" pointer long int string)))

(define (zip:fopen zip name-or-index)
 (zip:fopen-index zip (zip:index zip name-or-index)))
(define (zip:fopen-index zip index)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function zip-file ("zip_fopen_index" zip long int)) zip index 0))
  zip))
(define (zip:fclose zip-file)
 (unless (zero? ((c-function int ("zip_fclose" zip-file)) zip-file))
  (fuck-up)))
(define (zip:fread-to-buffer zip-file size)
 ;; TODO for now this must be freed, finalizers
 (let* ((buffer (malloc (+ size 1)))
	(read ((c-function int ("zip_fread" zip-file pointer long))
	       zip-file buffer (+ size 1))))
  (when (= -1 read) (fuck-up))
  (c-byte-set! buffer size 0)
  buffer))
(define (zip:fread zip-file size)
 (with-alloc
  (+ size 1)
  (lambda (buffer)
   (let ((read ((c-function int ("zip_fread" zip-file pointer long))
		zip-file buffer (+ size 1))))
    (when (= -1 read) (fuck-up))
    (let ((string (make-string (+ size 1))))
     (for-each-n
      (lambda (n) (string-set! string n
			       (integer->char (c-byte-ref buffer n))))
      size)
     string)))))

(define zip:get-archive-flag (c-function int ("zip_get_archive_flag" pointer int int)))

;; only in the new libzip
;; (define zip:get-file-comment (c-function string ("zip_get_file_comment" pointer long pointer int)))
;; (define zip:get-file-extra (c-function string ("zip_get_file_extra" pointer long pointer int)))

(define (zip:get-name zip index)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function string ("zip_get_name" zip long int)) zip index 0))
  zip))

(define (zip:get-num-entries zip)
 ((c-function long ("zip_get_num_files" zip)) zip)
 ;; only int he new libzip
 ;; ((c-function long ("zip_get_num_entries" zip int)) zip 0)
 )

(define (zip:name-locate zip name)
 ;; TODO should return #F when not found
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function int ("zip_name_locate" zip string int)) zip name 0))
  zip
  name))

;; only in the new libzip
;; (define zip:fdopen (c-function pointer ("zip_fdopen" int int pointer)))

(define zip:set-archive-flag (c-function int ("zip_set_archive_flag" zip int int)))
;; only in the new libzip
;; (define zip:set-default-password (c-function int ("zip_set_default_password" zip string)))
(define zip:set-file-comment (c-function int ("zip_set_file_comment" zip long string int)))
;; only in the new libzip
;; (define zip:set-file-extra (c-function int ("zip_set_file_extra" zip long string int)))

(define (zip:source-buffer zip buffer size free?)
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function zip-source ("zip_source_buffer" zip pointer long bool))
    zip buffer size free?))
  zip))
(define (zip:source-file zip filename . range)
 ;; reads the entire file when no range is passed in
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function zip-source ("zip_source_file" zip string long long))
    zip filename
    (if (null? range) 0 (first range))
    (if (null? range) 0 (second range))))
  zip))
(define (zip:source-zip zip-destination zip-source source-index . range)
 ;; reads the entire file when no range is passed in
 (zip:with-fail-on-error!
  (lambda (zip)
   ((c-function zip-source ("zip_source_zip" zip zip long int long long))
    zip-destination zip-source source-index 0
    (if (null? range) 0 (first range))
    (if (null? range) 0 (second range))))
  zip))
(define zip:source-function (c-function zip-source ("zip_source_function" zip pointer pointer)))
(define zip:source-filep (c-function zip-source ("zip_source_filep" zip pointer long long)))
;; Note that ownership of sources is transferred with zip_add or
;; zip_replace, no need to free in that case
(define zip:source-free (c-function void ("zip_source_free" zip-source)))

(define (zip:index zip name-or-index)
 (cond ((number? name-or-index) name-or-index)
       ((string? name-or-index) (zip:name-locate zip name-or-index))
       (else (fuck-up))))

(define (zip:stat-index zip index)
 (with-alloc
  (c-sizeof "struct zip_stat")
  (lambda (zip-stat)
   (zip:stat-init zip-stat)
   (zip:with-fail-on-error!
    (lambda (zip)
     ((c-function int ("zip_stat_index" zip long int pointer))
      zip index 0 zip-stat))
    zip)
   (make-zip-file-stat (zip-stat-name zip-stat)
		       (zip-stat-index zip-stat)
		       (zip-stat-size zip-stat)
		       (zip-stat-comp-size zip-stat)))))

(define zip:stat-init (c-function void ("zip_stat_init" pointer)))

(define zip:unchange (c-function int ("zip_unchange" zip long)))
(define zip:unchange-all (c-function int ("zip_unchange_all" zip)))
(define zip:unchange-archive (c-function int ("zip_unchange_archive" zip)))

(define zip:strerror (c-function string ("zip_strerror" zip)))

(define (zip:fail-on-error! zip)
 ;; TODO perfrom a less expensive check here
 (unless (equal? (zip:strerror zip) "No error")
  (panic "Zip error: ~a~%" (zip:strerror zip))))

(define (zip:with-fail-on-error! f zip . message)
 (let ((result (f zip)))
  ;; TODO perfrom a less expensive check here
  (unless (equal? (zip:strerror zip) "No error")
   (panic "Zip error: ~a~a~%"
	  (zip:strerror zip)
	  (if (null? message) "" (format #f ". ~a" (first message)))))
  result))

;;; zip files

(define (with-zip-file f filename mode)
 (let*  ((zip (zip:open filename mode)) (result (f zip)))
  (zip:close zip)
  result))

;;; -------------------------------------------------------------------- Pedro Detector

;;; Converts a ppm image into a flat array in the same column ordering
;;; as what matlab uses: channel x width x height
(define (ppm-to-flat-matlab-compatible-vector ppm-image)
 (let* ((n-rows (vector-length (vector-ref ppm-image 3)))
	(n-cols (vector-length (vector-ref (vector-ref ppm-image 3) 1)))
	(res (make-vector (* 3 n-rows n-cols))))
  (for-each-n
   (lambda (c-idx)
    (for-each-n
     (lambda (x-idx)
      (for-each-n
       (lambda (y-idx)
	(let ((p (vector-ref (vector-ref (vector-ref ppm-image (+ c-idx 3)) y-idx) x-idx))
	      (idx (+ (* c-idx n-rows n-cols) (* x-idx n-rows) y-idx)))
	 (vector-set! res idx p)))
       n-rows))
     n-cols))
   3)
  res))

;;; Resize algorithm that Pedro Felzenszwalb uses in the "pedro-detector"
;;; THIS CODE IS INCOMPLETE -- the result (scaled-image) must be re-
;;; transformed into a ppm-image.
(define (pedro-resize ppm-image scale)
 (let* (;; Convert ppm to flat array in matlab row-column ordering
	(raw-data (ppm-to-flat-matlab-compatible-vector ppm-image))
	;; Parameters for c-function call: c-raw-data, channels, height, width
	(c-raw-data (malloc (* c-sizeof-double (vector-length raw-data))))
	(c-raw-data (vector->c-inexact-array c-raw-data raw-data c-sizeof-double #t))
	(channels 3) ; We only deal with RGB images
	(height (vector-length (vector-ref ppm-image 3)))
	(width (vector-length (vector-ref (vector-ref ppm-image 3) 1)))
	;; Scale the image
 	(c-dim3array-st-ptr
 	 ((c-function pointer ("pedro_resize_image" pointer int int int double))
 	  c-raw-data height width channels scale))
	;; Unpack the result into a
	(out-width (dim3array-cols c-dim3array-st-ptr))
	(c-scaled-image (dim3array-data c-dim3array-st-ptr))
 	(scaled-image (c-inexact-array->vector c-scaled-image c-sizeof-double 3 #t)))
  (free c-scaled-image)
  (free c-dim3array-st-ptr)
  (free c-raw-data)
  scaled-image))

;;; Optical flow

(define (imlib-to-float-greyscale! imlib-image)
 (imlib-context-set-image! imlib-image)
 ((c-function pointer ("bgra_to_float_greyscale" pointer int int))
  (imlib-get-data-ptr-read-only)
  (imlib-get-image-width) (imlib-get-image-height)))

(define (flowlib-to-c-optical-flow image1 image2 width height)
 ;; these are greyscale images of floats
 (let* ((result
	 ((c-function pointer ("flowlib_optical_flow" pointer pointer int int))
	  image1 image2
	  width height)))
  (make-c-optical-flow (integral-optical-flow-from-c
			result (imlib-get-image-width) (imlib-get-image-height))
		       (imlib-get-image-width) (imlib-get-image-height))))

(define (flowlib-to-c-optical-flow-non-integral image1 image2 width height)
 ;; these are greyscale images of floats
 (let* ((result
	 ((c-function pointer ("flowlib_optical_flow" pointer pointer int int))
	  image1 image2
	  width height)))
  (make-c-optical-flow result (imlib-get-image-width) (imlib-get-image-height))))

(define (cuda-optical-flow video)
 ;; TODO change to use finalizers
 (set! *optical-flow-movie* '())
 (let ((free-list '()))
  (let ((result
	 (map-imlib-frame-pair-from-video
	  (lambda (frame image)
	   ;; (format #t "optical flow on frame ~a~%" frame)
	   (imlib-context-set-image! image)
	   (let* ((scaled-size (vector (/ (imlib-get-image-width) 2)
				       (/ (imlib-get-image-height) 2)))
		  (scaled (imlib-create-cropped-scaled-image
			   0 0 (imlib-get-image-width) (imlib-get-image-height)
			   (/ (imlib-get-image-width) 2) (/ (imlib-get-image-height) 2))))
	    (let ((image (imlib-to-float-greyscale! scaled)))
	     ;; FIXME Andrei this should be freed, no?
	     ;; (imlib-context-set-image! scaled)
	     ;; (imlib-free-image)
	     (set! free-list (cons image free-list))
	     (vector scaled-size image))))
	  (lambda (prev next)
	   (let ((result (flowlib-to-c-optical-flow
			  (y prev) (y next) (x (x prev)) (y (x prev)))))
	    (when *callback-function*
	     (call-with-current-continuation
	      (lambda (k)
	       (set! *optical-flow-continuation* k)
	       (set! *optical-flow-movie*
		     (append *optical-flow-movie* (list result)))
	       (*callback-function* #f))))
	    result))
	  video)))
   (map free free-list)
   (set! *optical-flow-continuation* #f)
   (when *callback-function* (*callback-function* #f))
   result)))

(define (imlib-draw-text-on-image image string colour font-size xc yc
                                  background-color)
 (imlib-context-set-image! image)
 (imlib-add-path-to-font-path (*default-font-path*))
 (let ((font (imlib-load-font! (string*-append "DejaVuSans/" font-size))))
  (imlib-context-set-font! font)
  (when background-color
   (let* ((text-width-height
           (c-exact-array->list (imlib-get-text-dimension string)
                                c-sizeof-int 2 #t)))
    (imlib-context-set-color!
     (x background-color) (y background-color) (z background-color) 255)
    (imlib-image-fill-rectangle
     xc yc (first text-width-height) (second text-width-height))))
  (imlib-context-set-color! (x colour) (y colour) (z colour) 255)
  (imlib-text-draw xc yc string)
  (imlib-free-font)))

(define (imlib-draw-histogram image data labels color alpha text-color font-size
			      bottom-spacer horizontal-spacer)
 (imlib-context-set-image! image)
 (let* ((width (imlib-get-image-width))
	(height (imlib-get-image-height))
	(bar-width (/ (- width (* horizontal-spacer (+ 1 (length data))))
                      (length data)))
	(max-data (maximum data))
	(min-data (minimum data))
        (top-spacer (/ bottom-spacer 10)))
  (for-each-indexed
   (lambda (point-and-label index)
    (imlib-context-set-color! (r color) (g color) (b color) alpha)
    (imlib-image-fill-rectangle
     (+ (* (+ horizontal-spacer bar-width) index) horizontal-spacer) ; x
     (min (- height bottom-spacer 3)
          (+ (exact-ceiling
              (* (- height bottom-spacer)
                 (- 1 (/ (- (first point-and-label) min-data)
                         (- max-data min-data))))) top-spacer)) ; y
     bar-width ; w
     (max 3 (- (exact-floor (* (- height bottom-spacer)
                               (/ (- (first point-and-label) min-data)
                                  (- max-data min-data)))) top-spacer))) ; h
    (imlib-context-set-direction imlib-text-to-up)
    (imlib-draw-text-on-image image (second point-and-label) text-color font-size
			      (* (+ horizontal-spacer bar-width) index)
			      (+ (- height bottom-spacer) (/ bottom-spacer 10))
			      #f))
   (zip data labels))
  (imlib-context-set-direction imlib-text-to-right))
 image)

(define (imlib-verb-likelihood-histogram! likelihoods width height font-size
					  bottom-spacer horizontal-spacer)
 (if (null? likelihoods) ;; check to see if any track is generated
     #f
     (let ((image (imlib-create-image width height)))
      (imlib-context-set-color! 255 255 255 255)
      (imlib-context-set-image! image)
      (imlib-image-fill-rectangle 0 0 (imlib-get-image-width)
                                  (imlib-get-image-height))
      (let* ((likelihoods
              (remove
               #f
               (map
                (lambda (verb)
                 (find-if (lambda (r) (equal? (hyphens->spaces (first verb))
                                              (hyphens->spaces (result-verb r))))
                          likelihoods))
                *verbs*)))
             (max-val (apply max (map result-loglk likelihoods))))
       (imlib-draw-histogram
        image
        (map (lambda (a) a)
             (map (lambda (a) (/ a)) (map (lambda (val)
                                           (abs (- val max-val 1)))
                                          (map result-loglk likelihoods))))
        (map result-verb likelihoods)
        '#(0 0 255) 255 '#(0 0 0) font-size
        bottom-spacer horizontal-spacer)))))


(define (draw-on-image-from-boxes-details image thickness names boxes colours name?
					  font-size parts? rank-box-colors?)
 ;; Assumes that boxes are scaled correctly
 (for-each
  (lambda (name-or-named boxes colour)
   (let ((name (if (list? name-or-named) name-or-named `(#f ,name-or-named #f #f))))
    (for-each-indexed
     (lambda (box index)
      (when name?
       (draw-box-name-on-image
	image box
	(format #f "~a" (or (voc4-detection-model box) (second name)))
	colour
	font-size))
      (let ((graded-colour
	     (if rank-box-colors?
		 (grade-colour colour (* 0.8 (/ index
						(if (list? boxes)
						    (length boxes)
						    1))))
		 colour)))
       (draw-box-on-image image (voc4-detection->coordinates box) thickness graded-colour)
       (when parts?
	(for-each (lambda (part) (draw-box-on-image image part thickness '#(0 255 0)))
		  (voc4-detection-parts box)))))
     (if (list? boxes) boxes (list boxes)))))
  names boxes colours))

(define (draw-single-f boxes video-name image frame index thickness name?
		       colours parts? frame-name f rank-box-colors?)
 (imlib-context-set-image! image)
 (draw-on-image-from-boxes-details
  image thickness
  (map first boxes)
  (map (lambda (a) (list-ref (second a) index)) boxes)
  colours name?
  15 parts? rank-box-colors?)
 (f)
 (write-out-frame video-name frame frame-name))

(define (render-sentence-9-tile-frames
	 video sentence colours
	 detected-boxes predicted-boxes tracked-boxes smooth-tracked-boxes
	 optical-flow-movie klt-movie tile-width tile-height
	 thickness caption-height frame-name rectangle-width rectangle-height
         histogram-image rank-box-colors?)
 (for-each-imlib-frame-from-video-indexed-but-last
  (lambda (frame index image)
   (imlib-context-set-image! image)
   (let* ((caption1-y (* 3 tile-height))
	  (output-image
	   (imlib-create-image
	    (* 3 tile-width)
	    (+ (* 3 tile-height) (* 2 caption-height))))
          (clean-video-name (pregexp-replace "-\\d*x\\d*@\\d*.+"
                                             (any-video->string video) ""))
          (downsample-name (pregexp-match "\\d*x\\d*@\\d*"
                                          (any-video->string video))))
    (imlib-context-set-image! output-image)
    (imlib-context-set-color! 0 0 0 255)
    (imlib-image-fill-rectangle
     0 0 (imlib-get-image-width) (imlib-get-image-height))
    (imlib-context-set-color! 255 255 255 255)
    (imlib-image-fill-rectangle
     0 caption1-y (imlib-get-image-width) (* 2 caption-height))
    (imlib-context-set-image! output-image)
    (draw-9-tiles
     image detected-boxes predicted-boxes tracked-boxes
     smooth-tracked-boxes optical-flow-movie klt-movie
     output-image tile-width tile-height frame index thickness colours
     rectangle-width rectangle-height
     histogram-image                    ; histogram
     #t
     caption-height
     (if downsample-name
         (format #f "~a (~a)" clean-video-name (first downsample-name))
         (any-video->string video))
     sentence rank-box-colors?)
    (imlib-context-set-image! output-image)
    (imlib-draw-text-on-image
     output-image (if sentence
                      (first sentence)
                      "no sentence produced")
     '#(0 0 0)
     (/ caption-height 2)               ; font size
     20					; x coord of upper left
     caption1-y #f)
    (imlib-context-set-image! output-image)
    (write-out-frame video frame frame-name)
    (imlib-free-images-and-decache (list image output-image))
    (any-video->string video)))
  video))

(define (draw-9-tiles
	 raw-image
	 detected-boxes predicted-boxes
	 tracked-boxes
	 smooth-tracked-boxes
	 optical-flow-movie
	 klt-movie
	 composite-image tile-width tile-height frame index thickness colours
	 rectangle-width rectangle-height likelihoods-histogram-image
         post-processing? caption-height caption sentence rank-box-colors?)
 ;;; TODO This should take the boxes only for the current frame rather
 ;;      than for the entire video
 ;;; TODO There is roundoff error when blending the images, if the width
 ;;;      and height are not divisible by 3 we'll get an ugly row of pixles
 ;;;      somewhere as imlib only renders to integral coordinates
 ;;; NOTE: composite-image must be at least (tile-width*3 x tile-height*3)
 ;;; NOTE: the boxes are named-boxes and the dimension is always in 1280x720
 ;;; coordinate.
 (define (downsample-boxes boxes scale)
  (map (lambda (name-box-pair)
        (list (first name-box-pair)
              (if (list? (first (second name-box-pair)))
                  (scale-detector-output (second name-box-pair) scale)
                  (first (scale-detector-output
                          (list (second name-box-pair)) scale)))))
       boxes))
 (define (draw image boxes name? font-size)
  (let ((boxes-list (map (lambda (a) (list-ref (second a) index)) boxes)))
   (draw-on-image-from-boxes-details image
				     thickness
				     (map first boxes)
				     boxes-list
				     colours name? font-size #f rank-box-colors?)))
 (let* ((height
	 (begin (imlib-context-set-image! composite-image)
		(imlib-get-image-height)))
	(width (imlib-get-image-width))
	(raw-height (begin (imlib-context-set-image! raw-image)
			   (imlib-get-image-height)))
	(raw-width (imlib-get-image-width))
	(font-size (/ raw-height 20))
	(images (begin (imlib-context-set-image! raw-image)
		       (map-n (lambda _ (imlib-clone-image)) 9)))
        (scale (/ raw-width 1280)) ; detector box is always in 1280x720 coord.
        (detected-boxes-resampled (downsample-boxes
                                   detected-boxes scale))
        (predicted-boxes-resampled (downsample-boxes
                                    predicted-boxes scale))
        (tracked-boxes-resampled (downsample-boxes
                                  tracked-boxes scale))
        (smooth-tracked-boxes-resampled (downsample-boxes
                                         smooth-tracked-boxes scale)))
  (when (or (< (/ width 3) tile-width) (< (/ height 3) tile-height)) (fuck-up))
  ;;; 9 images
  ;;; raw(0), klt(1), optical-flow(2)
  ;;; detected-boxes(3), predicted boxes(4), raw(5)
  ;;; tracked-boxes(6), smooth-tracked-boxes(7), raw(8)
  ;;;-------------------------------------------------------
  ;;; Draw images on raw video frame, (may not be 1280x720)
  ;;;-------------------------------------------------------
  ;;; Step 1.) KTL image: (IMG1)
  (when klt-movie
   (draw-klt (list-ref images 1) (list-ref klt-movie index)
             thickness (list-ref colours 0)))
  ;;; Step 2.) Optical Flow: (IMG2)
  (when optical-flow-movie
   (draw-optical-flow (list-ref images 2)
        	      (list-ref optical-flow-movie index)
        	      rectangle-width rectangle-height
        	      thickness (list-ref colours 0)
        	      ;; TODO Scaling by 2 is hardcoded
        	      (/ 2)))
  ;;; Step 3.) Detected+legend image: (IMG3)
  (draw (list-ref images 3) detected-boxes-resampled #f font-size)
  (draw-legend-on-image
   (list-ref images 3) (map second (map first detected-boxes-resampled))
   colours font-size)
  (when post-processing?
   ;;; Step 4.) Predicted+legend (IMG4) tracked (IMG6), smooth tracked (IMG7)
   ;;; images:
   (draw (list-ref images 4) predicted-boxes-resampled #f font-size)
   (draw-legend-on-image (list-ref images 4)
                         (map second
                              (map first
                                   (remove '() predicted-boxes-resampled)))
                         colours font-size)
   (draw (list-ref images 6) tracked-boxes-resampled #t font-size)
   (draw (list-ref images 7) smooth-tracked-boxes-resampled #t font-size)
   ;;; Step 5.) Likelihoods image: (IMG5)
   (when likelihoods-histogram-image
    (imlib-context-set-image! likelihoods-histogram-image)
    (let ((likelihoods-histogram-w (imlib-get-image-width))
          (likelihoods-histogram-h (imlib-get-image-height)))
     (imlib-context-set-image! composite-image)
     (imlib-blend-image-onto-image
      likelihoods-histogram-image 0
      0 0 likelihoods-histogram-w likelihoods-histogram-h
      ;; TODO: Dont hard-code the blending location.
      (* tile-width 2) tile-height tile-width tile-height)))
   ;;; Step 6.) captioned image: (IMG8)
   (when sentence
    (draw (list-ref images 8)
	  (downsample-boxes (fourth sentence) scale) #t font-size)))
   ;;; Step 7.) Caption
  (when caption
   (imlib-context-set-image! composite-image)
   (imlib-draw-text-on-image
    composite-image caption '#(0 0 0)
    (/ caption-height 2)                ; font size
    20                                  ; x,y coord of upper left
    (- (imlib-get-image-height) caption-height) #f))
  ;;;-----------------------------
  ;;; ASPECT RATIO ADJUSTMENT:
  ;;;-----------------------------
  (let* ((new-images (map (lambda (image)
                           (create-padded-image image tile-width
                                                tile-height)) images)))
   (imlib-context-set-image! composite-image)
   (imlib-blend-image-onto-image
    (list-ref new-images 0) 0 0 0 tile-width tile-height
    0                 0 tile-width tile-height)
   (imlib-blend-image-onto-image
    (list-ref new-images 1) 0 0 0 tile-width tile-height
    tile-width       0 tile-width tile-height)
   (imlib-blend-image-onto-image
    (list-ref new-images 2) 0 0 0 tile-width tile-height
    (* tile-width 2) 0 tile-width tile-height)
   (imlib-blend-image-onto-image
    (list-ref new-images 3) 0 0 0 tile-width tile-height
    0                 tile-height tile-width tile-height)
   (when post-processing?
    (imlib-blend-image-onto-image
     (list-ref new-images 4) 0 0 0 tile-width tile-height
     tile-width       tile-height tile-width tile-height)
    (imlib-blend-image-onto-image
     (list-ref new-images 6) 0 0 0 tile-width tile-height
     0                 (* tile-height 2) tile-width tile-height)
    (imlib-blend-image-onto-image
     (list-ref new-images 7) 0 0 0 tile-width tile-height
     tile-width       (* tile-height 2) tile-width tile-height)
    (imlib-blend-image-onto-image
     (list-ref new-images 8) 0 0 0 tile-width tile-height
     (* tile-width 2) (* tile-height 2) tile-width tile-height))
   (imlib-free-images-and-decache new-images))
  (imlib-free-images-and-decache images)))


(define (render-forward-projection video
				   boxes-movie
				   transformation-movie
				   delta
				   top-n
				   output-directory)
 ;; Red -- current frame
 ;; Green -- next frame
 ;; Example usage:
 ;; (define video (make-stand-alone-video "/aux/amichaux/tmp/test-video-datasets/t1/CARRY5_A1_C1_Act8_URBAN_MC_AFTN_b433a4cf-07b6-11e0-8d92-e80688cb869a"))
 ;; (define boxes-movie (read-detector-boxes video "voc4" "person"))
 ;; (define klt-movie (read-klt-movie video))
 ;; (render-forward-projection video boxes-movie klt-movie 1 1 1 "/tmp/")
 ;; (define flow-movie (read-optical-flow-movie-in-c video))
 ;; (render-forward-projection video boxes-movie flow-movie 1 1 1 "/tmp/")
 (define (draw-one image frame boxes transformation color1 color2 kind)
  ;; Draws one image
  (let ((transformation-type (cond
			      ((c-optical-flow? transformation) 'flow)
			      ((and (list? transformation)
				    (every klt-pair? transformation))
			       'klt)
			      (else
			       (panic "Transformation was neither a klt-pair nor optical-flow")))))
   (define (caption box transformation)
    ;; A caption placed onto the image
    (cond
     ((equal? 'flow transformation-type)
      (format #f "average-flow: ~a" (average-flow-in-box box transformation)))
     ((equal? 'klt transformation-type)
      (format #f "scale-and-shift: ~a" (get-scale-and-shift box transformation)))
     (else (fuck-up))))
   (imlib-context-set-image! image)
   (let* ((scale (/ (imlib-get-image-width) 1280))
	  (scaled-image (imlib-create-cropped-scaled-image 0 0
							   (imlib-get-image-width)
							   (imlib-get-image-height)
							   1280
							   (/ (* 1280 (imlib-get-image-height))
							      (imlib-get-image-width)))))
    (imlib-context-set-image! scaled-image)
    (for-each
     (lambda (box)
      (draw-imlib-line scaled-image
		       (voc4-detection-center box)
		       (voc4-detection-center
			(forward-project-box-scaled box transformation delta scale))
		       1
		       '#(0 255 255))
      (draw-box-on-image scaled-image
			 (voc4-detection->coordinates box) 1 color1)
      (draw-box-on-image scaled-image
			 (voc4-detection->coordinates
			  (forward-project-box-scaled box transformation delta scale))
			 1 color2)
      (imlib-draw-text-on-image scaled-image
				kind
				(cond ((equal? kind "current") '#(255 0 0))
				      ((equal? kind "next")    '#(0 255 0))
				      (else (fuck-up)))
				15 0 50 #f)
      (imlib-draw-text-on-image scaled-image
				(caption box transformation)
				(cond ((equal? kind "current") '#(255 0 0))
				      ((equal? kind "next")    '#(0 255 0))
				      (else (fuck-up)))
				15 0 0 #f))
     boxes)
    (imlib-context-set-image! scaled-image)
    (imlib-save-image (format #f
			      "~a/projection-~a-~a-~a.png"
			      output-directory
			      transformation-type
			      (number->padded-string-of-length frame 5) kind)))))
 (for-each-imlib-frame-pair-from-video-indexed
  list
  (lambda (nr-index-imlib1 nr-index-imlib2)
   (imlib-context-set-image! (third nr-index-imlib1))
   (let ((target1 (imlib-clone-image)))
    (imlib-context-set-image! (third nr-index-imlib2))
    (let ((target2 (imlib-clone-image)))
     (format #t "Rendering forward-projection frame ~a~%" (first nr-index-imlib1))
     (draw-one target1
	       (second nr-index-imlib1)
	       (take-if-possible top-n
				 (list-ref boxes-movie (second nr-index-imlib1)))
	       (list-ref transformation-movie (second nr-index-imlib1))
	       '#(255 0 0) '#(0 255 0) "current")
     (draw-one target2
	       (second nr-index-imlib1)
	       (take-if-possible top-n
				 (list-ref boxes-movie (second nr-index-imlib1)))
	       (list-ref transformation-movie (second nr-index-imlib1))
	       '#(255 0 0) '#(0 255 0) "next")
     (imlib-free-images-and-decache (list target1 target2)))))
  video))

(define (upgrade-old-hmm hmm-filename participants feature-type)
 (unless (trained-hmm? (read-object-from-file hmm-filename))
  (write-object-to-file
   (vector-append
    (read-object-from-file hmm-filename)
    (vector
     participants
     feature-type
     'ml))
   hmm-filename)))

(define (write-optical-flow-movie video-name optical-flow-movie)
 (rm-if-necessary (optical-flow-pathname video-name))
 (with-zip-file
  (lambda (zip2)
   (for-each-frame-indexed-but-last
    (lambda (frame offset)
     (let* ((flow (list-ref optical-flow-movie offset))
	    (buffer-size ;; TODO abstract this out
	     (+ (* (c-optical-flow-height flow) (c-optical-flow-width flow)
		   2 c-sizeof-float)
		c-sizeof-float (* 2 c-sizeof-int)))
	    (buffer (malloc buffer-size)))
      (write-flo-to-buffer flow buffer buffer-size)
      (zip:add-file-from-buffer
       zip2
       (per-frame-optical-flow-in-zip-pathname frame)
       buffer buffer-size #t)))
    video-name))
  (optical-flow-pathname video-name) *zip:mode-create-new*))

;; Colorized output

(define (text-format output attribute foreground background)
 (format output "~a[~a~a~am" (integer->char 27)
	 (if attribute (text-attribute attribute) "0")
	 (if foreground
	     (string*-append ";" (text-foreground foreground))
	     "")
	 (if background
	     (string*-append ";" (text-background background))
	     "")))

(define (reset-format) (format #t "~a[0m" (integer->char 27)))

(define (text-attribute attribute)
 (case attribute
  ((off) 0)
  ((bold) 1)
  ((underscore) 4)
  ((blink) 5)
  ((reverse) 7)
  ((conceal) 8)))

(define (text-foreground foreground)
 (case foreground
  ((black)	30)
  ((red)	31)
  ((green)	32)
  ((yellow)	33)
  ((blue)	34)
  ((magenta)	35)
  ((cyan)	36)
  ((white)	37)
  (else (fuck-up))))

(define (text-background background)
 (case background
  ((black)	40)
  ((red)	41)
  ((green)	42)
  ((yellow)	44)
  ((blue)	44)
  ((magenta)	45)
  ((cyan)	46)
  ((white)	47)
  (else (fuck-up))))

(define (format2 output attribute foreground background s . fmt)
 (text-format output attribute foreground background)
 (apply format (cons output (cons s fmt)))
 (reset-format))

;; Unit testing

(define-structure test-statistics good bad)
(define *test-offset* 0)
(define *test-statistics* (make-test-statistics 0 0))

(define-macro test
 (lambda (form e)
  (unless (or (= (length form) 3) (= (length form) 4))
   (panic "Bad test definition"))
  (e `(let*
	((expected   ,(if (= (length form) 3) (second form) (third form)))
	 (errors #f)
	 (result
	  (begin
	   (let* ((old-error error)
		  (result
		   (call-with-current-continuation
		    (lambda (k)
		     (set! error
			   ;; This setup does not catch errors in the
			   ;; format string for error itself
			   (lambda a
			    (set! errors
				  (list (car a)
					(apply format (cons #f (cdr a)))))
			    (k #f)))
		     ,(if (= (length form) 3) (third form)  (fourth form))))))
	    (set! error old-error)
	    result)))
	 (expression ',(if (= (length form) 3) (third form)  (fourth form))))
       (format #t "~aRunning ~a ["
	       (pad-right "" *test-offset*)
	       (pad-right
		(format #f "~s"
			,(if (= (length form) 3) 'expression (second form)))
		(- 60 *test-offset*)))
       (cond ((equal? expected result) (format2 #t 'bold 'green #f "   ok   "))
	     ((not errors) (format2 #t 'bold 'red #f   " FAILED "))
	     (else (format2 #t 'bold 'red #f   " ERROR  ")))
       (format #t "]~%")
       (cond (errors (format #t "~a    Error in ~a: ~a~%"
			     (pad-right "" *test-offset*)
			     (first errors) (second errors)))
	     ((not (equal? expected result))
	      (format #t "~a    Expected ~a but got ~a~%"
		      (pad-right "" *test-offset*) expected result)))
       (set! *test-statistics*
	     (make-test-statistics
	      (+ (test-statistics-good *test-statistics*)
		 (if (and (not errors) (equal? expected result)) 1 0))
	      (+ (test-statistics-bad *test-statistics*)
		 (if (and (not errors) (equal? expected result)) 0 1))))
       (and (not errors) (equal? expected result))) e)))

(define (merge-test-statistics s1 s2)
 (make-test-statistics
  (+ (test-statistics-good s1) (test-statistics-good s2))
  (+ (test-statistics-bad s1) (test-statistics-bad s2))))

(define (test-report)
 (format #t "Report ~a~%" *test-statistics*)
 (let ((all-good? (= (test-statistics-bad *test-statistics*) 0)))
  (set! *test-statistics* (make-test-statistics 0 0))
  all-good?))

(define (test-statistics-total s)
 (+ (test-statistics-good s) (test-statistics-bad s)))

(define-macro test-group
 (lambda (form e)
  (when (< (length form) 2) (panic "Bad test group definition"))
  (e `(let ((old-*test-offset* *test-offset*)
	    (old-*test-statistics* *test-statistics*)
	    (start-time (current-time)))
       (format #t "~aTesting: ~a~%" (pad-right "" *test-offset*) ,(second form))
       (set! *test-offset* (+ *test-offset* 4))
       (set! *test-statistics* (make-test-statistics 0 0))
       ,@(cddr form)
       (set! *test-offset* (- *test-offset* 4))
       (format #t "~aCompleted ~a tests: ~a% passed (~a/~a) in ~as~%"
	       (pad-right "" *test-offset*)
	       (test-statistics-total *test-statistics*)
	       (number->string-of-length-and-precision
		(* 100 (if (= (test-statistics-total *test-statistics*) 0)
			   1
			   (/ (test-statistics-good *test-statistics*)
			      (test-statistics-total *test-statistics*))))
		6 2)
	       (test-statistics-good *test-statistics*)
	       (test-statistics-total *test-statistics*)
	       (number->string-of-length-and-precision
		(- (current-time) start-time) 8 2))
       (set! *test-statistics* (merge-test-statistics old-*test-statistics* *test-statistics*))
       (= (test-statistics-bad *test-statistics*) 0))
     e)))

(define (run-sample-tests)
 (begin
  (test-group "Fun tests"
	      (test-group "Fun tests"
			  (test "Error" 1 (begin (error "A" "Grr")  1))
			  (test "Success" 1 1)
			  (test "Failure" 1 2))
	      (test "Error" 1 (begin (error "A" "Grr")  1))
	      (test "Success" 1 1)
	      (test "Failure" 1 2))
  (test 1 1)
  (test-report)))

;;; Tam V'Nishlam Shevah L'El Borei Olam
