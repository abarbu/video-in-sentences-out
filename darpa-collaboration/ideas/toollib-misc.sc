(MODULE TOOLLIB-MISC)

(include "QobiScheme-AD.sch")
(include "toollib-c-macros.sch")
(include "idealib-c-externals.sch")
(include "toollib-misc.sch")

(c-include "unistd.h")

(define *debugging?* #t)
(define *quiet?* #f)

(define (implies a b) (or (not a) b))

(define (dtrace s v) (format #t "~s ~s~%" s v) v)

(define-macro trace-call
 (lambda (f e)
  (e
   `(let ((v ,(second f)))
     (format #t "~a: " v)
     (pp ',(second f))
     (newline)
     v)
   e)))

(define (fixpointp p f v)
 (let ((a v) (b (f v))) (if (p a b) a (fixpointp p f b))))

(define (exact-round v) (inexact->exact (round v)))
(define (exact-floor v) (inexact->exact (floor v)))
(define (exact-ceiling v) (inexact->exact (ceiling v)))

(define (correct-angle a)
 (let ((mod-angle (float-modulo a pi)))
  (cond ((> mod-angle half-pi) (- mod-angle pi))
	((< mod-angle (- 0 half-pi)) (+ mod-angle pi))
	(else mod-angle))))

(define (percentage a) (/ (round (* 100 a)) 100))

;;; Settings

(define *settings* #f)

(define (*load-settings!*)
 (set!
  *settings*
  (read-object-from-file
   (format #f
	   "~a/imitate/tool/settings"
	   (getenv "HOME")))))

(define (*save-settings*)
 (write-object-to-file
  *settings*
  (format #f
	  "~a/imitate/tool/settings"
	  (getenv "HOME"))))

(define (settings-lookup r path)
 (call-with-current-continuation
  (lambda (c) (foldl
	       (lambda (t v) (unless (assoc v t) (c #f))
		       (cadr (assoc v t)))
	       path r))))

(define (settings-update r val path)
 (let loop ((t r) (path path))
  (if (null? path)
      val
      (if (find-if (lambda (x) (equal? (car x) (car path))) t)
	  (map
	   (lambda (t)
	    (if (equal? (car t) (car path))
		`(,(car t) ,(loop (cadr t) (cdr path)))
		t))
	   t)
	  (append
	   (foldl (lambda (a b) `((,b ,a))) (reverse path) val)
	   t)))))

(define (settings-remove r path)
 (let loop ((t r) (path path))
  (cond
   ((null? path) #f)
   ((= (length path) 1)
    (remove-if (lambda (x) (equal? (car x) (car path))) t))
   (else (map (lambda (t)
	       (if (equal? (car t) (car path))
		   `(,t ,(loop (cadr t) (cdr path)))
		   t))
	      t)))))

(define (*settings-lookup* . path)
 (settings-lookup *settings* path))

(define (*settings-update!* val . path)
 (set! *settings* (settings-update *settings* val path)))

(define (*settings-remove* . path)
 (set! *settings* (settings-remove *settings* path)))

;;; Statistics

(define (list-mean p)
 (if (vector? (car p))
     (k*v  (/ 1 (length p)) (reduce v+ p 0))
     (/ (reduce + p 0) (length p))))

(define (list-covariance l)
 (let ((mu (list-mean l)))
  (k*m (/ (length l))
       (reduce m+ (map (lambda (e) (self-outer-product * (v- e mu))) l) #f))))

(define (list-variance s)
 (let ((mu (list-mean s)))
  (/ (reduce + (map (lambda (s) (sqr (- s mu))) s) 0) (length s))))

(define (list-skewness l)
 (let ((mu (list-mean l))
       (sigma (list-variance l)))
  (/ (* (/ (length l)) (reduce + (map (lambda (e) (expt (-  e mu) 3)) l) 0))
     (expt sigma (/ 3 2)))))

(define (list-kurtosis l)
 (let ((mu (list-mean l))
       (sigma (list-variance l)))
  (- (/ (* (/ (length l)) (reduce + (map (lambda (e) (expt (-  e mu) 4)) l) 0))
	(sqr sigma))
     3)))

(define (list-correlation l1 l2)
 (let ((mu1 (list-mean l1)) (mu2 (list-mean l2))
       (s1 (sqrt (list-variance l1))) (s2 (sqrt (list-variance l2))))
  (/
   (reduce + (map (lambda (v1 v2) (* (- v1 mu1) (- v2 mu2))) l1 l2) 0)
   (* (- (length l1) 1) s1 s2))))

(define (vector-mean v)
 (/ (reduce-vector + v 0) (vector-length v)))

(define (vector-variance v)
 (let ((mu (vector-mean v)))
  (/ (map-reduce-vector + 0 (lambda (s) (sqr (- s mu))) v) (vector-length v))))

(define (vector-skewness v)
 (let ((mu (vector-mean v))
       (sigma (vector-variance v)))
  (/ (* (/ (vector-length v)) (map-reduce-vector + 0 (lambda (e) (expt (- e mu) 3)) v))
     (expt sigma (/ 3 2)))))

(define (vector-kurtosis v)
 (let ((mu (vector-mean v))
       (sigma (vector-variance v)))
  (- (/ (* (/ (vector-length v)) (map-reduce-vector + 0 (lambda (e) (expt (- e mu) 4)) v))
	(sqr sigma))
     3)))

(define (vector-correlation v1 v2)
 (let ((mu1 (vector-mean v1)) (mu2 (vector-mean v2))
       (s1 (sqrt (vector-variance v1))) (s2 (sqrt (vector-variance v2))))
  (/
   (map-reduce-vector + 0 (lambda (v1 v2) (* (- v1 mu1) (- v2 mu2))) v1 v2)
   (* (- (length v1) 1) s1 s2))))

(define (coefficient-of-bimodality v)
 (cond ((list? v)
	(/ (+ 1 (sqr (list-skewness v))) (+ (list-kurtosis v) 3)))
       ((vector? v)
	(/ (+ 1 (sqr (vector-skewness v))) (+ (vector-kurtosis v) 3)))
       (else (fuck-up))))

(define (vectors-mean values)
 (k*v (/ 1 (vector-length values)) (reduce-vector v+ values #f)))

(define (vectors-variance mu values)
 (k*m (/ 1 (vector-length values))
      (reduce-vector m+
		     (map-vector
		      (lambda (value)
		       (self-outer-product * (v- value mu)))
		      values)
		     #f)))

(define (mahalanobis-distance val mu isigma)
 (let ((dev (v- val mu)))
  (sqrt (abs (dot dev (m*v isigma dev))))))

(define (frequencies l)
 (let loop ((f '()) (l l))
  (if (null? l)
      f
      (loop (cons `(,(car l) ,(count (car l) l)) f)
	    (remove (car l) l)))))

;;; Log space

(define (my-floor a) (if (number? a) (floor a) (floor (primal* a))))
(define (my-ceiling a) (if (number? a) (ceiling a) (ceiling (primal* a))))
(define (my-round a) (if (number? a) (round a) (round (primal* a))))

(define (my-max a b) (if (> a b) a b))
(define (my-min a b) (if (< a b) a b))

(define (my-add-exp e1 e2)
 (let* ((e-max (my-max e1 e2))
	(e-min (my-min e1 e2))
	(factor (my-floor e-min)))
  (if (= e-max minus-infinity)
      minus-infinity
      (if (> (- e-max factor) log-math-precision)
	  e-max
	  (+ (log (+ (exp (- e-max factor)) (exp (- e-min factor))))
	     factor)))))

(define (sub-exp e1 e2)
 (let* ((e-max (my-max e1 e2))
	(e-min (my-min e1 e2))
	(factor (my-floor e-min)))
  (if (= e-max minus-infinity)
      minus-infinity
      (if (> (- e-max factor) log-math-precision)
	  e-max
	  (+ (log (- (exp (- e-max factor)) (exp (- e-min factor))))
	     factor)))))

(define (recip-exp e2)
 (let* ((factor (my-floor e2)))
  (if (> (- factor) log-math-precision)
      0
      (+ (log (- (exp (- factor)) (exp (- e2 factor))))
	 factor))))

;;; Screen recording

(define (window-id name)
 (let ((pathname
	(format #f "/tmp/xwininfo-~a-~a.text" (getenv "USERNAME") (getpid))))
  (rm pathname)
  (system
   (format #f "xwininfo -name ~a|fgrep id:|cut -d ' ' -f 4 >~a" name pathname))
  (let ((id (first (read-file pathname))))
   (rm pathname)
   id)))

(define (start-recording! fps)
 (system
  (format
   #f
   "recordmydesktop >/tmp/rec -windowid ~a --no-sound --on-the-fly-encoding -fps ~a&"
   (window-id *program*)
   fps)))

(define (start-recording-location! fps x y w h)
 (system
  (format
   #f
   "recordmydesktop >/tmp/rec -x ~a -y ~a -width ~a -height ~a --no-sound --on-the-fly-encoding -fps ~a&"
   x y w h fps)))

(define (stop-recording!) (system "killall >/dev/null recordmydesktop"))

;;; 3D Homogenous Transformations

(define (translation-3d x y z)
 `#(#(1 0 0 ,x)
    #(0 1 0 ,y)
    #(0 0 1 ,z)
    #(0 0 0  1)))

(define (scaling-3d x y z)
 `#(#(,x 0 0 0)
    #(0 ,y 0 0)
    #(0 0 ,z 0)
    #(0 0 0  1)))

(define (rotation-3d-x a)
 `#(#(1 0            0            0)
    #(0 ,(cos a)     ,(sin a)     0)
    #(0 ,(- 0 (sin a)) ,(cos a)     0)
    #(0 0            0            1)))

(define (rotation-3d-y a)
 `#(#(,(cos a)     0 ,(sin a) 0)
    #(0            1 0        0)
    #(,(- 0 (sin a)) 0 ,(cos a) 0)
    #(0            0 0        1)))

(define (rotation-3d-z a)
 `#(#(,(cos a)     ,(sin a) 0 0)
    #(,(- 0 (sin a)) ,(cos a) 0 0)
    #(0            0        1 0)
    #(0            0        0 1)))

(define identity-3d (translation-3d 0 0 0))

(define (make-transform-3d theta phi psi x y z)
 (m* (translation-3d x y z)
     (m* (m* (rotation-3d-x theta) (rotation-3d-y phi)) (rotation-3d-z psi))))

(define (point->homogenous p) (list->vector (append (vector->list p) `(,1))))
(define (homogenous->point p) (subvector p 0 3))

(define (transform-point-3d m p)
 (homogenous->point (m*v m (point->homogenous p))))

(define (transform-line-3d m l)
 (make-line-segment (transform-point-3d m (p l))
		    (transform-point-3d m (q l))))

(define (apply-transform-line t line)
 (make-line-segment (apply-transform t (p line))
		    (apply-transform t (q line))))

(define (transform->components transform)
 (let* ((phi (asin (matrix-ref (transform-rotation transform) 0 2)))
	(theta (acos (/ (matrix-ref (transform-rotation transform) 2 2)
		    (cos phi))))
	(psi (asin (/ (matrix-ref (transform-rotation transform) 0 1)
		    (cos phi)))))
  `(,(radians->degrees theta)
    ,(radians->degrees phi)
    ,(radians->degrees psi)
    ,(vector-ref (transform-translation transform) 0)
    ,(vector-ref (transform-translation transform) 1)
    ,(vector-ref (transform-translation transform) 2))))

(define (transform-3d->components transform)
 (let* ((phi (asin (matrix-ref  transform 0 2)))
	(theta (acos (/ (matrix-ref transform 2 2) (cos phi))))
	(psi (asin (/ (matrix-ref  transform 0 1) (cos phi)))))
  `(,(radians->degrees theta)
    ,(radians->degrees phi)
    ,(radians->degrees psi)
    ,(matrix-ref transform 0 3)
    ,(matrix-ref transform 1 3)
    ,(matrix-ref transform 2 3))))

(define (project-line l f)
 (make-line-segment (project (p l) f) (project (q l) f)))

(define (safe-project v focal-length)
 (if (= (z v) 0)
     `#(,infinity ,infinity)
     (k*v (/ focal-length (z v)) (vector (x v) (y v)))))

(define (safe-project-line l f)
 (make-line-segment (safe-project (p l) f)
		    (safe-project (q l) f)))

(define (rotate-point-2d p theta)
 (m*v (rotation-matrix-2d (degrees->radians theta)) p))

(define (rotate-line-2d l theta)
 (make-line-segment (rotate-point-2d (p l) theta)
		    (rotate-point-2d (q l) theta)))

(define (line->direction-vector l) (unit (map-vector - (p l) (q l))))

(define (translate-line-2d l v)
 (make-line-segment
  (map-vector (lambda (x) (+ v x)) (p l))
  (map-vector (lambda (x) (+ v x)) (q l))))

(define (filter-lines lines min max)
 (remove-if
  (lambda (l) (or (and (> (x (p l)) (x max)) (> (x (q l)) (x max)))
	     (and (< (x (p l)) (x min)) (< (x (q l)) (x min)))
	     (and (> (y (p l)) (y max)) (> (y (q l)) (y max)))
	     (and (< (y (p l)) (y min)) (< (y (q l)) (y min)))
	     (< (line-segment-length l) 10)))
  lines))

(define (line-distance l1 l2)
 (minp < (list (distance (p l1) (p l2))
	       (distance (p l1) (q l2))
	       (distance (q l1) (p l2))
	       (distance (q l1) (q l2)))))

;;; I/O

(define (with-persistent-temporary-file prefix f)
 (let* ((filename (unique-temporary-file prefix))
	(result (f filename)))
  (rm-if-necessary filename)
  result))

(define (system-output cmd)
 (with-temporary-file "/tmp/system.out"
		      (lambda (file)
		       (system (format #f "~a > ~s" cmd file))
		       (read-file file))))

(define (architecture-path) (car (system-output "architecture-path")))

(define (pwd) (getenv "PWD"))

;;; Lists & Vectors

(define (maximum l)
 (define (m l x)
  (if (null? l) x
      (if (> (car l) x) (m (cdr l) (car l)) (m (cdr l) x))))
 (when (not (null? l)) (m (cdr l) (car l))))
(define (minimum l)
 (define (m l x)
  (if (null? l) x
      (if (< (car l) x) (m (cdr l) (car l)) (m (cdr l) x))))
 (when (not (null? l)) (m (cdr l) (car l))))

(define (maximump l p)
 (define (m p l x)
  (if (null? l) x
      (if (> (p (car l)) (p x)) (m p (cdr l) (car l)) (m p (cdr l) x))))
 (when (not (null? l)) (m p (cdr l) (car l))))
(define (minimump l p)
 (define (m p l x)
  (if (null? l) x
      (if (< (p (car l)) (p x)) (m p (cdr l) (car l)) (m p (cdr l) x))))
 (when (not (null? l)) (m p (cdr l) (car l))))

(define (maximum-with-position l)
 (let loop ((i 0) (r -1) (m #f) (l l))
  (if (null? l)
      (list m r)
      (if (> (first l) (if m m minus-infinity))
	  (loop (+ i 1) i (first l) (rest l))
	  (loop (+ i 1) r m (rest l))))))

(define (minimum-with-position l)
 (let loop ((i 0) (r -1) (m #f) (l l))
  (if (null? l)
      (list m r)
      (if (< (first l) (if m m infinity))
	  (loop (+ i 1) i (first l) (rest l))
	  (loop (+ i 1) r m (rest l))))))

(define (safe-sublist l s f)
 (if (<= (length l) (- f s)) l (sublist l s f)))

(define (append-vector vec1 vec2)
 (let ((l1 (vector-length vec1))
       (l2 (vector-length vec2)))
  (map-n-vector
   (lambda (i)
    (if (< i l1) (vector-ref vec1 i) (vector-ref vec2 (- i l1))))
   (+ l1 l2))))

(define (vector-position vector val)
  (let loop ((i 0))
    (if (< i (vector-length vector))
	(if (equal? val (vector-ref vector i))
	    i
	    (loop (+ i 1)))
	#f)))

(define (rest-vector v)
 (subvector v 1 (vector-length v)))

(define (remove-if-vector p v)
 (let loop ((v v) (c '#()))
  (cond ((zero? (vector-length v)) c)
	((p (x v)) (loop (rest-vector v) c))
	(else (loop (rest-vector v) (append-vector c (subvector v 0 1)))))))

(define (remove-if-not-vector p v)
 (let loop ((v v) (c '#()))
  (cond ((zero? (vector-length v)) c)
	((p (x v)) (loop (rest-vector v) (append-vector c (subvector v 0 1))))
	(else (loop (rest-vector v) c)))))

(define (o a b . c)
 (let ((fs (cons a (cons b c))))
  (lambda d (foldl (lambda (x f) (f x)) (but-last fs) (apply (last fs) d)))))

(define (for-each-indexed-vector f v)
 (for-each-n (lambda (i) (f (vector-ref v i) i)) (vector-length v)))

(define (crop m x y w h)
 (map-vector
  (lambda (row)
   (subvector row x (+ x w)))
  (subvector m y (+ y h))))

(define (map-linear f s e n)
 (map-n (lambda (v) (f (+ s (* v (/ (- e s) n))))) (+ 1 n)))

(define (map-medial f l key)
 (if (null? l)
     l
     (map (lambda (a b) (f (/ (+ (key a) (key b)) 2)))
	  (cdr (sort l < key))
	  (sort l < key))))

(define (matrix-ref-nd m . is)
 (if (= (length is) 1)
     (vector-ref m (first is))
     (apply matrix-ref-nd `(,(vector-ref m (first is)) ,@(rest is)))))
(define (matrix-3d-ref a s i j) (matrix-ref-nd a s i j))

(define (matrix-set-nd! m v . is)
 (if (= (length is) 1)
     (begin (write m) (newline) (vector-set! m (first is) v))
     (apply matrix-set-nd! `(,(vector-ref m (first is)) ,v ,@(rest is)))))
(define (matrix-3d-set! a v s i j) (matrix-set-nd! a v s i j))

(define (map-matrix-nd f m n)
 (if (= n 1)
     (map-vector f m)
     (map-vector (lambda (m) (map-matrix-nd f m (- n 1))) m)))
(define (for-each-matrix-nd f m n)
 (if (= n 1)
     (for-each-vector f m)
     (for-each-vector (lambda (m) (for-each-matrix-nd f m (- n 1))) m)))
(define (map-matrix f m) (map-matrix-nd f m 2))
(define (for-each-matrix f m) (for-each-matrix-nd f m 2))
(define (map-matrix-3d f m) (map-matrix-nd f m 3))
(define (for-each-matrix-3d f m) (for-each-matrix-nd f m 3))

(define (map-n-matrix f i j)
 (map-n-vector (lambda (i) (map-n-vector (lambda (j) (f i j)) j)) i))
(define (for-each-n-matrix f i j)
 (for-each-n (lambda (i) (for-each-n (lambda (j) (f i j)) j)) i))

(define (map-indexed-matrix f m)
 (map-indexed-vector (lambda (r i) (map-indexed-vector (lambda (c j) (f c i j)) r)) m))
(define (for-each-indexed-matrix f m)
 (for-each-indexed-vector
  (lambda (r i) (for-each-indexed-vector (lambda (c j) (f c i j)) r))
  m))
(define (map-indexed-matrix-3d f p)
 (map-indexed-vector
  (lambda (s l) (map-indexed-matrix (lambda (c i j) (f c l i j)) s))
  p))
(define (for-each-indexed-matrix-3d f p)
 (for-each-indexed-vector
  (lambda (s l) (for-each-indexed-matrix (lambda (c i j) (f c l i j)) s))
  p))

(define (reduce-matrix-nd g m i n)
 (if (= n 1)
     (reduce-vector g m i)
     (reduce-vector
      g (map-vector (lambda (m) (reduce-matrix-nd g m i (- n 1))) m) i)))
(define (map-reduce-matrix-nd g i f m n)
 (if (= n 1)
     (map-reduce-vector g i f m)
     (map-reduce-vector
      g i f (map-vector (lambda (m) (map-reduce-matrix-nd g i f m (- n 1))) m))))
(define (reduce-matrix g m i) (reduce-matrix-nd g m i 2))
(define (map-reduce-matrix g i f m) (map-reduce-matrix-nd g i f m 2))
(define (reduce-matrix-3d g m i) (reduce-matrix-nd g m i 3))
(define (map-reduce-matrix-3d g i f m) (map-reduce-matrix-nd g i f m 3))

(define (map-m-n f m n)
 (let loop ((i m) (c '()))
  (if (> i n) (reverse c) (loop (+ i 1) (cons (f i) c)))))
(define (for-each-m-n f m n) (do ((i m (+ i 1))) ((> i n) #f) (f i)))

(define (concat l) (reduce append l '()))

(define (matrix->flat-list p) (vector->list (unshape-matrix p)))

(define (map-n-3 f xb yb zb)
 (map-n
  (lambda (x-c)
   (map-n
    (lambda (y-c) (map-n (lambda (z-c) (f x-c y-c z-c)) zb))
    yb))
  xb))

(define (map-concat-n-3 f xb yb zb)
 (map-concat-n
  (lambda (x-c)
   (map-concat-n
    (lambda (y-c)
     (map-n (lambda (z-c) (f x-c y-c z-c)) zb)) yb))
  xb))

(define (map-vector-2d f m) (map-matrix f m))
(define (map-vector-2d-2-args f m1 m2)
 (map-vector (lambda (v1 v2) (map-vector (lambda (x1 x2) (f x1 x2)) v1 v2)) m1 m2))
(define (map-vector-2d-2 f a b)
 (map-vector (lambda (a b) (map-vector (lambda (x y) (f x y)) a b)) a b))
(define (some-vector-2d p v)
 (some-vector (lambda (sub-v) (some-vector (lambda (x) (p x)) sub-v)) v))
(define (every-vector-2d p v) (not (some-vector-2d (lambda (x) (not (p x))) v)))
(define (reduce-vector-2d f m i) (reduce-matrix f m i))

(define (shape-matrix v c)
 (let* ((r (/ (vector-length v) c))
	(m (make-vector r)))
  (for-each-n
   (lambda (i) (vector-set! m i (subvector v (* i c) (* (+ i 1) c))))
   r)
  m))

(define (unshape-matrix m) 
 (if (and (not (equal? m '#())) (matrix? m))
     (unshape-matrix (reduce-vector append-vector m '#()))
     m))

(define (matrix->list-of-lists m) (vector->list (map-vector vector->list m)))

(define (list-of-lists->matrix lol) (list->vector (map list->vector lol)))

(define (transpose-list-of-lists lol)
 (matrix->list-of-lists (transpose (list-of-lists->matrix lol))))

(define (zip a b) (if (null? a) '() (cons `(,(car a) ,(car b)) (zip (cdr a) (cdr b)))))

(define (take n l) (sublist l 0 n))
(define (drop n l) (sublist l n (length l)))
(define (drop-until p ls)
 (let loop ((ls ls)) (if (or (null? ls) (p (car ls))) ls (loop (cdr ls)))))

(define (fold f l i) (foldr f l i))
(define (foldl f l i)
 (let loop ((l l) (c i)) (if (null? l) c (loop (rest l) (f c (first l))))))
(define (foldr f l i)
 (let loop ((l l)) (if (null? l) i (f (car l) (loop (rest l))))))

(define (join l) (reduce append l '()))
(define (map-concat f l) (join (map f l)))
(define (map-concat-n f n) (join (map-n f n)))

(define (ring-forward l)
 (if (> (length l) 1) (cons (last l) (reverse (cdr (reverse l)))) l))
(define (ring-backward l) (append (cdr l) `(,(car l))))
(define (ring-forward-to l o)
 (if (equal? o (car l)) l (ring-forward-to (ring-forward l) o)))
(define (ring-forward-between r a b)
 (take (+ (position b (ring-forward-to r a)) 1) (ring-forward-to r a)))

(define (map-indexed-vector f v . &rest)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((u (make-vector (vector-length v))))
  (for-each-n
   (lambda (i)
    (vector-set!
     u i
     (apply f (vector-ref v i) i (map (lambda (v) (vector-ref v i)) &rest))))
   (vector-length v))
  u))

(define (line-segment-center l) (k*v (/ 2) (v+ (p l) (q l))))

(define (inexact-matrix-ref m y x)
 (matrix-ref m (inexact->exact (round y)) (inexact->exact (round x))))

(define (matrix? v)
 (and (vector? v) (or (= (vector-length v) 0) (vector? (vector-ref v 0)))))

(define (submatrix m x-offset y-offset x-size y-size)
 (define (safe-matrix-ref m r c)
  (if (and (< r (matrix-rows m)) (< c (matrix-columns m)))
      (matrix-ref m r c)
      #f))
 (map-n-vector
  (lambda (x)
   (map-n-vector
    (lambda (y) (safe-matrix-ref m (+ x x-offset) (+ y y-offset)))
    y-size))
  x-size))

(define (string-join delim l)
 (if (null? l) "" (foldl (lambda (a b) (string-append a delim b)) (cdr l) (car l))))

(define (string*-join delim l)
 (if (null? l) "" (foldl (lambda (a b) (string*-append a delim b)) (cdr l) (car l))))

(define (string-find string char)
 (let loop ((i 0))
  (if (= i (string-length string))
      #f
      (if (equal? char (string-ref string i)) i (loop (+ i 1))))))

(define (string-find-from-index string char start)
 (let loop ((i start))
  (if (= i (string-length string))
      #f
      (if (equal? char (string-ref string i)) i (loop (+ i 1))))))

(define (unlines l) (string-join (list->string '(#\newline)) l))
(define (lines string) 
 (let loop ((index 0) (strings '()))
  (let ((new-index (string-find-from-index string #\newline index)))
   (if new-index
       (loop (+ new-index 1)
	     (cons (substring string index new-index) strings))
       (reverse 
	(if (= index (- (string-length string) 1))
	    strings
	    (cons (substring string index (string-length string))
		  strings)))))))

;;; GUI

(define-structure image-center x y)

(define (transform-line-to-image-center l image-center)
 (make-line-segment
  `#(,(--two (image-center-x image-center) (x (p l)))
	 ,(--two (image-center-y image-center) (y (p l))))
  `#(,(--two (image-center-x image-center) (x (q l)))
	 ,(--two (image-center-y image-center) (y (q l))))))

(define *thick-red-gc* #f)
(define *thick-blue-gc* #f)
(define *thick-orange-gc* #f)
(define *thick-green-gc* #f)
(define *medium-red-gc* #f)
(define *medium-blue-gc* #f)
(define *medium-orange-gc* #f)
(define *medium-green-gc* #f)
(define *large-lucida-font* #f)
(define *large-lucida-gc* #f)
(define *large-lucida-baseline* #f)

(define (setup-*thick-red-gc*)
 (set! *thick-red-gc* (xcreategc *display* *window* 0 (make-xgcvalues)))
 (xsetbackground *display* *thick-red-gc*
		 (xcolor-pixel (second *background*)))
 (xsetforeground *display* *thick-red-gc* (xcolor-pixel (second *red*)))
 (xsetfont *display* *thick-red-gc* (xfontstruct-fid *bold-font*))
 (xsetlineattributes *display* *thick-red-gc*
		     5 linesolid capround joinround))

(define (setup-*thick-blue-gc*)
 (set! *thick-blue-gc* (xcreategc *display* *window* 0 (make-xgcvalues)))
 (xsetbackground *display* *thick-blue-gc*
		 (xcolor-pixel (second *background*)))
 (xsetforeground *display* *thick-blue-gc* (xcolor-pixel (second *blue*)))
 (xsetfont *display* *thick-blue-gc* (xfontstruct-fid *bold-font*))
 (xsetlineattributes *display* *thick-blue-gc*
		     5 linesolid capround joinround))

(define (setup-*thick-orange-gc*)
 (set! *thick-orange-gc* (xcreategc *display* *window* 0 (make-xgcvalues)))
 (xsetbackground *display* *thick-orange-gc*
		 (xcolor-pixel (second *background*)))
 (xsetforeground *display* *thick-orange-gc* (xcolor-pixel (second *orange*)))
 (xsetfont *display* *thick-orange-gc* (xfontstruct-fid *bold-font*))
 (xsetlineattributes *display* *thick-orange-gc*
		     5 linesolid capround joinround))

(define (setup-*thick-green-gc*)
 (set! *thick-green-gc* (xcreategc *display* *window* 0 (make-xgcvalues)))
 (xsetbackground *display* *thick-green-gc*
		 (xcolor-pixel (second *background*)))
 (xsetforeground *display* *thick-green-gc* (xcolor-pixel (second *green*)))
 (xsetfont *display* *thick-green-gc* (xfontstruct-fid *bold-font*))
 (xsetlineattributes *display* *thick-green-gc*
		     5 linesolid capround joinround))

(define (setup-*medium-red-gc*)
 (set! *medium-red-gc* (xcreategc *display* *window* 0 (make-xgcvalues)))
 (xsetbackground *display* *medium-red-gc*
		 (xcolor-pixel (second *background*)))
 (xsetforeground *display* *medium-red-gc* (xcolor-pixel (second *red*)))
 (xsetfont *display* *medium-red-gc* (xfontstruct-fid *bold-font*))
 (xsetlineattributes *display* *medium-red-gc*
		     2 linesolid capround joinround))

(define (setup-*medium-blue-gc*)
 (set! *medium-blue-gc* (xcreategc *display* *window* 0 (make-xgcvalues)))
 (xsetbackground *display* *medium-blue-gc*
		 (xcolor-pixel (second *background*)))
 (xsetforeground *display* *medium-blue-gc* (xcolor-pixel (second *blue*)))
 (xsetfont *display* *medium-blue-gc* (xfontstruct-fid *bold-font*))
 (xsetlineattributes *display* *medium-blue-gc*
		     2 linesolid capround joinround))

(define (setup-*medium-orange-gc*)
 (set! *medium-orange-gc* (xcreategc *display* *window* 0 (make-xgcvalues)))
 (xsetbackground *display* *medium-orange-gc*
		 (xcolor-pixel (second *background*)))
 (xsetforeground *display* *medium-orange-gc* (xcolor-pixel (second *orange*)))
 (xsetfont *display* *medium-orange-gc* (xfontstruct-fid *bold-font*))
 (xsetlineattributes *display* *medium-orange-gc*
		     2 linesolid capround joinround))

(define (setup-*medium-green-gc*)
 (set! *medium-green-gc* (xcreategc *display* *window* 0 (make-xgcvalues)))
 (xsetbackground *display* *medium-green-gc*
		 (xcolor-pixel (second *background*)))
 (xsetforeground *display* *medium-green-gc* (xcolor-pixel (second *green*)))
 (xsetfont *display* *medium-green-gc* (xfontstruct-fid *bold-font*))
 (xsetlineattributes *display* *medium-green-gc*
		     2 linesolid capround joinround))

(define (setup-*large-lucida-gc*)
 (set! *large-lucida-font*
       (xloadqueryfont *display* "-*-lucidatypewriter-*-*-*-*-26-*-*-*-*-*-*-*"))
 (set! *large-lucida-baseline* (xfontstruct-descent *large-lucida-font*))
 (set! *large-lucida-gc*
       (xcreategc *display* *window* 0 (make-xgcvalues)))
 (xsetbackground *display* *large-lucida-gc*
		 (xcolor-pixel (second *background*)))
 (xsetforeground *display* *large-lucida-gc*
		 (xcolor-pixel (second *foreground*)))
 (xsetfont *display* *large-lucida-gc* (xfontstruct-fid *large-lucida-font*)))

(define (setup-extra-x-gcs)
 (setup-*thick-red-gc*)
 (setup-*thick-blue-gc*)
 (setup-*thick-orange-gc*)
 (setup-*thick-green-gc*)
 (setup-*medium-red-gc*)
 (setup-*medium-blue-gc*)
 (setup-*medium-orange-gc*)
 (setup-*medium-green-gc*)
 (setup-*large-lucida-gc*))

(define (free-extra-x-gcs)
 (xfreegc *display* *medium-red-gc*)
 (xfreegc *display* *medium-blue-gc*)
 (xfreegc *display* *medium-orange-gc*)
 (xfreegc *display* *medium-green-gc*)
 (xfreegc *display* *thick-red-gc*)
 (xfreegc *display* *thick-blue-gc*)
 (xfreegc *display* *thick-orange-gc*)
 (xfreegc *display* *thick-green-gc*)
 (xfreegc *display* *large-lucida-gc*))

(define (xremove-expose-events)
 (let loop ()
  (when (> (xpending *display*) 0)
   (let ((event (xpeekevent *display*)))
    (when (= (xevent-xany-type event) expose)
     (xnextevent *display*) (loop))))))

(define (standard-buttons button-columns help)
 (define-button 0 0 "Help" #f
  (lambda () (message "") (help) (help-command)))
 (define-button (- button-columns 1) 0 "Quit" #f quit)
 (define-key (list (control #\x) (control #\c)) "Quit" quit)
 (define-key (control #\h) "Help" (lambda () (help) (help-command)))
 (define-key (control #\g) "Abort" abort-command))

(define (draw-clickable-strings-with-scroll-bar
	 first-line set-first-line!
	 left middle right strings
	 xmin xmax ymin ymax)
 ;; belongs in QobiScheme
 (let* ((visible-lines (quotient (- ymax ymin) *roman-height*))
	(first-line (first-line))
	(last-line (min (+ first-line visible-lines) (length strings))))
  (unless (null? strings)
   (let* ((y1 ymin)
	  (y2 ymax)
	  (y3 (+ y1
		 (inexact->exact
		  (floor (* (- y2 y1) (/ first-line (length strings)))))))
	  (y4 (+ y1
		 (inexact->exact
		  (floor (* (- y2 y1) (/ last-line (length strings))))))))
    (xfillrectangle
     *display* *display-pane* *thin-gc* (+ xmax 4) y1 1 (- y2 y1))
    (xfillrectangle
     *display* *display-pane* *thin-gc* (+ xmax 2) y3 5 (- y4 y3))
    (define-region
     (+ xmax 2)
     y1
     5
     (- y2 y1)
     (lambda (x y)
      (set-first-line!
       (min (max 0 (- (length strings) visible-lines))
	    (quotient (* (length strings) (- y y1)) (- y2 y1))))
      (redraw-display-pane)))
    (define-region
     (+ xmax 2)
     y3
     5
     (- y4 y3)
     (lambda (x y5)
      (tracking-pointer
       #f
       #f
       (lambda (x y6)
	(set-first-line!
	 (min (max 0 (- (length strings) visible-lines))
	      (max 0
		   (+ first-line
		      (quotient (* (length strings) (- y6 y5)) (- y2 y1))))))
	(redraw-display-pane)))))))
  (for-each-indexed (lambda (string i)
		     (define-button-specific-region
		      button1
		      0
		      0
		      xmin
		      (+ (* i *roman-height*) ymin)
		      (xtextwidth *roman-font* string (string-length string))
		      *roman-height*
		      (lambda (x y) (left (+ i first-line))))
		     (define-button-specific-region
		      button2
		      0
		      0
		      xmin
		      (+ (* i *roman-height*) ymin)
		      (xtextwidth *roman-font* string (string-length string))
		      *roman-height*
		      (lambda (x y) (middle (+ i first-line))))
		     (define-button-specific-region
		      button3
		      0
		      0
		      xmin
		      (+ (* i *roman-height*) ymin)
		      (xtextwidth *roman-font* string (string-length string))
		      *roman-height*
		      (lambda (x y) (right (+ i first-line))))
		     (xdrawstring *display* *display-pane* *roman-gc*
				  xmin (+ (* (+ i 1) *roman-height*) ymin)
				  string (string-length string)))
		    (sublist strings first-line last-line))))

(define (ellipse->lines ellipse)
 (let* ((previous-x #f)
	(previous-y #f)
	(x0 (ellipse-x0 ellipse))
	(y0 (ellipse-y0 ellipse))
	(t0 (ellipse-t0 ellipse))
	(a (ellipse-a ellipse))
	(b (ellipse-b ellipse))
	(rxx (cos t0))
	(rxy (- (sin t0)))
	(ryx (- rxy))
	(ryy rxx)
	(lines '()))
  (for-each-n
   (lambda (i)
    (let* ((ellipse-x (* a (sin (degrees->radians (* 10 i)))))
	   (ellipse-y (* b (cos (degrees->radians (* 10 i)))))
	   (this-x (+ (* rxx ellipse-x) (* rxy ellipse-y) x0))
	   (this-y (+ (* ryx ellipse-x) (* ryy ellipse-y) y0)))
     (when previous-x
      (set! lines
	    (cons (make-line-segment
		   `#(,this-x ,this-y) `#(,previous-x ,previous-y)) lines)))
     (set! previous-x this-x)
     (set! previous-y this-y)))
   37)
  lines))

(define (acot x) (- half-pi (atan x)))

(define (left-pseudo-inverse m)
 (let ((inverse (invert-matrix (m* (transpose m) m))))
  (if inverse (m* inverse (transpose m)) #f)))

(define (right-pseudo-inverse m)
 (let ((inverse (invert-matrix (m* m (transpose m)))))
  (if inverse (m* (transpose m) inverse) #f)))

(define (ellipse-fit ps)
 (scheme->matlab! "x" (map (lambda (a) (vector (x a))) ps))
 (scheme->matlab! "y" (map (lambda (a) (vector (y a))) ps))
 (matlab-eval-string "addpath('~/imitate/matlab')")
 (matlab-eval-string "[a b x0 y0 t0] = fit_ellipse3(x,y);")
 (make-ellipse
  (matlab-get-double "x0")
  (matlab-get-double "y0")
  (matlab-get-double "t0")
  (matlab-get-double "a")
  (matlab-get-double "b")))

(define (ellipse-fit-scheme pts)
 ;; http://mathworld.wolfram.com/Ellipse.html (reworked for angle)
 (let* ((observation-matrix
		 (list->vector
		  (map
		   (lambda (p)
			`#(,(* 2 (x p) (y p)) ,(sqr (y p)) ,(* 2 (x p)) ,(* 2 (y p)) 1))
		   pts)))
		(b-matrix (list->vector (map (lambda (p) (vector (- (sqr (x p))))) pts)))
		(pseudo-obs (left-pseudo-inverse observation-matrix))
		(parameters (unshape-matrix (m* pseudo-obs b-matrix)))
		(a 1)
		(b (x parameters))
		(c (y parameters))
		(d (z parameters))
		(f (vector-ref parameters 3))
		(g (vector-ref parameters 4))
		(delta (- (* b b) (* a c)))
		(intermediate
		 (* 2 (+ (* a f f)
				 (* c d d)
				 (* g b b)
				 (- (* 2 b d f))
				 (- (* a c g)))))
		(s (sqrt (+ 1 (/ (* 4 b b) (sqr (- a c))))))
		(a-prime (sqrt (/ intermediate (* delta (- (* (- a c) s) (+ c a))))))
		(b-prime (sqrt (/ intermediate (* delta (- (* (- c a) s) (+ c a)))))))
  (format #t "~a ~a ~a ~a ~a ~a ~a~%"
		  a b c d
		  (list (and (= b 0) (< a c))
				(and (= b 0) (> a c))
				(and (not (= b 0)) (< a c))
				(and (not (= b 0)) (> a c)))
		  (+ (if (> b 0) half-pi 0) (* (/ 2) (acot (/ (- a c) (* 2 b)))))
		  (cond ((and (= b 0) (< a c)) 0)
				((and (= b 0) (> a c)) half-pi)
				((and (not (= b 0)) (< a c)) (/ (acot (/ (- a c) (* 2 b))) 2))
				((and (not (= b 0)) (> a c)) (+ half-pi (/ (acot (/ (- a c) (* 2 b))) 2)))
				(else (fuck-up))))
  (make-ellipse
   (/ (- (* c d) (* b f)) delta)
   (/ (- (* a f) (* b d)) delta)
   (cond ((and (= b 0) (< a c)) 0)
		 ((and (= b 0) (> a c)) half-pi)
		 ((and (not (= b 0)) (< a c)) (/ (acot (/ (- a c) (* 2 b))) 2))
		 ((and (not (= b 0)) (> a c)) (+ half-pi (/ (acot (/ (- a c) (* 2 b))) 2)))
		 (else (fuck-up)))
   a-prime
   b-prime)))

(define (every-n-2d p v w)
 (every-n (lambda (a) (every-n (lambda (b) (p a b)) w)) v))
(define (every-n-3d p v w x)
 (every-n (lambda (a) (every-n-2d (lambda (b c) (p a b c)) w x)) v))
(define (every-n-4d p v w x y)
 (every-n-2d (lambda (a b) (every-n-2d (lambda (c d) (p a b c d)) x y)) v w))
(define (every-n-5d p v w x y z)
 (every-n (lambda (a) (every-n-4d (lambda (b c d e) (p a b c d e)) w x y z)) v))
(define (map-n-vector-2d f m n)
 (map-n-vector (lambda (a) (map-n-vector (lambda (b) (f a b)) n)) m))
(define (map-n-vector-3d f m n p)
 (map-n-vector (lambda (a) (map-n-vector-2d (lambda (b c) (f a b c)) n p)) m))
(define (map-n-vector-4d f m n p q)
 (map-n-vector-2d (lambda (a b) (map-n-vector-2d (lambda (c d) (f a b c d)) p q)) m n))
(define (map-n-vector-5d f m n p q r)
 (map-n-vector-3d (lambda (a b c) (map-n-vector-2d (lambda (d e) (f a b c d e)) q r)) m n p))
(define (product-2d f m n) (product (lambda (a) (product (lambda (b) (f a b)) n)) m))
(define (ref-1d m a) (vector-ref m a))
(define (ref-2d m a b) (matrix-ref m a b))
(define (ref-3d m a b c) (matrix-ref (vector-ref m a) b c))
(define (ref-4d m a b c d) (matrix-ref (matrix-ref m a b) c d))
(define (ref-5d m a b c d e) (matrix-ref (ref-3d m a b c) d e))
(define (sum-2d f m n) (sum (lambda (a) (sum (lambda (b) (f a b)) n)) m))
(define (sum-3d f m n p) (sum-2d (lambda (a b) (sum (lambda (c) (f a b c)) p)) m n))
(define (sum-4d f m n p q) (sum-2d (lambda (a b) (sum-2d (lambda (c d) (f a b c d)) p q)) m n))
(define (sum-pairs f m) (sum (lambda (a) (sum (lambda (b) (f a b)) a)) m))
(define (vector-sum f n i)
 (let loop ((n (- n 1)) (c i))
  (if (negative? n) c (loop (- n 1) (v+ c (f n))))))
(define (vector-sum-2d f m n i)
 (vector-sum (lambda (a) (vector-sum (lambda (b) (f a b)) n i)) m i))
(define (matrix-sum f n i)
 (let loop ((n (- n 1)) (c i))
  (if (negative? n) c (loop (- n 1) (m+ c (f n))))))
(define (matrix-sum-2d f m n i)
 (matrix-sum (lambda (a) (matrix-sum (lambda (b) (f a b)) n i)) m i))
(define (v/k v k) (k*v (/ 1 k) v))
(define (m/k m k) (k*m (/ 1 k) m))
(define (v*m*v v m) (dot v (m*v m v)))

(define (float-modulo x m) (- x (* m (inexact->exact (/ x m)))))
(define (exact-identity-matrix n) (vector->diagonal-matrix (make-vector n 1.0)))

(define (translate-3d x y z)
  `#(#(1 0 0 ,x) #(0 1 0 ,y) #(0 0 1 ,z) #(0 0 0 1)))

;;; Drawing

(define (draw-circle p radius gc)
 (xdrawarc *display*
	   *display-pane*
	   gc
	   (- (x p) radius)
	   (- (y p) radius)
	   (+ (* 2 radius) 1)
	   (+ (* 2 radius) 1)
	   (* 0 360)
	   (* 64 360)))

(define (degenerate-triangle? triangle)
 (or (v= (first triangle) (second triangle))
     (v= (second triangle) (third triangle))
     (v= (third triangle) (first triangle))))

(define (draw-filled-polygon pbm points)
 ;; points is a counterclockwise list of covex-polygon vertices
 (let ((u `#(,(exact-round (x (first (sort points < x))))
	     ,(exact-round (y (first (sort points < y))))))
       (v `#(,(exact-round (x (first (sort points > x))))
	     ,(exact-round (y (first (sort points > y))))))
       (triangles (remove-if degenerate-triangle? (triangulate points))))
  (do ((y1 (y u) (+ y1 1))) ((>= y1 (y v)))
   (do ((x1 (x u) (+ x1 1))) ((>= x1 (x v)))
    (let ((point (vector x1 y1)))
     (when (some (lambda (triangle)
		  (point-inside-or-on-triangle?
		   point (first triangle) (second triangle) (third triangle)))
		 triangles)
      (matrix-set! (pbm-bitmap pbm) y1 x1 #t)))))
  pbm))

(define (ellipse-circumference ellipse)
 ;; better Ramanujan's approximation: avoids infinite series
 (let* ((sum (+ (ellipse-a ellipse) (ellipse-b ellipse)))
	(diff (- (ellipse-a ellipse) (ellipse-b ellipse)))
	(inter1 (* 3.0 (sqr (/ diff sum)))))
  (* pi sum (+ 1 (/ inter1 (+ 10 (sqrt (- 4 inter1))))))))

(define (point-inside-or-on-ellipse? point ellipse)
 (let* ((xc (ellipse-x0 ellipse)) (yc (ellipse-y0 ellipse))
	(a (ellipse-a ellipse)) (b (ellipse-b ellipse))
	(theta (ellipse-t0 ellipse))
	(p0 (m*v (rotation-matrix-2d (- theta)) (v- point `#(,xc ,yc)))))
  (<= (+ (sqr (/ (x p0) a)) (sqr (/ (y p0) b))) 1.0)))

(define (ellipse-bounding-box ellipse)
 (let* ((xc (ellipse-x0 ellipse)) (yc (ellipse-y0 ellipse))
	(a (ellipse-a ellipse)) (b (ellipse-b ellipse))
	(theta (ellipse-t0 ellipse)))
  (map (lambda (p) (v+ `#(,xc ,yc) (m*v (rotation-matrix-2d theta) p)))
       `(#(,a ,b) #(,(- a) ,b) #(,(- a) ,(- b)) #(,a ,(- b))))))

(define (draw-filled-ellipse pbm ellipse)
 (let* ((ebpoints (ellipse-bounding-box ellipse))
	(u `#(,(exact-round (x (first (sort ebpoints < x))))
	      ,(exact-round (y (first (sort ebpoints < y))))))
	(v `#(,(exact-round (x (first (sort ebpoints > x))))
	      ,(exact-round (y (first (sort ebpoints > y)))))))
  (do ((y1 (y u) (+ y1 1))) ((>= y1 (y v)))
   (do ((x1 (x u) (+ x1 1))) ((>= x1 (x v)))
    (let ((point (vector x1 y1)))
     (when (point-inside-or-on-ellipse? point ellipse)
      (matrix-set! (pbm-bitmap pbm) y1 x1 #t)))))
  pbm))

;;; ROC

(define (precision relevant retreived)
 (if (= (length retreived) 0)
     0
     (/ (length (intersection relevant retreived))
	(length retreived))))

(define (recall relevant retreived)
 (if (= (length relevant) 0)
     0
     (/ (length (intersection relevant retreived))
	(length relevant))))

(define (accuracy all thresh)
 (define (tp)
  (remove-if-not (lambda (a) (and (>= (second a) thresh) (third a))) all))
 (define (tn)
  (remove-if-not (lambda (a) (and (< (second a) thresh) (not (third a)))) all))
 (define (fp)
  (remove-if-not (lambda (a) (and (>= (second a) thresh) (not (third a)))) all))
 (define (fn)
  (remove-if-not (lambda (a) (and (< (second a) thresh) (third a))) all))
 (/ (+ (length (tp)) (length (tn)))
    (length all)))

(define (prf found thresh)
 (let ((a
	(map
	 second
	 (map (lambda (a)
	       (maximump
		(map (lambda (a) (list (first a)
				  (map percentage (pr (second a) thresh))
				  (percentage (apply * (pr (second a) thresh))))) a)
		last))
	      found))))
  (list (/ (reduce + (map first a) 0) (length a))
  	(/ (reduce + (map second a) 0) (length a)))))

(define (accuracy-n all a b n)
 (map-n (lambda (v)
	 `(,(+ a (* v (/ (- b a) n)))
	   ,(accuracy all (+ a (* v (/ (- b a) n)))))) (+ 1 n)))

(define (accuracy-a all n) (let ((a (map second all))) (accuracy-n all (minimum a) (maximum a) n)))

(define (prf-n all a b n)
 (map-n (lambda (v)
	 `(,(+ a (* v (/ (- b a) n)))
	   ,(prf all (+ a (* v (/ (- b a) n)))))) (+ 1 n)))

(define (precision-recall-threshold-p thresh all p g)
 (define found (remove-if (lambda (a) (< (p a) thresh)) all))
 (define ground (remove-if-not g all))
 (list (precision ground found) (recall ground found)))

(define (pr all thresh)
 (precision-recall-threshold-p thresh all second third))

(define (graph-roc a s)
 (format #t "P~a = [" s)
 (for-each (lambda (a) (format #t "~a ~%" (first (second a)))) a)
 (format #t "]~%")
 (format #t "R~a = [" s)
 (for-each (lambda (a) (format #t "~a ~%" (second (second a)))) a)
 (format #t "]~%")
 (format #t "scatter(P~a,R~a)~%" s))

(define (graph-color-roc a s)
 (format #t "P~a = [" s)
 (for-each (lambda (a) (format #t "~a ~%" (first (second a)))) a)
 (format #t "]~%")
 (format #t "R~a = [" s)
 (for-each (lambda (a) (format #t "~a ~%" (second (second a)))) a)
 (format #t "]~%")
 (format #t "C~a = [" s)
 (for-each (lambda (a) (format #t "~a ~%" (first a))) a)
 (format #t "]~%")
 (format #t "scatter(P~a,R~a,C~a)~%" s s s))

(define (with-decompressed pathname f)
 (with-temporary-file
  (strip-directory pathname)
  (lambda (temp)
   (f (cond ((file-exists? (string-append pathname ".gz"))
			 (system (format #f "gunzip -c ~a > ~a" (string-append pathname ".gz") temp))
			 temp)
			((file-exists? (string-append pathname ".bz2"))
			 (system (format #f "bunzip2 -c ~a > ~a" (string-append pathname ".bz2") temp))
			 temp)
			(else pathname))))))

(define (read-object-from-cfile pathname)
 (with-decompressed pathname read-object-from-file))
