(MODULE TOOLLIB-C-BINDINGS)

(include "QobiScheme.sch")
(include "toollib-c-macros.sch")
(include "toollib-c-bindings.sch")

(c-include "stdlib.h")
(c-include "stdio.h")
(c-include "sys/types.h")
(c-include "unistd.h")
(c-include "gmp.h")

(define (curry2 f)
  (lambda (x) (lambda (y) (f x y))))

(define malloc (c-function pointer ("malloc" int)))
(define memcpy (c-function pointer ("memcpy" pointer pointer int)))
(define bzero (c-function void ("bzero" pointer int)))
(define free (c-function void ("free" pointer)))
(define (with-alloc x f)
  (let* ((data (malloc x))
	 (r (begin (bzero data x) (f data))))
    (free data)
    r))

(define (with-file-stream f filename mode)
 (let* ((file (fopen filename mode))
	(result (f file)))
  (fclose file)
  result))

(define (with-buffer-stream f buffer size mode)
 (let* ((file (fmemopen buffer size mode))
	(result (f file)))
  (fclose file)
  result))

(define fclose (c-function void ("fclose" pointer)))
(define fopen (c-function pointer ("fopen" string string)))
(define fmemopen (c-function pointer ("fmemopen" pointer unsigned string)))

(define (with-c-string str f)
 (with-alloc (+ (string-length str) 1)
	     (lambda (buf)
	      (for-each-indexed
	       (lambda (c i) (c-byte-set! buf i (char->integer c)))
	       (string->list str))
	      (f buf))))

(define (c-null-separated-strings->strings c-strings)
 (let loop ((c-strings c-strings) (strings '()) (i 0))
  (if (= 0 (c-s2cuint-ref c-strings 0))
      (reverse strings)
      (loop (+ c-strings (c-sizeof "void*"))
	    (cons (c-string->string (c-s2cuint-ref c-strings 0))
		  strings)
	    (+ i 1)))))

(define (with-vector->c-array f set-element element-size v)
 (with-array (vector-length v) element-size
	     (lambda (array)
	      (f (vector->c-array array v set-element element-size)))))

(define (with-c-pointers f v)
 (with-vector->c-array f c-s2cuint-set! c-sizeof-s2cuint v))

(define (list->c-array array l set-element element-size)
 (for-each-indexed (lambda (x i) (set-element array (* i element-size) x)) l)
 array)

(define (list->c-inexact-array array l element-size signed?)
  (list->c-array
   array
   l
   (c-sized-inexact-ptr-set! element-size signed?)
   element-size))

(define (list->c-exact-array array l element-size signed?)
  (list->c-array
   array
   l
   (c-sized-int-ptr-set! element-size signed?)
   element-size))

(define (vector->c-array array v set-element element-size)
 (for-each-vector
  (lambda (x i) (set-element array (* i element-size) x))
  v
  (enumerate-vector (vector-length v)))
 array)

(define (vector->c-inexact-array array v element-size signed?)
 (vector->c-array
  array
  v
  (c-sized-inexact-ptr-set! element-size signed?)
  element-size))

(define (vector->c-exact-array array v element-size signed?)
 (vector->c-array
  array
  v
  (c-sized-int-ptr-set! element-size signed?)
  element-size))

(define (with-array elements element-size f)
  (with-alloc (* elements element-size) f))

(define (c-array->list array get-element element-size nr-elements)
  (vector->list (c-array->vector array get-element element-size nr-elements)))

(define (c-array->vector array get-element element-size nr-elements)
  (map-n-vector
   (lambda (x) (get-element array (* x element-size))) nr-elements))

(define (c-exact-array->list array element-size nr-elements signed?)
  (vector->list
   (c-exact-array->vector array element-size nr-elements signed?)))

(define (c-exact-array->vector array element-size nr-elements signed?)
  (c-array->vector
   array
   (c-sized-int-ptr-ref element-size signed?) element-size nr-elements))

(define (c-inexact-array->list array element-size nr-elements signed?)
  (vector->list
   (c-inexact-array->vector array element-size nr-elements signed?)))

(define (c-inexact-array->vector array element-size nr-elements signed?)
  (c-array->vector
   array
   (c-sized-inexact-ptr-ref element-size signed?) element-size nr-elements))

(define (c-ptr-byte-offset ptr off)
  ((lap (ptr off)
	(POINTER_TSCP (PLUS
		       ("(char*)" (TSCP_POINTER ptr))
		       (TSCP_S2CINT off))))
   ptr off))

(define (c-sized-int-ptr-ref size signed?)
  (cond
   ((= size 1) c-byte-ref)
   ((= size c-sizeof-short) (if signed? c-shortint-ref c-shortunsigned-ref))
   ((= size c-sizeof-int)   (if signed? c-int-ref c-unsigned-ref))
   ((= size c-sizeof-long)  (if signed? c-longint-ref c-longunsigned-ref))
   (else (fuck-up))))

(define (c-sized-int-ptr-set! size signed?)
  (cond
   ((= size 1) c-byte-set!)
   ((= size c-sizeof-short) (if signed? c-shortint-set! c-shortunsigned-set!))
   ((= size c-sizeof-int)   (if signed? c-int-set! c-unsigned-set!))
   ((= size c-sizeof-long)  (if signed? c-longint-set! c-longunsigned-set!))
   (else (fuck-up))))

(define (c-sized-inexact-ptr-ref size signed?)
  (cond
   ((= size c-sizeof-float) c-float-ref)
   ((= size c-sizeof-double) c-double-ref)
   (else (fuck-up))))

(define (c-sized-inexact-ptr-set! size signed?)
  (cond
   ((= size c-sizeof-float) c-float-set!)
   ((= size c-sizeof-double) c-double-set!)
   (else (fuck-up))))

(define (matrix->c-array array m set-element element-size)
 (for-each-n
   (lambda (i)
    (for-each-n
     (lambda (j)
      (c-float-set!
       array
       (* c-sizeof-float (+ j (* i (matrix-columns m))))
       (matrix-ref m i j)))
     (matrix-columns m)))
   (matrix-rows m)))

(define (pgm->float-buffer! pgm)
 (let ((array (malloc (* c-sizeof-float (pnm-width pgm) (pnm-height pgm)))))
  (matrix->c-array array (map-vector
			  (lambda (e)
			   (map-vector
			    (lambda (e) (* (/ e (pgm-maxval pgm)) 255))
			    e))
			  (pgm-grey pgm))
		   c-float-set!
		   c-sizeof-float)
  array))

(define popen (c-function pointer ("popen" string string)))
(define pclose (c-function int ("pclose" pointer)))

(define srand (c-function void ("srand" unsigned)))

;;; GMP internal, WIP see FIXMEs

;; http://gmplib.org/manual/Initializing-Rationals.html
(define gmp:c-rational-canonicalize (c-function void ("mpq_canonicalize" pointer)))
(define gmp:c-rational-init (c-function void ("mpq_init" pointer)))
(define gmp:c-rational-clear (c-function void ("mpq_clear" pointer)))
(define gmp:c-rational-set (c-function void ("mpq_set" pointer pointer)))
(define gmp:c-rational-set-unsigned (c-function void ("mpq_set_ui" pointer unsigned unsigned)))
(define gmp:c-rational-set-signed (c-function void ("mpq_set_si" pointer unsigned unsigned)))
;;; FIXME memory leak here, since c doesn't free the string
(define gmp:c-rational-set-string (c-function void ("mpq_set_si" pointer int int)))
(define gmp:c-rational-swap (c-function void ("mpq_swap" pointer pointer)))
;; TODO http://gmplib.org/manual/Applying-Integer-Functions.html
;; mpq_numref and mpq_denref are macros
;; (define (gmp:c-rational-numerator r) ((lap () (POINTER_TSCP ("mpq_numref" r)))))
;; (define (gmp:c-rational-denominator r) ((lap () (POINTER_TSCP ("mpq_denref" r)))))

;; http://gmplib.org/manual/Rational-Arithmetic.html
(define gmp:c-rational-add (c-function void ("mpq_add" pointer pointer pointer)))
(define gmp:c-rational-sub (c-function void ("mpq_sub" pointer pointer pointer)))
(define gmp:c-rational-mul (c-function void ("mpq_mul" pointer pointer pointer)))
(define gmp:c-rational-div (c-function void ("mpq_div" pointer pointer pointer)))
(define gmp:c-rational-neg (c-function void ("mpq_neg" pointer pointer)))
(define gmp:c-rational-abs (c-function void ("mpq_abs" pointer pointer)))
(define gmp:c-rational-inv (c-function void ("mpq_abs" pointer pointer)))
;; http://gmplib.org/manual/Rational-Conversions.html
(define gmp:c-rational-set-double (c-function void ("mpq_set_d" pointer double)))
(define gmp:c-rational-get-double (c-function double ("mpq_get_d" pointer)))
(define gmp:c-rational-cmp (c-function int ("mpq_cmp" pointer pointer)))

(define (gmp:rational-pointer r) (%record-ref r 1))

;;; GMP external, WIP see FIXMEs

(define (% n d) (ratio->gmp:rational n d))

(define (gmp:rational? r)
 (and (%record? r) (equal? (%record-ref r 0) 'rational)))

(define (gmp:rational->string r)
 (let* ((c-str ((c-function pointer ("mpq_get_str" pointer int pointer))
		0 10 (gmp:rational-pointer r)))
	(str (c-string->string c-str)))
  (free c-str)
  str))

(define (gmp:create-rational)
 (let* ((r (%record 'rational (malloc (c-sizeof "mpq_t"))))
	(show (lambda (r port . ignore)
	       (let ((strings (pregexp-split "/" (gmp:rational->string r))))
		(format port "(% ~a ~a)"
			(first strings)
			(if (= (length strings) 1)
			    "1"
			    (second strings)))))))
  (gmp:c-rational-init (gmp:rational-pointer r))
  ;; FIXME This is a memory leak
  ;;
  ;; This is disabled because Scheme->C uses a list to store
  ;; finalizers and each additional finalizer leads to a linear
  ;; increase in lookup time. This degrades performance by around 100x
  ;;
  ;; (when-unreferenced r (lambda (r) (free (gmp:rational-pointer r))))
  (%record-methods-set! r `((%to-display . ,show)
			    (%to-write . ,show)
			    (free . ,(lambda (r)
				      (free (gmp:rational-pointer r))
				      (%record-set! r 1 0)))))
  r))

(define (number->gmp:rational n)
 (cond ((gmp:rational? n) n)
       ((exact? n) (ratio->gmp:rational n 1))
       ((inexact? n) (double->gmp:rational n))
       (else (fuck-up))))
(define (ratio->gmp:rational n d)
 (let ((r (gmp:create-rational)))
  (gmp:c-rational-set-signed (gmp:rational-pointer r) n d)
  (gmp:c-rational-canonicalize (gmp:rational-pointer r))
  r))
(define (double->gmp:rational d)
 (let ((r (gmp:create-rational)))
  (gmp:c-rational-set-double (gmp:rational-pointer r) d)
  r))
(define (gmp:rational->double r)
 (gmp:c-rational-get-double (gmp:rational-pointer r)))
(define (gmp:rational-+ a b)
 (let ((r (gmp:create-rational)))
  (gmp:c-rational-add
   (gmp:rational-pointer r) (gmp:rational-pointer a) (gmp:rational-pointer b))
  r))
(define (gmp:rational-- a b)
 (let ((r (gmp:create-rational)))
  (gmp:c-rational-sub
   (gmp:rational-pointer r) (gmp:rational-pointer a) (gmp:rational-pointer b))
  r))
(define (gmp:rational-* a b)
 (let ((r (gmp:create-rational)))
  (gmp:c-rational-mul
   (gmp:rational-pointer r) (gmp:rational-pointer a) (gmp:rational-pointer b))
  r))
(define (gmp:rational-/ a b)
 (let ((r (gmp:create-rational)))
  (gmp:c-rational-div
   (gmp:rational-pointer r) (gmp:rational-pointer a) (gmp:rational-pointer b))
  r))
(define (gmp:rational-inv a)
 (let ((r (gmp:create-rational)))
  (gmp:c-rational-inv
   (gmp:rational-pointer r) (gmp:rational-pointer a))
  r))
(define (gmp:rational-neg a)
 (let ((r (gmp:create-rational)))
  (gmp:c-rational-neg
   (gmp:rational-pointer r) (gmp:rational-pointer a))
  r))
(define (gmp:rational-abs a)
 (let ((r (gmp:create-rational)))
  (gmp:c-rational-abs
   (gmp:rational-pointer r) (gmp:rational-pointer a))
  r))
(define (gmp:rational-= a b)
 (not (= ((c-function int ("mpq_equal" pointer pointer))
	  (gmp:rational-pointer a)
	  (gmp:rational-pointer b)) 0)))

;; FIXME a bad interaction between scheme integers and the 32bit ints returned by mpq_cmp means that -1 shows up as 2^32-1
(define (gmp:rational-> a b)
 (= (gmp:c-rational-cmp (gmp:rational-pointer a) (gmp:rational-pointer b)) 1))
(define (gmp:rational-< a b)
 (> (gmp:c-rational-cmp (gmp:rational-pointer a) (gmp:rational-pointer b)) 1))
(define (gmp:rational->= a b) (not (gmp:rational-< a b)))
(define (gmp:rational-<= a b) (not (gmp:rational-> a b)))
