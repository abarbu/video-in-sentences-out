(MODULE

TOOLLIB-MATLAB)

;;; TODO Scheme to matlab conversion and proper matlab function calls
;;; TODO Matlab cell, struct, logical and function
;;; TODO Support for building matlab structures
;;; TODO Support for complex numbers

(include "QobiScheme.sch")
(include "toollib-c-macros.sch")
(include "toollib-matlab.sch")

(c-include "toollib-matlab.h")
(c-include "engine.h")

;; The current matlab engine
(define *engine* #f)

(define *matlab-class-unknown*  (c-value int "mxUNKNOWN_CLASS"))
(define *matlab-class-cell*     (c-value int "mxCELL_CLASS"))
(define *matlab-class-struct*   (c-value int "mxSTRUCT_CLASS"))
(define *matlab-class-logical*  (c-value int "mxLOGICAL_CLASS"))
(define *matlab-class-char*     (c-value int "mxCHAR_CLASS"))
(define *matlab-class-double*   (c-value int "mxDOUBLE_CLASS"))
(define *matlab-class-single*   (c-value int "mxSINGLE_CLASS"))
(define *matlab-class-int8*     (c-value int "mxINT8_CLASS"))
(define *matlab-class-uint8*    (c-value int "mxUINT8_CLASS"))
(define *matlab-class-int16*    (c-value int "mxINT16_CLASS"))
(define *matlab-class-uint16*   (c-value int "mxUINT16_CLASS"))
(define *matlab-class-int32*    (c-value int "mxINT32_CLASS"))
(define *matlab-class-uint32*   (c-value int "mxUINT32_CLASS"))
(define *matlab-class-int64*    (c-value int "mxINT64_CLASS"))
(define *matlab-class-uint64*   (c-value int "mxUINT64_CLASS"))
(define *matlab-class-function* (c-value int "mxFUNCTION_CLASS"))

(define *matlab-sizeof-mwSize* (c-sizeof "mwSize"))

;;; Roughly the number of elements a vector can have when sent over
;;; the matlab string interface, anything bigger than this must be
;;; written to a file first; the actual number seems to be close to
;;; 200. I have no idea how to increase the size of the string buffer
;;; that matlab uses

(define *matlab-largest-string* 150)

;;; Matlab engine bindings

(define matlab-start (c-function pointer ("engOpen" string)))
(define matlab-stop (c-function int ("engClose" pointer)))
(define matlab-variable
 (c-function pointer ("engGetVariable" pointer string)))
(define matlab-variable-set!
 (c-function int ("engPutVariable" pointer string pointer)))
(define c-matlab-eval-string (c-function int ("engEvalString" pointer string)))
(define matlab-set-output-buffer
 (c-function int ("engOutputBuffer" pointer pointer int)))
(define matlab-set-visible (c-function int ("engSetVisible" pointer int)))
(define matlab-get-visible (c-function int ("engGetVisible" pointer pointer)))

(define (matlab-eval-string s) (start-matlab!) (c-matlab-eval-string *engine* s))

(define (matlab-eval-strings . strings)
 (for-each (lambda (s) (matlab-eval-string s)) strings))

(define (matlab . strings)
 (with-matlab-default-output-buffer
  (lambda (matlab-result-string)
   (apply matlab-eval-strings strings)
   (matlab-result-string))))

(define *default-matlab-engine-command*
 (string-append
  "matlab -nosplash -nodesktop "
  (let ((args (getenv "MATLAB_LOCAL_ARGS"))) (if  args args ""))))
(define *default-matlab-buffer-size* 5000)

(define (with-matlab-default-engine f)
 (with-matlab-engine *default-matlab-engine-command* f))

(define (with-matlab-engine str f)
 (set! *engine* (matlab-start str))
 (let ((result (f)))
  (matlab-stop *engine*)
  result))

(define (with-matlab-default-output-buffer f)
 (with-matlab-output-buffer *default-matlab-buffer-size* f))

(define (with-matlab-output-buffer size f)
 (with-alloc
  size
  (lambda (buffer)
   (matlab-set-output-buffer *engine* buffer size)
   (let ((result (f (lambda () (c-string->string buffer)))))
    (matlab-set-output-buffer *engine* null 0)
    result))))

(define (start-matlab!)
 (unless *engine*
  (set! *engine* (matlab-start *default-matlab-engine-command*))))

;;; Matlab MX Array bindings

(define matlab-class-id (c-function int ("mxGetClassID" pointer)))
(define matlab-class-name (c-function string ("mxGetClassName" pointer)))
(define matlab-array->string (c-function string ("mxArrayToString" pointer)))
(define matlab-calloc (c-function pointer ("mxCalloc" int int)))
(define matlab-malloc (c-function pointer ("mxMalloc" int)))
(define matlab-free (c-function void ("mxFree" pointer)))
(define matlab-make-numeric-matrix
 (c-function pointer ("mxCreateNumericMatrix" int int int int)))
(define matlab-destroy (c-function void ("mxDestroyArray" pointer)))
(define matlab-nr-rows (c-function int ("mxGetM" pointer)))
(define matlab-nr-columns (c-function int ("mxGetN" pointer)))
(define matlab-nr-dimensions
 (c-function int ("mxGetNumberOfDimensions" pointer)))
(define matlab-nr-elements (c-function int ("mxGetNumberOfElements" pointer)))
(define matlab-dimensions-internal
 (c-function pointer ("mxGetDimensions" pointer)))
(define matlab-data-set! (c-function void ("mxSetData" pointer pointer)))
(define matlab-data (c-function void ("mxGetData" pointer)))
(define matlab-real-double-data (c-function pointer ("mxGetPr" pointer)))
(define matlab-real-double-data-set!
 (c-function void ("mxSetPr" pointer pointer)))
(define matlab-complex-double-data (c-function pointer ("mxGetPi" pointer)))
(define matlab-complex-double-data-set!
 (c-function void ("mxSetPi" pointer pointer)))
(define matlab-element-size (c-function int ("mxGetElementSize" pointer)))
(define matlab-calculate-index-internal
 (c-function int ("mxCalcSingleSubscript" pointer int pointer)))

(define (matlab-dimensions matrix)
 (c-exact-array->list (matlab-dimensions-internal matrix)
		      *matlab-sizeof-mwSize*
		      (matlab-nr-dimensions matrix)
		      #f))

(define (matlab-calculate-index matrix array subscripts)
 (matlab-calculate-index-internal
  matrix
  (length subscripts)
  (list->c-array array subscripts *matlab-sizeof-mwSize* #f)))

(define (matlab-string? m)
 (= *matlab-class-char* (matlab-class-id m)))

(define (matlab-matrix-exact? m)
 (eq? (car (matlab-matrix-numeric-type m)) 'exact))
(define (matlab-matrix-inexact? m)
 (eq? (car (matlab-matrix-numeric-type m)) 'inexact))
(define (matlab-matrix-signed? m)
 (eq? (cadr (matlab-matrix-numeric-type m)) 'signed))
(define (matlab-matrix-unsigned? m)
 (eq? (cadr (matlab-matrix-numeric-type m)) 'unsigned))

(define (matlab-matrix-numeric-type m)
 (let ((id (matlab-class-id m)))
  (cond
   ((or (= *matlab-class-double* id)
	(= *matlab-class-single* id))
    `(inexact signed))
   ((or (= *matlab-class-int8* id)
	(= *matlab-class-int16* id)
	(= *matlab-class-int32* id)
	(= *matlab-class-int64* id))
    `(exact signed))
   ((or (= *matlab-class-uint8* id)
	(= *matlab-class-uint16* id)
	(= *matlab-class-uint32* id)
	(= *matlab-class-uint64* id))
    `(exact unsigned))
   (else '(#f #f)))))

;;; Various handy utilities

(define (matlab-size-ref struct x)
 ((c-sized-int-ptr-ref *matlab-sizeof-mwSize* #f)
  struct (* *matlab-sizeof-mwSize* x)))

(define (matlab-matrix->list matrix signed?)
 (vector->list (matlab-matrix->matrix matrix signed?)))

(define (matlab-matrix->matrix matrix signed?)
 (let ((size (matlab-element-size matrix)))
  (matlab-data->vector
   (cond ((matlab-matrix-exact? matrix) (matlab-data matrix))
	 ((matlab-matrix-inexact? matrix) (matlab-real-double-data matrix))
	 (else (fuck-up)))
   (cond ((matlab-matrix-exact? matrix)
	  (lambda (array nr-elements)
	   (c-exact-array->vector array size nr-elements signed?)))
	 ((matlab-matrix-inexact? matrix)
	  (lambda (array nr-elements)
	   (c-inexact-array->vector array size nr-elements signed?)))
	 (else (fuck-up)))
   (matlab-element-size matrix)
   (reverse (matlab-dimensions matrix)))))

(define (matlab-data->vector data get-row element-size dimensions)
 (cond
  ((= 1 (length dimensions))
   (get-row data (car dimensions)))
  ((< 1 (length dimensions))
   (transpose
    (map-n-vector
     (lambda (n)
      (matlab-data->vector
       (c-ptr-byte-offset data
			  (* n element-size (apply * (cdr dimensions))))
       get-row element-size (cdr dimensions)))
     (car dimensions))))
  (else (fuck-up))))

(define (matlab-list-output t . file)
 (if (null? file)
     (matlab-matrix-output (list->vector (map list->vector t)))
     (matlab-matrix-output (list->vector (map list->vector t)) (car file))))

(define (matlab-matrix-output t . file)
 ((if (null? file)
      (lambda (f) (f #t))
      (lambda (f) (call-with-output-file (first file) f)))
  (lambda (port)
   (let loop ((a t))
    (for-each-vector (lambda (a) (if (vector? a)
				(begin (loop a) (format port "~%"))
				(format port " ~a" a)))
		     a)))))

(define (scheme-vector->matlab-string v)
 (string-append
  "["
  (foldl
   (lambda (i e)
    (string-append
     i
     ", "
     (number->string e)))
   (cdr (vector->list v))
   (number->string (vector-ref v 0)))
  "]"))

(define (scheme->matlab! variable s)
 (cond
  ((pnm? s)
   (with-temporary-file
    (cond ((pbm? s) "matlab.pbm")
		  ((pgm? s) "matlab.pgm")
		  ((ppm? s) "matlab.ppm"))
    (lambda (f)
     (write-pnm s f)
     (matlab-eval-string
      (format #f "~a=imread('~a');" variable f)))))
  ((and (matrix? s)
		(< (* (matrix-rows s) (matrix-columns s))
		   *matlab-largest-string*))
   (let ((vec (map-vector scheme-vector->matlab-string s)))
    (matlab-eval-string
     (format #f
			 "~a=~a"
			 variable
			 (string-append
			  "["
			  (foldl
			   (lambda (j f) (string-append j "; " f))
			   (cdr (vector->list vec))
			   (vector-ref vec 0))
			  "]")))))
  ((and (vector? s) (not (matrix? s))
		(< (vector-length s) *matlab-largest-string*))
   (matlab-eval-string
    (format #f
			"~a=~a;"
			variable
			(scheme-vector->matlab-string s))))
  ((vector? s)
   (with-temporary-file
    "/tmp/matlabmatrix.m"
    (lambda (f)
     (matlab-matrix-output s f)
     (matlab-eval-string
      (format #f "~a = importdata('~a', ' ');" variable f)))))
  ((list? s)
   (scheme->matlab! variable (list->vector s)))
  (else (fuck-up))))

(define (matlab->scheme m)
 (cond ((or (matlab-matrix-exact? m) (matlab-matrix-inexact? m))
		(matlab-matrix->matrix m (matlab-matrix-signed? m)))
       ((or (matlab-string? m)) (matlab-array->string m))
       (else (fuck-up))))

(define (with-matlab-variable var f)
 (let* ((x (matlab-variable *engine* var))
		(result (f x)))
  (matlab-destroy x)
  result))

(define (matlab-get-variable name)
 (with-matlab-variable name (lambda (var) (matlab->scheme var))))

(define (matlab-get-double name)
 (with-matlab-variable name (lambda (var) (x (x (matlab->scheme var))))))

(define (matlab-save-variables filename . variables)
 (matlab-eval-string
  (format #f "save ~a ~a"
		  filename
		  (reduce
		   string-append
		   (map (lambda (v) (format #f "~a " v)) variables)
		   ""))))

(define (matlab-load-variables filename)
 (matlab-eval-string (format #f "load ~a" filename)))

;; andrei debugging
(define (matlab-show-variable name)
 (with-matlab-variable
  name
  (lambda (var)
   (format #t "~a is ~a~%" name (matlab->scheme var)))))

(define (test-matlab)
 (with-matlab-default-engine
  (lambda ()
   (with-matlab-default-output-buffer
    (lambda (matlab-result-string)
     (matlab-eval-string "A=[[1,2,3];[4,5,6]]")
     (matlab-eval-string "B='Hi'")
     (matlab-eval-string "C=1")
     (matlab-show-variable "A")
     (matlab-show-variable "B")
     (matlab-show-variable "C")
     (matlab-eval-string "D=randn(2,2,2,2)")
     (display (matlab-result-string))
     (matlab-show-variable "D")
     (matlab-eval-string "'Can also get the output from matlab'")
     (display (matlab-result-string))
     (newline)
     (display "Bye")(newline))))))

(define (matlab-append-to-path directory)
 (matlab-eval-string
  (format "addpath(genpath('~a'))" directory)))

;;; Graphing functions

(define (maybe-looping v steps)
 (cond ((number? v) (lambda (f) (f v)))
       ((list? v)
	(unless (= (length v) 2) (fuck-up))
	(lambda (f)
	 (map-n
	  (lambda (n)
	   (f (+ (car v) (* n (/ (- (cadr v) (car v)) steps)))))
	  (+ steps 1))))
       (else (fuck-up))))

(define (evaluate-function f values steps)
 ((foldl
   (lambda (a b) (lambda (vs) (b (lambda (v) (a (cons v vs))))))
   (map (lambda (v) (maybe-looping v steps)) values)
   (lambda (v) (apply f v))) '()))
