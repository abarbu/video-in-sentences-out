;;; C bindings for Scheme->C

(eval-when
 (compile)

 (define (reduce f l i)
   (cond ((null? l) i)
 	((null? (rest l)) (first l))
  	(else (let loop ((l (rest l)) (c (first l)))
  	       (if (null? l) c (loop (rest l) (f c (first l))))))))

 (define *c-ffi:renaming-rules* '())
 (define *c-ffi:custom-types* '())

 (define gensym-data 0)
 (define (gensym x)
   (set! gensym-data (+ gensym-data 1))
   (string->symbol (format "gensym-~a-~s-~a" module-name gensym-data x)))

 (define (zip a b)
   (if (or (null? a) (null? b))
       '()
       (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))

 (define (c-ify x)
   (list->string (map (lambda (x) (if (eq? x #\-) #\_ x)) (string->list x))))

 ;; Lifted from QobiScheme
 (define (map-n f n)
   ;; needs work: To eliminate REVERSE.
   (let loop ((i 0) (c '()))
     (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

 (define (c-out-convert type)
   (case type
     ((bool) "bool_tscp")
     ((int) 'S2CINT_TSCP)
     ;; TODO ANDREI Add long support
     ((long) 'S2CINT_TSCP)
     ((char) 'CHAR_TSCP)
     ((unsigned) 'S2CUINT_TSCP)
     ;; TODO ANDREI Add float support
     ((float) 'DOUBLE_TSCP)
     ((double) 'DOUBLE_TSCP)
     ((pointer) 'POINTER_TSCP)
     ((string) 'CSTRING_TSCP)
     (else (error "macro" (format "Can't convert ~a to Scheme from C" type)))))
 (define (c-in-convert x)
   (case x
     ((bool) "tscp_bool")
     ((int) 'TSCP_S2CINT)
     ;; TODO ANDREI Add long support
     ((long) 'TSCP_S2CINT)
     ((char) 'TSCP_CHAR)
     ((unsigned) 'TSCP_S2CUINT)
     ;; TODO ANDREI Add float support
     ((float) 'TSCP_DOUBLE)
     ((double) 'TSCP_DOUBLE)
     ((pointer) 'TSCP_POINTER)
     ((string) 'TSCP_POINTER)
     (else (error "macro" (format "Can't convert ~a to C from Scheme" x)))))

 (define (append-symbols . s)
   (string->symbol (reduce string-append (map symbol->string s) "")))

 (define (c-set-convert x)
   (case x
     ((char) 'c-byte-set!)
     ((short) 'c-shortint-set!)
     ((unsigned-short) 'c-shortunsigned-set!)
     ((int) 'c-int-set!)
     ((unsigned) 'c-unsigned-set!)
     ((long) 'c-longint-set!)
     ((unsigned-long) 'c-longunsigned-set!)
     ((pointer) 'c-s2cuint-set!)
     ((float) 'c-float-set!)
     ((double) 'c-double-set!)
     (else (error "macro" (format "~a doesn't support ~a" 'c-set-convert  x)))))

 (define (c-ref-convert x)
   (case x
     ((char) 'c-byte-ref)
     ((short) 'c-shortint-ref)
     ((unsigned-short) 'c-shortunsigned-ref)
     ((int) 'c-int-ref)
     ((unsigned) 'c-unsigned-ref)
     ((long) 'c-longint-ref)
     ((unsigned-long) 'c-longunsigned-ref)
     ((pointer) 'c-s2cuint-ref)
     ((float) 'c-float-ref)
     ((double) 'c-double-ref)
     (else (error "macro" (format "~a doesn't support ~a" 'c-ref-convert x)))))

 (define (schemeify-name x)
   (if (assoc x *c-ffi:renaming-rules*)
       (second (assoc x *c-ffi:renaming-rules*))
       (list->string (map (lambda (x) (if (eq? x #\_)
					  #\-
					  x)) (string->list x)))))

 (define (string->pretty-symbol s)
   (string->symbol
    (list->string
     (map char-upcase (string->list s))))))

;;; horribly ugly hack to get an #include at the toplevel
;;; only use at the toplevel, otherwise unspeakable horrors will unfold
(define-macro c-include
  (lambda (f e)
    `(define (,(gensym 0)) ((lap () ,(string-append "0 );}
  #include<" (string-append
	      (second f)
	      (let ((dummy (c-ify (symbol->string (gensym 0)))))
		(string-append ">
int " (string-append dummy
		     (string-append
		      "(){ return 1;"
		      (string-append
		       dummy "("))))))))))))

;;; Required for offsetof
(c-include "stddef.h")

(define-macro typedef-offset
  (lambda (f e)
    (e `((lap () (C_FIXED ("offsetof"
			   ,(second f)
			   ,(third f))))) e)))

(define-macro struct-offset
  (lambda (f e)
    (e `((lap () (C_FIXED ("offsetof"
			   ,(string-append "struct " (second f))
			   ,(third f))))) e)))

(define-macro union-offset
 (lambda (f e)
  (e `((lap () (C_FIXED ("offsetof"
			 ,(string-append "union " (second f))
			 ,(third f)
			 ;; ,(string-append (third f) "." (fourth f))
			 )))) e)))

(define-macro annon-offset
 (lambda (f e)
  (e `((lap () (C_FIXED ("offsetof"
			 ,(second f)
			 ,(third f)
			 ;; ,(string-append (third f) "." (fourth f))
			 )))) e)))

(define-macro c-value
  (lambda (f e) `((lap () (,(c-out-convert (second f)) ,(third f))))))

(define-macro c-function
 (lambda (f e)
  (e
   (let* ((c-vars (cdaddr f))
	  (vars (map-n gensym (length c-vars))))
    `(lambda ,vars
      ,(let
	 ((body
	   `(let (,@(map (lambda (x)
			  (if (assoc (first x) *c-ffi:custom-types*)
			      `(,(second x) (,(fourth (assoc (first x) *c-ffi:custom-types*)) ,(second x)))
			      `(,(second x) ,(second x))))
			 (zip c-vars vars)))
	     ((lap ,vars
		   ,((if (eq? (second f) 'void)
			 (lambda (x) x)
			 (lambda (x) `(,(c-out-convert
					 (if (assoc (second f) *c-ffi:custom-types*)
					     (second (assoc (second f) *c-ffi:custom-types*))
					     (second f)))
				       ,x)))
		     `(,(caaddr f)
		       ,@(map (lambda (x) 
			       (list (c-in-convert 
				      (if (assoc (first x) *c-ffi:custom-types*)
					  (second (assoc (first x) *c-ffi:custom-types*))
					  (first x)))
				     (second x)))
			      (zip c-vars vars)))))
	      ,@vars))))
	(if (eq? (second f) 'void)
	    `(begin ,body ((lap () FALSEVALUE)))
	    (if (assoc (second f) *c-ffi:custom-types*)
		`(,(third (assoc (second f) *c-ffi:custom-types*)) ,body)
		body)))))
   e)))



(define-macro c-define-field
  (lambda (f e)
    (e (let ((struct (second f))
	     (field (third f))
	     (field-type (fourth f))
	     (offst (fifth f)))
	 `(begin
	    (define (,(append-symbols struct '- field '- 'set!) o v)
	      (,(c-set-convert field-type) o ,offst v))
	    (define (,(append-symbols struct '- field) o)
	      (,(c-ref-convert field-type) o ,offst))
	    (define (,(append-symbols struct '- field '- 'update) o f)
	      (,(append-symbols struct '- field '- 'set!) o
	       (f (,(append-symbols struct '- field) o)))))) e)))

(define-macro c-define-struct-field-full
  (lambda (f e)
    (e (let* ((struct      (second f))
	      (struct-name (third f))
	      (field       (fourth f))
	      (field-name  (fifth f))
	      (field-type  (sixth f))
	      (offst `(struct-offset ,struct-name ,field-name)))
	 `(begin
	    (define (,(append-symbols struct '- field '- 'set!) o v)
	      (,(c-set-convert field-type) o ,offst v))
	    (define (,(append-symbols struct '- field) o)
	      (,(c-ref-convert field-type) o ,offst))
	    (define (,(append-symbols struct '- field '- 'update) o f)
	      (,(append-symbols struct '- field '- 'set!) o
	       (f (,(append-symbols struct '- field) o)))))) e)))

(define-macro c-define-struct-field
  (lambda (f e)
    (e (let* ((struct-name (second f))
	      (struct (string->pretty-symbol (schemeify-name struct-name)))
	      (field-name (third f))
	      (field (string->pretty-symbol (schemeify-name field-name)))
	      (type (fourth f)))
	 `(c-define-struct-field-full ,struct ,struct-name
				      ,field ,field-name ,type)) e)))

(define-macro c-define-union-field-full
 (lambda (f e)
  (e (let* ((union      (second f))
			(union-name (third f))
			(field       (fourth f))
			(field-name  (fifth f))
			(field-type  (sixth f))
			(offst `(union-offset ,union-name ,field-name)))
	  `(begin
	    (define (,(append-symbols union '- field '- 'set!) o v)
		 (,(c-set-convert field-type) o ,offst v))
	    (define (,(append-symbols union '- field) o)
		 (,(c-ref-convert field-type) o ,offst))
	    (define (,(append-symbols union '- field '- 'update) o f)
		 (,(append-symbols union '- field '- 'set!) o
		  (f (,(append-symbols union '- field) o)))))) e)))

(define-macro c-define-union-field
 (lambda (f e)
  (e (let* ((union-name (second f))
			(union (string->pretty-symbol (schemeify-name union-name)))
			(field-name (third f))
			(field (string->pretty-symbol (schemeify-name field-name)))
			(type (fourth f)))
	  `(c-define-union-field-full ,union ,union-name
								  ,field ,field-name ,type)) e)))

(define-macro c-define-annon-field-full
 (lambda (f e)
  (e (let* ((annon      (second f))
			(annon-name (third f))
			(field       (fourth f))
			(field-name  (fifth f))
			(field-type  (sixth f))
			(offst `(annon-offset ,annon-name ,field-name)))
	  `(begin
	    (define (,(append-symbols annon '- field '- 'set!) o v)
		 (,(c-set-convert field-type) o ,offst v))
	    (define (,(append-symbols annon '- field) o)
		 (,(c-ref-convert field-type) o ,offst))
	    (define (,(append-symbols annon '- field '- 'update) o f)
		 (,(append-symbols annon '- field '- 'set!) o
		  (f (,(append-symbols annon '- field) o)))))) e)))

(define-macro c-define-annon-field
 (lambda (f e)
  (e (let* ((annon-name (second f))
			(annon (string->pretty-symbol (schemeify-name annon-name)))
			(field-name (third f))
			(field (string->pretty-symbol (schemeify-name field-name)))
			(type (fourth f)))
	  `(c-define-annon-field-full ,annon ,annon-name
								  ,field ,field-name ,type)) e)))

(define-macro c-sizeof
  (lambda (f e)
    (e `((lap () (C_FIXED ("sizeof" ,(second f))))) e)))

(define-macro c-ffi:rename-id
 ;; (c-ffi:rename-id "_Image" "imagemagick-image")
 (lambda (f e)
  (e `(eval-when (compile)
       (set! *c-ffi:renaming-rules*
	     (cons (list ,(second f) ,(third f)) *c-ffi:renaming-rules*))) e)))

(define-macro c-ffi:add-custom-type
 ;; (c-ffi:add-custom-type zip pointer 
 ;;                        make-zip-archive zip-archive-handle)
 (lambda (f e)
  (e `(eval-when (compile)
       (set! *c-ffi:custom-types*
	     (cons (list ',(second f) ',(third f) ',(fourth f) ',(fifth f))
		   *c-ffi:custom-types*))) e)))

(eval-when (eval)
  (define-macro c-include (lambda (f e) #f))
  (define-macro c-function (lambda (f e) #f))
  (define int #f)
  (define pointer #f)
  (define void #f))

;;; Get rid of a malloc warning
(c-include "stdlib.h")
(c-include "string.h")

(define-macro time
 (lambda (form expander)
  (expander `(time-thunk ,(second form) (lambda () ,(third form))) expander)))

(define-macro time-code
 (lambda (form expander)
  (expander `(time-thunk (format #f "~a~~a : ~a~%"
				 (make-string *time-depth* #\+)
				 ,(format #f "~a" (second form)))
			 (lambda ()
			  (set! *time-depth* (+ *time-depth* 1))
			  (let ((ret ,(second form)))
			   (set! *time-depth* (- *time-depth* 1))
			   ret)))
	    expander)))

(define-external *time-depth* idealib-stuff)
(define-external (time-thunk format-string thunk) idealib-stuff)
