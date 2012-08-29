(MODULE
  DETECTOR-SVM-TRAIN
  (WITH
    QOBISCHEME
    XLIB
    TOOLLIB-MATLAB
    TOOLLIB-MISC
    TOOLLIB-C-BINDINGS
    TOOLLIB-IMAGE-PROCESSING
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    IDEALIB-MATPLOTLIB
    HMM-WBM
    HMM-TRAIN-CLASSIFY)
  (MAIN MAIN))
(include "QobiScheme.sch")
(include "detector-svm-train.sch")

(set! *program* "detector-svm-train")
(set! *panic?* #f)

(define (dtracef f s v) (format #t "~a: ~a~%" s (f v)) v)

(define (read-appropriate-statistics video row)
 (let ((entities (pregexp-split "," row)))
  ;; name mu sigma skewness kurtosis cbimodal mu2 sigma2 low-hit-ratio label
  ;; (cons video (list (list-ref entities 0) (list-ref entities 1) (list-ref entities 2)
  ;; 		    (list-ref entities 6) (list-ref entities 7) (list-ref entities 8)
  ;; 		    (list-ref entities 9) ))
  (cons video (list (list-ref entities 0) (list-ref entities 4) 
		    (list-ref entities 6) (list-ref entities 7) (list-ref entities 8)
		    (list-ref entities 9) ))
  ))

(define (read-statistics model profiles-path videos-list)
 (remove-if
  (lambda (l) (some (lambda (p) (or (string=? p "-nan.") (string=? p "nan."))) l))
  (map-reduce append
	      '()
	      (lambda (v)
	       (remove-if-not
		(lambda (e) (equal? (second e) model))
		(map (lambda (r) (read-appropriate-statistics v r))
		     (read-file (string-append  profiles-path "/" v)))))
	      (read-file videos-list))))

(define-command
 (main
  (exactly-one ("model" model?
		(name "name" string-argument "")
		(videos-list "file" string-argument "")
		(profiles-path "path" string-argument "")))
  (exactly-one ("output" output? (output "path" string-argument ""))))
 (start-matlab!)
 (let ((data (read-statistics name profiles-path videos-list))
       (data2 (read-statistics name profiles-path "/net/maniishaa/tmp/C-E1-testset")))
  (format #t "len: ~a~%  y: ~a ~%" (length data) (count-if (lambda (e) (string=? (last e) "1")) data))
  (format #t "len: ~a~%  y: ~a ~%" (length data2) (count-if (lambda (e) (string=? (last e) "1")) data2))
  ;; (write (first data)) (newline)
  (scheme->matlab! "data" (list-of-lists->matrix (map cddr data)))
  (scheme->matlab! "data2" (list-of-lists->matrix (map cddr data2)))
  (format #t "Data read into Matlab~%")
  (matlab "addpath('/home/snarayan/tmp/');"
	  "svmstruct = svm_train_models(data);"
	  "test=[];"
	  "for i=1:size(data,1); test(i)=svmclassify(svmstruct,data(i,1:end-1)); end;"
	  "test2=[];"
	  "for i=1:size(data2,1); test2(i)=svmclassify(svmstruct,data2(i,1:end-1)); end;"
	  (format #f "save('~a','svmstruct');"  output))
  (let ((test (map inexact->exact (vector->list (x (matlab-get-variable "test")))))
	(test2 (map inexact->exact (vector->list (x (matlab-get-variable "test2")))))
	(labels (map string->number (map last data)))
	(labels2 (map string->number (map last data2))))
   ;; (write-object-to-file (zip (map first data2) labels2) (string-append (strip-extension output) ".sc"))
   (format #t "Training Set:~%tp: ~a~%tn: ~a~%fp: ~a~%fn: ~a~%"
	   (count-if (lambda (p) (equal? p '(1 1))) (zip labels test))
	   (count-if (lambda (p) (equal? p '(0 0))) (zip labels test))
	   (count-if (lambda (p) (equal? p '(0 1))) (zip labels test))
	   (count-if (lambda (p) (equal? p '(1 0))) (zip labels test)))
   (format #t "Test Set:~%tp: ~a~%tn: ~a~%fp: ~a~%fn: ~a~%"
	   (count-if (lambda (p) (equal? p '(1 1))) (zip labels2 test2))
	   (count-if (lambda (p) (equal? p '(0 0))) (zip labels2 test2))
	   (count-if (lambda (p) (equal? p '(0 1))) (zip labels2 test2))
	   (count-if (lambda (p) (equal? p '(1 0))) (zip labels2 test2))))))
