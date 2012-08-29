(require-extension ssax)

(define (stringify l)
 (cond ((null? l) l)
       ((string? l) l)
       ((number? l) l)
       ((list? l) (map stringify l))
       ((symbol? l) (symbol->string l))
       (else (error l))))

(if (< (length (command-line-arguments)) 2)
    (begin (display "Usage: input-xml-file output-sc-file")(newline))
    (let ((input (car (command-line-arguments)))
	  (output (cadr (command-line-arguments))))
     (call-with-output-file
       output
      (lambda (out-port)
       (write
	(call-with-input-file
	  input
	 (lambda (in-port)
	  (stringify (ssax:xml->sxml in-port '()))))
	out-port)))))
