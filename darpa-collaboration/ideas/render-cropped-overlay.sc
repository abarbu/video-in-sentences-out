(MODULE
  RENDER-CROPPED-OVERLAY
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
(include "render-cropped-overlay.sch")

(set! *program* "render-cropped-overlay")
(set! *panic?* #f)

(define (render-all video frame)
 (define (mask n)
  (format #f "~a/video-datasets/C-D1a/purdue36/~a/~a/results-closure/frame-cropped-person-1_solution_~a.jpg"
	  (getenv "HOME")
	  (darpa-video->name video)
	  (number->padded-string-of-length frame 4)
	  (number->padded-string-of-length n 3)))
 (define (ppm)
  (format #f "~a/video-datasets/C-D1a/purdue36/~a/~a/frame-cropped-person-1.ppm"
	  (getenv "HOME")
	  (darpa-video->name video)
	  (number->padded-string-of-length frame 4)))
 (define (render-outline a)
  (begin
   (let ((i (read-pnm (ppm))))
    (for-each
     (lambda (p) (set-ppm-pixel! i (x p) (y p) '#(255 0 0)))
     (region->boundary
      (pbm->points
       (pgm->pbm
	(begin
	 (system (format #f "mogrify -format pgm ~a" (mask a)))
	 (let ((image (read-pnm (replace-extension (mask a) "pgm"))))
	  (replace-extension (mask a) "pgm")
	  image))
	176))))
    i)))
 (for-each-n
  (lambda (a)
   (when (file-exists? (mask a))
    (call-with-video-output-file
     (format #f "/tmp/all-closure-solutions/~a-~a-~a.jpeg"
	     (darpa-video->name video)
	     (number->padded-string-of-length frame 4)
	     (number->padded-string-of-length a 3))
     "Jpeg"
     1
     (lambda (port) (write-pnm-to-video-output-port (render-outline a) port)))))
  11))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))))
 (system (format #f "mkdir /tmp/all-closure-solutions"))
 (let ((v (cond (standard? (standard-corpus-video corpus sequence person location n))
		(darpa? (string->darpa-video name))
		(else (fuck-up)))))
  (map-n (lambda (a) (format #t "~a~%" a) (render-all v (+ a 1)))
	 (- (video-length v) 1))))
