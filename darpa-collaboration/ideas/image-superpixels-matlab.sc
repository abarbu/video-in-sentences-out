;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "image-superpixels-matlab.sch")

(set! *program* "image-superpixels-matlab")
(set! *panic?* #f)

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("image-name" image-name? (image-name "name" string-argument "")))
       (at-most-one ("num-superpixels" num? (num "name" integer-argument 100)))
       (at-most-one ("weight" weight? (weight "name" integer-argument 15)))
       )
 (set! *engine* (matlab-start *default-matlab-engine-command*))
 (let* ((video-name (cond (standard?
			   (standard-corpus-video corpus sequence person location n))
			  (darpa? (string->darpa-video name))
			  (else (fuck-up))))
	(image (read-pnm (generic-pathname video-name 1 (default-extension image-name "ppm"))))
	(width (pnm-width image))
	(height (pnm-height image)))
  (for-each-frame
   (lambda (frame)
    ;; TODO: support for essa and turbopixels
    (system
     (format #f "cd ~a; ~a/darpa-collaboration/bin/slic ~a ~a ~a"
	     (generic-pathname video-name frame "")
	     (getenv "HOME")
	     (generic-pathname video-name frame (default-extension image-name "ppm"))
	     num
	     weight))
    (scheme->matlab! "superpixels"
		     (read-slic-file (dtrace "a"(generic-pathname video-name frame (default-extension image-name "dat")))
				    width
				    height))
    (matlab-eval-strings "video.superpixels = superpixels")
    (matlab-save-variables (generic-pathname video-name frame (format #f "~a-seg.mat" image-name)) "video"))
   video-name)))

  
