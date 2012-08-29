(MODULE
  DISTANCE-TRANSFORM
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

(include "QobiScheme-AD.sch")
(include "distance-transform.sch")

(set! *program* "distance-transform")
(set! *panic?* #t)

(define-command (main (required (sequence "sequence" string-argument))
		      (required (person "person" string-argument))
		      (required (location "location" string-argument))
		      (required (n "n" integer-argument))
		      (optional (name "name" string-argument "frame-canny.pgm"))
		      (optional (threshold "threshold" integer-argument 1))
		      (optional (min-frames "min-frames" integer-argument 0))
		      (optional (max-frames "max-frames" integer-argument infinity)))
 (let ((video-name (standard-corpus-video "office" sequence person location n)))
  (map-m-n
   (lambda (frame)
    (format #t "~a~%" (generic-pathname video-name frame name))
    (unless
      (file-exists?
       (generic-pathname video-name frame (string-append name ".dt"))) 
     (write-object-to-file
      (distance-transform
       (pgm->pbm (read-pnm (generic-pathname video-name frame name)) threshold))
      (generic-pathname video-name frame (string-append name ".dt")))))
   (max min-frames 0)
   (min max-frames (- (video-length video-name) 1)))))
