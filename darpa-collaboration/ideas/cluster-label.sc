(MODULE
  SCHEMEREADER
  (WITH
    QOBISCHEME
    XLIB
    TOOLLIB-MATLAB
    TOOLLIB-MISC
    TOOLLIB-C-BINDINGS
    TOOLLIB-IMAGE-PROCESSING
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    IDEALIB-MATPLOTLIB)
  (MAIN MAIN))
(include "QobiScheme.sch")
(include "cluster-label.sch")

(set! *program* "cluster-label")
(set! *panic?* #f)

(define (video-voc4-overgenerated-person-tracked-boxes-available video)
 (video-boxes-available video "voc4_overgenerated-person" "tracked-box"))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("cd1" cd1?)))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video-from-corpus name
							(if cd1?
							    "C-D1/recognition"
							    "C-E1/recognition")))
	       (else (fuck-up)))))
  (for-each
   (lambda (tb)
    (let* ((tracked-boxes (read-voc4-overgenerated-tracked-boxes video-name (second tb) (string->number (third tb))))
	   (medoid-list (map first (read-object-from-file "/home/zburchil/kmeddatamedoids")))
	   (closest-medoid-list
	    (map
	     (lambda (box)
	      (let* ((center (voc4-detection-center box))
		     (normal-parts
		      (list->vector (join (map
					   (lambda (part)
					    (vector->list
					     (normalize-line-in-voc4-box
					       (make-line-segment (part-center part) center) box)))
					   (voc4-detection-parts box))))))
	       (position (first (sort medoid-list < (lambda (m) (distance m normal-parts)))) medoid-list)))
	     tracked-boxes)))	       
     (write-object-to-file closest-medoid-list
			   (string-append "/aux/qobi/video-datasets/" (if cd1? "C-D1/recognition/" "C-E1/recognition/") name "/pose-clusters-voc4_overgenerated-" (second tb) "-" (third tb) ".sc"))))
   (video-voc4-overgenerated-person-tracked-boxes-available video-name))))