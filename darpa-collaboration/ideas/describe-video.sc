(MODULE
 DESCRIBE-VIDEO
 (WITH
  QOBISCHEME
  XLIB
  TOOLLIB-MATLAB
  TOOLLIB-MISC
  TOOLLIB-C-BINDINGS
  TOOLLIB-IMAGE-PROCESSING
  IDEALIB-HASH-TABLE
  IDEALIB-PREGEXP
  IDEALIB-STUFF
  IDEALIB-MATPLOTLIB
  HMM-WBM
  HMM-TRAIN-CLASSIFY)
 (MAIN MAIN))

(include "QobiScheme-AD.sch")
(include "describe-video.sch")

(set! *program* "describe-video")
(set! *panic?* #f)

(define-command (main (required (likelihoods "likelihoods" string-argument))
		      (required (destination "destination" string-argument)))
 (let ((locations (map fields (read-file "/home/snarayan/tmp/new-locations"))))
  (write-object-to-file
   (map (lambda (pathname)
	 (write pathname) (newline)
	 (let ((results (take 3 (sort (map (lambda (v) (maximump v result-loglk))
					   (transpose-list-of-lists
					    (read-object-from-file pathname)))
				      >
				      result-loglk))))
	  (list
	   (first (result-name (first results)))
	   (map (lambda (result)
		 (let ((video
			(string->darpa-video (first (result-name result)))))
		  (list
		   (sententify
		    (flatten
		     (generate-sentence
		      (result-verb result)
		      (map first
			   (fast-read-boxes
			    (server-specific-track-pathname
			     (second (assoc (first (result-name result)) locations))
			     video
			     ((if (list? (second (result-name result)))
				  first
				  identity)
			      (second (result-name result))))))
		      (if (list? (second (result-name result)))
			  (map first
			       (fast-read-boxes
				(server-specific-track-pathname
				 (second (assoc (first (result-name result)) locations))
				 video
				 (second (second (result-name result))))))
			  #f))))
		   (result-loglk result)
		   (second (result-name result)))))
		results))))
	(directory-list (format #f "~a/*" likelihoods)))
   destination)))
