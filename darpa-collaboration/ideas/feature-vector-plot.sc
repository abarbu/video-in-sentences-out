(MODULE
  FEATURE-VECTOR-PLOT
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

(include "QobiScheme.sch")
(include "feature-vector-plot.sch")

(set! *program* "feature-vector-plot")
(set! *panic?* #t) 
 
(define-command
 (main (exactly-one
	("standard" standard?
	 (corpus "corpus" string-argument "")
	 (sequence "sequence" string-argument "")
	 (person "person" string-argument "")
	 (location "location" string-argument "")
	 (n "n" integer-argument 0))
	("darpa" darpa? (name "name" string-argument ""))
	("stand-alone" stand-alone? (path "path" string-argument ""))
	("demo" demo? (demo-name "name" string-argument ""))
	("verb" by-corpus?
	 (by-corpus "corpus" string-argument "")
	 (verb "verb" string-argument"")))
       (required (lookahead "lookahead" integer-argument))
       (required (destination-directory "destination-directory" string-argument)))
 (if by-corpus?
     (generate-graphs by-corpus (list verb) lookahead destination-directory)
     (let ((video (cond (standard? (standard-corpus-video corpus sequence person location n))
			(darpa? (string->darpa-video name))
			(stand-alone? (make-stand-alone-video path))
			(demo? (string->demo-video demo-name))
			(else (fuck-up)))))
      (system (format #f "mkdir -p ~a" destination-directory))
      (plot-all-track-features video lookahead destination-directory ""))))





