(MODULE
  VERB-FUN
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
(include "verb-fun.sch")

;;;
;;; This file uses functionality found in verb-ideas.sc, the contents of which should
;;; be copied to the end of this file before compiling.
;;;
;;; Did not use "verb-fun" toward the final runs whilst working on the hand-detector,
;;; so not everything is guaranteed to work correctly from this file.
;;;
;;; -- Aaron
;;;

(set! *program* "verb-fun")
(set! *panic?* #t)
 
(define-command
 (main (required (operation "operation" string-argument))
       (required (corpus "corpus" string-argument))
       (required (html-root "http-root" string-argument))
       (required (verb "verb" string-argument))
       (required (every-nth "every-nth" integer-argument))
       (required (offset "offset" integer-argument))
       (required (recalculate "recalculate" string-argument))
       (required (lookahead "lookahead" integer-argument))
       (required (frame-len "frame-len" integer-argument))
       (required (stationary-v-t "stationary-v-t" real-argument))
       (required (run-v-t "run-v-t" real-argument))
       (required (overlap "overlap" integer-argument)))
 (let* ((list-of-verbs
	 (cond
	  ((equal? verb ".") (map first *principled-verbs*))
	  ((equal? verb "w") (written-verbs))
	  (else (list verb))))
	(recalculate (equal? "t" recalculate))
	(c-lookahead
	 (if (< lookahead 0) (detect-model-lookahead *default-detect-model*) lookahead))
	(c-frame-len
	 (if (< frame-len 0) (detect-model-required-frames *default-detect-model*) frame-len))
	(c-stationary-v-t
	 (if (< stationary-v-t 0)
	     (detect-model-velocity-threshold-stationary *default-detect-model*)
	     stationary-v-t))
	(c-run-v-t
	 (if (< run-v-t 0) (detect-model-velocity-threshold-walk *default-detect-model*)
	     run-v-t))
	(c-overlap
	 (if (< overlap 0) (detect-model-overlap-threshold *default-detect-model*) overlap))
        (model (make-simple-model c-lookahead c-frame-len c-stationary-v-t c-run-v-t c-overlap)))
  (unless (equal? operation "list-verbs")
   (if recalculate
       (format #t "Recalculate set to TRUE~%")
       (format #t "Recalculate false. Must pass 't' if you want to regenerate everything~%")))
  (format #t "Using model ~a~%" (detect-model-name model))
  (principled-verb-operations operation
			      corpus
			      html-root
			      list-of-verbs
			      model
			      every-nth
			      offset
			      recalculate)
  (exit 1)
  (exit)))



