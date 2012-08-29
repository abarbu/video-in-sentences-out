(MODULE
  IS-WELL-FORMED-SC-OBJECT-FILE
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
(include "is-well-formed-sc-object-file.sch")

(set! *program* "is-well-formed-sc-object-file")
(set! *panic?* #t) 
 
(define-command
 (main (required (filename "filename" string-argument)))
 (read-object-from-file filename)
 (display "If you can see this text, then the file was well-formed.") (newline))


