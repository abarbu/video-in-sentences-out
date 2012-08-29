(MODULE
  DSCI
  (WITH
    QOBISCHEME
    XLIB
    CUPEDRO-BINDINGS
    HMM-TRAIN-CLASSIFY
    HMM-WBM
    IDEALIB-HASH-TABLE
    IDEALIB-MATPLOTLIB
    IDEALIB-PREGEXP
    IDEALIB-TRACKS
    IDEALIB-STUFF
    TOOLLIB-C-BINDINGS
    TOOLLIB-CAMERA
    TOOLLIB-HACK-TRACK-DRAWABLE
    TOOLLIB-HACK-TRACK
    TOOLLIB-IMAGE-PROCESSING
    TOOLLIB-MATLAB
    TOOLLIB-MISC)
  (MAIN MAIN))

(include "QobiScheme-AD.sch")

(include "toollib-c-macros.sch")
(include "toollib-c-bindings.sch")
(include "idealib-c-externals.sch")
(include "dsci.sch")

(eval-when
  (load)
 (include "QobiScheme.load"))

(set! *program* "dsci")
(set! *panic?* #f)
(define-c-external (nobuff) void "nobuff")
(nobuff)

(define (main arguments)
 (apply qobischeme-read-eval-print arguments))
