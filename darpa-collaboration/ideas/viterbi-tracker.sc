(MODULE
  VITERBI-TRACKER
  (WITH
    QOBISCHEME
    XLIB
    CUPEDRO-BINDINGS
    HMM-TRAIN-CLASSIFY
    HMM-WBM
    IDEALIB-HASH-TABLE
    IDEALIB-MATPLOTLIB
    IDEALIB-PREGEXP
    IDEALIB-STUFF
    TOOLLIB-C-BINDINGS
    TOOLLIB-CAMERA
    TOOLLIB-HACK-TRACK-DRAWABLE
    TOOLLIB-HACK-TRACK
    TOOLLIB-IMAGE-PROCESSING
    TOOLLIB-MATLAB
    TOOLLIB-MISC)
  (MAIN MAIN))

;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2011 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "viterbi-tracker.sch")

(set! *program* "viterbi-tracker")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

(define *minimum-track-length* 10)
(define *maximum-track-overlap-ratio* 0.7)
(define *model-threshold-tracker-offset* -0.4)
(define *overgeneration-minimum-track-length* 0)
(define *overgeneration-maximum-track-overlap-ratio* 0.1)
(define *suppression-delta* 0.1)

;;; Procedures

;;; General stuff

;;; I/O

;;; Commands

;;; Top Level

(define-command
 (main
  (any-number ("m" model-names-groups? (model-names-groups "model-name-group" string-argument)))
  (at-most-one ("t" top-n? (top-n "top-n" integer-argument 0)))
  (at-most-one ("with-dt" with-dt?))
  (exactly-one ("standard" standard?
		(corpus "corpus" string-argument "")
		(sequence "sequence" string-argument "")
		(person "person" string-argument "")
		(location "location" string-argument "")
		(n "n" integer-argument 0))
	       ("darpa" darpa? (name "name" string-argument ""))
	       ("stand-alone" stand-alone? (path "path" string-argument "")))
  (at-most-one ("cuda-object-detector" cuda-object-detector?
		(threshold-offset "threshold-offset" real-argument 0)
		(nms "nms" integer-argument 0)))
  (at-most-one ("cuda-klt" cuda-klt?))
  (at-most-one ("cuda-optical-flow" cuda-optical-flow?))
  (at-most-one ("alpha" alpha? (alpha "alpha" real-argument 3)))
  (at-most-one ("beta" beta? (beta "beta" real-argument 3)))
  (required (model-path "model-path" string-argument))
  (required (look-ahead "look-ahead" integer-argument)))
 (let* ((video (cond
		(standard?
		 (standard-corpus-video corpus sequence person location n))
		(darpa? (string->darpa-video name))
		(stand-alone? (make-stand-alone-video path))
		(else (fuck-up)))))
  (read-and-viterbi-track
   video cuda-klt? cuda-optical-flow? cuda-object-detector?
   (if model-names-groups
       model-names-groups
       (get-cleaned-up-model-names-list video))
   nms alpha beta threshold-offset top-n with-dt?
   model-path *model-threshold-tracker-offset*
   #t look-ahead
   *minimum-track-length*
   *maximum-track-overlap-ratio*
   *overgeneration-minimum-track-length*
   *overgeneration-maximum-track-overlap-ratio*
   *suppression-delta*)))

;;;" "Tam" "V'Nishlam" "Shevah" "L'El" "Borei" "Olam
