(MODULE
  PRECOMPUTE-ANNOTATION
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
(include "precompute-annotation.sch")

(set! *program* "precompute-annotation")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

;; darpa-wrap ./precompute-annotation -darpa Run1_A1_C2_Act1_Park3_MC_AFTN_482699a6-c5af-11df-a1f0-e80688cb869a -prefix frame -frame 1 -slic

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument "")))
       (at-most-one ("prefix" prefix? (prefix "file" string-argument "")))
       (at-most-one ("level" level? (level "n" string-argument "")))
       (at-most-one ("frame" frame? (frame "n" integer-argument 0)))
       (at-most-one ("region" region?))
       (exactly-one ("slic" slic?) ("essa" essa?) ("turbopixels" turbopixels?)))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up))))
	(superpixel-type
	 (cond (slic? 'slic)
	       (essa? 'essa)
	       (turbopixels? 'turbopixels)
	       (else (fuck-up))))
	(image (read-pnm (generic-pathname video-name frame (string-append prefix ".ppm"))))
	(superpixels
	 (read-object-from-file
	  (superpixel-pathname video-name frame prefix superpixel-type)))
	(annotation (frame->empty-annotations superpixels)))
  (write-pnm
   (superpixel-annotate-blank-ppm
    annotation
    (ppm-constant (pnm-width image) (pnm-height image) 255 255 255))
   (cached-blank-annotation-pathname video-name frame prefix superpixel-type))
  (write-pnm
   (superpixel-annotation-on-ppm annotation image)
   (cached-ppm-annotation-pathname video-name frame prefix superpixel-type))
  (write-binary-int-matrix
   (superpixels->map superpixels (pnm-width image) (pnm-height image))
   (cached-superpixel-map-pathname video-name frame prefix superpixel-type))
  (write-object-to-file
   (second (frame->regions-neighbours (superpixels->map superpixels (pnm-width image) (pnm-height image))))
   (cached-superpixel-adjacency video-name frame prefix superpixel-type))
  (write-binary-superpixels
   (map (lambda (s)
	 (make-superpixel
	  (if level?
	      (y (superpixel-name s))
	      (superpixel-name s))
	  (if level?
	      (map y (superpixel-next s))
	      (superpixel-next s))
	  (if level?
	      (y (superpixel-parent s))
	      (superpixel-parent s))
	  (if level?
	      (map y (superpixel-children s))
	      (superpixel-children s))
	  (superpixel-velocity s)
	  (if region?
	      (superpixel-pixels s)
	      (superpixel->outline s))))
	superpixels)
   (cached-superpixels-pathname video-name frame prefix superpixel-type))))
