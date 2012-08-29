(MODULE
  SMOOTH-BOXES
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
(include "smooth-boxes.sch")

(set! *program* "smooth-boxes")
(set! *panic?* #f)

(define (box->vector box)
 (vector (voc4-detection-x1 box)
	 (voc4-detection-y1 box)
	 (voc4-detection-x2 box)
	 (voc4-detection-y2 box)))

(define (smooth-box-movie box-movie)
 (let* ((l (length box-movie))
	(splits (max (exact-round (* (/ l 140) 10)) 1))
	(splits2 (max (exact-round (* (/ l 140) 5)) 1))
	(ncx splits)
	(ncy splits)
	(nw splits2)
	(nh splits2))
  (scheme->matlab! "boxes" (list->vector (map box->vector box-movie)))
  (scheme->matlab!
   "skip"
   (list->vector
    (map (lambda (box)
	  (if (and (nondropped-box? box) (voc4-detection-model box)) 0 1))
	 box-movie)))
  (matlab-eval-strings
   "addpath('~/darpa-collaboration/splines');"
   "box_centre = @(x) [(x(1,1)+x(1,3))/2 (x(1,2)+x(1,4))/2];"
   "centres = cell2mat(cellfun(box_centre,num2cell(boxes,2),'UniformOutput',false));"
   (format #f "ts = [1:~s];" (length box-movie))
   "new_ts = ts(skip()==0);"
   "new_centres = double(centres(skip()==0,:));"
   "widths = abs(boxes(:,1)-boxes(:,3));"
   "heights = abs(boxes(:,2)-boxes(:,4));"
   "new_widths = double(widths(skip()==0));"
   "new_heights = double(heights(skip()==0));"
   (format #f "ppx = splinefit(new_ts,new_centres(:,1),~s,4);" ncx)
   (format #f "ppy = splinefit(new_ts,new_centres(:,2),~s,4);" ncy)
   "smx = ppval(ppx,ts);"
   "smy = ppval(ppy,ts);"
   (format #f "ppw = splinefit(new_ts,new_widths(:),~s,4);" nw)
   (format #f "pph = splinefit(new_ts,new_heights(:),~s,4);" nh)
   "smw = ppval(ppw,ts);"
   "smh = ppval(pph,ts);")
  (map (lambda (x y w h box)
	(if (nondropped-box? box)
	    (make-voc4-detection (- x (/ w 2))
				 (- y (/ h 2))
				 (+ x (/ w 2))
				 (+ y (/ h 2))
				 (voc4-detection-filter box)
				 (voc4-detection-strength box)
				 (voc4-detection-delta box)
				 (voc4-detection-model box))
	    box))
       (vector->list (x (matlab-get-variable "smx")))
       (vector->list (x (matlab-get-variable "smy")))
       (vector->list (x (matlab-get-variable "smw")))
       (vector->list (x (matlab-get-variable "smh")))
       box-movie)))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0))
		    ("darpa" darpa? (name "name" string-argument ""))
		    ("stand-alone" stand-alone? (path "path" string-argument "")))
       (any-number ("box" box?
		    (box-type "type" string-argument)
		    (box-label "label" string-argument)
		    (box-number "number" integer-argument)
		    (box-extension "ext" string-argument)
		    (bloat "bloat" real-argument))))
 (start-matlab!)
 (let* ((video-name (cond (standard?
			   (standard-corpus-video corpus sequence person location n))
			  (darpa? (string->darpa-video name))
			  (stand-alone? (make-stand-alone-video path))
			  (else (fuck-up))))
	(available-tracked-boxes (video-voc4-tracked-boxes-available video-name)))
  (for-each
   (lambda (tracked-box-name)
    (let ((model (second tracked-box-name))
	  (number (third tracked-box-name)))
     (for-each-indexed
      (lambda (box frame)
       (write-voc4-track-box
	box
	(smooth-tracked-box-pathname video-name (+ frame 1) model number)))
      (smooth-box-movie (map first (read-tracked-boxes video-name model number))))))
   available-tracked-boxes)))
