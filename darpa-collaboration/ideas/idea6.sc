(MODULE IDEA6 (WITH QOBISCHEME XLIB IDEALIB-PREGEXP IDEALIB-STUFF) (MAIN MAIN))
;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme-AD.sch")
(include "idea6.sch")

(set! *program* "idea6")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level



(define-command (main (required (sequence "sequence" string-argument))
		      (required (person "person" string-argument))
		      (required (location "location" string-argument))
		      (required (n "n" integer-argument))
		      (required (delta "delta" integer-argument))
		      (required (output "output" string-argument))
		      (optional (max-frames "max-frames" integer-argument infinity))
		      (optional (pathname "pathname" string-argument *video-pathname*)))
 (set! *video-pathname* pathname)
 (display (video-length sequence person location n))(newline)
 (display (string-append *video-pathname*
			 sequence
			 "-"
			 person
			 "-"
			 location
			 "-"
			 (number->string n)
			 "/"
			 (number->padded-string-of-length 0 4)
			 "/frame.ppm"))(newline)
 (pbms->avi-file
  (map-m-n
   (lambda (frame)
    (write frame) (newline)
    (let* ((temp-prefix (format #f "/tmp/rc-~a" (getpid)))
	   (prefix (generic-pathname sequence person location n frame "rc"))
	   (pgm (ppm->pgm (read-pnm (berkeley-pathname sequence person location n frame))))
	   (rc-in (pgm->rc-graph pgm delta))
	   (rc-chains (make-rc-chains (pnm-height pgm) (pnm-width pgm)
				     (first rc-in) (second rc-in) (third rc-in))))
     (if (> (length (rc-chains-dashed-edges rc-chains)) 75000)
	 (pbm-constant (pnm-width pgm) (pnm-height pgm) #f)
	 (begin
	  (write-rc-output (string-append temp-prefix ".graph")
			   (rc-chains-vertices rc-chains)
			   (rc-chains-solid-edges rc-chains)
			   (rc-chains-dashed-edges rc-chains))
	  (write-solid-lines (string-append temp-prefix ".lines")
			     (rc-chains-solid-edges rc-chains))
	  (write-object-to-file rc-chains (string-append temp-prefix ".chains"))
	  (write-w-file (convert-graph (rc-chains->graph rc-chains))
			(string-append temp-prefix ".w"))
	  (system (format #f "~~/darpa-collaboration/rc/RatioContour3 ~a" temp-prefix))
	  (let* ((cycle (read-cycle-chains rc-chains temp-prefix 0))
		 (image-cycle-solid (rc-cycle-solid->ppm cycle)))
	   ;; TODO Andrei Fix up idealib functions
	   ;; (write-pnm (rc-cycle-solid-dashed->ppm cycle) (string-append output "-cycle.ppm"))
	   ;; (write-pnm image-cycle-solid (string-append output "-cycle-solid.ppm"))
	   (rc-draw-cycle-edges prefix cycle)
	   (rc-draw-cycle-chains prefix cycle)
	   (rc-draw-cycle prefix cycle)
	   (pgm->pbm (make-pgm #f 255 (ppm-red image-cycle-solid)) 1))))))
   1
   (min max-frames (- (video-length sequence person location n) 1)))
  15					;hardwired
  (tmp-avi-pathname sequence person location n)))

;;; Tam V'Nishlam Shevah L'El Borei Olam
