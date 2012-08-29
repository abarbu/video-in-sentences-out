(MODULE
  BERKELEY
  (WITH QOBISCHEME XLIB IDEALIB-PREGEXP IDEALIB-STUFF)
  (MAIN MAIN))
;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2010 Purdue University. All rights reserved.

(include "QobiScheme.sch")
(include "berkeley.sch")

(set! *program* "berkeley")
(set! *panic?* #t)

;;; Macros

;;; Structures

;;; Variables

;;; Parameters

;;; C Externals

;;; Procedures

;;; Commands

;;; Top Level

(define-command (main (exactly-one ("standard" standard?
				    (corpus "corpus" string-argument "")
				    (sequence "sequence" string-argument "")
				    (person "person" string-argument "")
				    (location "location" string-argument "")
				    (n "n" integer-argument 0))
				   ("darpa" darpa? (name "name" string-argument ""))))
 (let* ((video-name
	 (cond (standard?
		(standard-corpus-video corpus sequence person location n))
	       (darpa? (string->darpa-video name))
	       (else (fuck-up)))))
  (map-m-n
   (lambda (frame)
    (format #t "Frame: ~a~%" frame)
    (system (format #f "cd ~~/darpa-collaboration/berkeley/; matlab -nodesktop -nosplash -r \"; run('~a','~a')\" < /dev/null" ;; > /dev/null"
		    (ppm-pathname video-name frame)
		    (berkeley-pathname video-name frame))))
   0
   (video-length video-name))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
