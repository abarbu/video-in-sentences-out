;;; LaHaShem HaAretz U'Mloah
;;; Copyright 2011 Purdue University. All rights reserved.
;;; scc -o analyze -O2 analyze.sc ~/lib/`architecture-path`/QobiScheme.a ~/lib/`architecture-path`/scxl.a -L /usr/X11R6/lib -lX11;rm analyze.c

(module main (with QobiScheme xlib) (main main17))

(include "QobiScheme.sch")

(set! *program* "analyze")
(set! *panic?* #t)

(define *verbs*
 '#("approach"
    "arrive"
    "attach"
    "bounce"
    "bury"
    "carry"
    "catch"
    "chase"
    "close"
    "collide"
    "dig"
    "drop"
    "enter"
    "exchange"
    "exit"
    "fall"
    "flee"
    "fly"
    "follow"
    "get"
    "give"
    "go"
    "hand"
    "haul"
    "have"
    "hit"
    "hold"
    "jump"
    "kick"
    "leave"
    "lift"
    "move"
    "open"
    "pass"
    "pick up"
    "push"
    "put down"
    "raise"
    "receive"
    "replace"
    "run"
    "snatch"
    "stop"
    "take"
    "throw"
    "touch"
    "turn"
    "walk"))

(define *verb-list* (vector->list *verbs*))

(define (verb->verb-code verb) (+ (position verb *verb-list*) 1))

(define (verb-code->verb verb-code) (vector-ref *verbs* (- verb-code 1)))

(define (dos->unix strings)
 (map (lambda (string) (list->string (but-last (string->list string))))
      strings))

(define *official-directory*
 "/home/qobi/darpa-collaboration/documentation/official/")

(define (read-data parser pathname)
 (let ((lines
	(dos->unix (read-file (string-append *official-directory* pathname)))))
  (map parser
       (sublist lines
		(+ (position-if (lambda (line) (substring? "[DATA]" line))
				lines)
		   1)
		(length lines)))))

(define (read-unix-data parser pathname)
 (let ((lines (read-file (string-append *official-directory* pathname))))
  (map parser
       (sublist lines
		(+ (position-if (lambda (line) (substring? "[DATA]" line))
				lines)
		   1)
		(length lines)))))

(define (swap-commas-and-spaces string)
 (list->string (map (lambda (char)
		     (cond ((char=? char #\,) #\space)
			   ((char=? char #\space) #\,)
			   (else char)))
		    (string->list string))))

(define (parse-VS string)
 (let ((string (swap-commas-and-spaces string)))
  ;; ignore Frame_Rate Vignette_Local_ID Vignette_Creator
  (list (field-ref string 0) (strip-extension (field-ref string 1)))))

(define (read-VS pathname) (read-data parse-VS pathname))

(define (read-unix-VS pathname) (read-unix-data parse-VS pathname))

(define (parse-SD string)
 (let ((string (swap-commas-and-spaces string)))
  ;; ignore Vignette_Local_ID
  (list (field-ref string 0) (field-ref string 1))))

(define (read-SD pathname) (read-data parse-SD pathname))

(define (parse-HR-REC string)
 (let ((string (swap-commas-and-spaces string)))
  (list
   ;; vignette
   (field-ref string 0)
   ;; subject
   ;; for ARL is a natural number but for Turk is a string
   (field-ref string 1)
   ;; verb
   (verb-code->verb (string->number (field-ref string 2)))
   ;; judgment
   (case (string->number (field-ref string 3))
    ((0) #f)
    ((1) #t)
    (else (fuck-up))))))

(define (read-HR-REC pathname) (read-data parse-HR-REC pathname))

(define (compose-SD-VS sd vs)
 (map (lambda (sd) (list (first sd) (second (assoc (second sd) vs)))) sd))

(define (compose-HR-SD-VS hr sd vs)
 (let ((sd (compose-SD-VS sd vs)))
  (map (lambda (hr)
	(list (second (assoc (first hr) sd))
	      (second hr)
	      (third hr)
	      (fourth hr)))
       hr)))

(define (parse-SR-REC string) (fields (swap-commas-and-spaces string)))

(define (read-SR-REC pathname) (read-unix-data parse-SR-REC pathname))

(define (compose-SR-SD-VS sr sd vs)
 (let ((sd (compose-SD-VS sd vs)))
  (map (lambda (sr) (cons (second (assoc (first sr) sd)) (rest sr))) sr)))

(define (panel-r31mar2011)
 (compose-HR-SD-VS
  (read-HR-REC "HR_ARL_D_REC_PANEL_20110328_ARL.txt")
  (read-SD "SD_ARL_D_REC_PANEL_20110328_1_SD-FOR-ARL-REC-PANEL.txt")
  (read-VS "VS_ARL_20110328_1_VS-FOR-ARL-REC.txt")))

(define (roundtable-r31mar2011)
 (compose-HR-SD-VS
  (read-HR-REC "HR_ARL_D_REC_ROUNDTABLE_20110331_ARL.txt")
  (read-SD "SD_ARL_D_REC_ROUNDTABLE_20110331_1_SD-FOR-ARL-REC-ROUNDTABLE.txt")
  (read-VS "VS_ARL_20110328_1_VS-FOR-ARL-REC.txt")))

(define (panel-f15apr2011)
 (compose-HR-SD-VS
  (read-HR-REC "HR_ARL_D_REC_PANEL_20110415_ARL.csv")
  (read-SD "SD_ARL_D_REC_PANEL_20110415_1_2_SD-FOR-ARL-REC-DEV.csv")
  (read-VS "VS_ARL_20110415_1_2_VS-FOR-ARL-REC-DEV.csv")))

(define (roundtable-f15apr2011)
 (compose-HR-SD-VS
  (read-HR-REC "HR_ARL_D_REC_ROUNDTABLE_20110415_ARL.csv")
  (read-SD "SD_ARL_D_REC_ROUNDTABLE_20110415_1_2_SD-FOR-ARL-RTREC-DEV.csv")
  (read-VS "VS_ARL_20110415_1_2_VS-FOR-ARL-REC-DEV.csv")))

(define (AMZ-f6may2011)
 (compose-HR-SD-VS
  (read-HR-REC "HR_ARL_D_REC_SVPA_20110504_AMZ_Partial.csv")
  (read-SD "SD_ARL_D_REC_SVPA_1_1_SD-FOR-AMZ-REC-DEV.csv")
  (read-VS "VS_ARL_20110504_1_1_VS-FOR-AMZ-REC-DEV.csv")))

(define (AMZ-C-D1-recognition-r19may2011)
 (compose-HR-SD-VS
  (read-HR-REC "HR_ARL_D_REC_SVPA_20110519_AMZ_1_1_HR-FOR-AMZ-REC-DEV.csv")
  (read-SD "SD_ARL_D_REC_SVPA_20110519_1_1_SD-FOR-AMZ-REC-DEV.csv")
  (read-VS "VS_ARL_20110519_1_1_VS-FOR-AMZ-REC-DEV.csv")))

(define (AMZ-C-D1-recognition-m15aug2011)
 (compose-HR-SD-VS
  (read-HR-REC "HR_ARL_D_REC_SVPA_20110630_AMZ_1_1_HR-FOR-AMZ-REC-DEV.CSV")
  (read-SD "SD_ARL_D_REC_SVPA_20110630_1_0_SD-FOR-AMZ-REC-DEV.CSV")
  (read-VS "VS_ARL_20110630_1_0_VS-FOR-AMZ-REC-DEV.CSV")))

(define (AMZ-C-E1-recognition)
 (compose-HR-SD-VS
  (read-HR-REC "HR_ARL_9_REC_SVPA_20110629_ARL_1_1_HR-AMZ-REC-Y1-TEST-90.CSV")
  (read-SD "SD_ARL_T_REC_SVPA_20110629_1_0_SD-AMZ-REC-Y1-TEST.CSV")
  (read-VS "VS_ARL_20110629_1_0_VS-AMZ-REC-Y1-TEST.CSV")))

(define (AMZ-C-E1-recognition-m15aug2011)
 (compose-HR-SD-VS
  (read-HR-REC "HR_ARL_9_REC_SVPA_20110629_ARL_1_1_HR-AMZ-REC-Y1-TEST-90.CSV")
  (read-SD "SD_ARL_9_REC_SVPA_20110629_1_1_SD-AMZ-REC-Y1-TEST-90.CSV")
  (read-VS "VS_ARL_20110629_1_0_VS-AMZ-REC-Y1-TEST-90.CSV")))

(define (C-E1-recognition)
 (compose-SD-VS
  (read-SD "SD_ARL_T_REC_SVPA_20110629_1_0_SD-AMZ-REC-Y1-TEST.CSV")
  (read-VS "VS_ARL_20110629_1_0_VS-AMZ-REC-Y1-TEST.CSV")))

(define (C-E1-description)
 (compose-SD-VS
  (read-SD "SD_ARL_T_DES_240HP_20110629_1_0_SD-AMZ-DES-Y1-TEST.CSV")
  (read-VS "VS_ARL_20110629_1_0_VS-AMZ-DES-Y1-TEST.CSV")))

(define (run016)
 (compose-SR-SD-VS
  (read-SR-REC "SR_PDU_001_E_REC_SVPA_20110826_016.CSV")
  (read-SD "SD_ARL_T_REC_SVPA_20110629_1_0_SD-AMZ-REC-Y1-TEST.CSV")
  (read-VS "VS_ARL_20110629_1_0_VS-AMZ-REC-Y1-TEST.CSV")))

(define (test1) (subset? (panel-f15apr2011) (panel-r31mar2011)))

(define (test2)
 ;; 6024
 (length (set-difference (roundtable-f15apr2011) (roundtable-r31mar2011))))

(define (have hr)
 (let ((vignettes
	(union
	 (union
	  (read-file "/home/qobi/darpa-collaboration/documentation/C-D1a.text")
	  (read-file "/home/qobi/darpa-collaboration/documentation/C-D1b.text"))
	 (read-file
	  "/home/qobi/darpa-collaboration/documentation/animated.text"))))
  (remove-if-not (lambda (hr) (member (first hr) vignettes)) hr)))

(define (counts hr)
 (map (lambda (verb)
       (let ((hr (remove-if-not (lambda (hr) (string=? (third hr) verb)) hr)))
	(list verb
	      (map (lambda (vignette)
		    (let ((hr
			   (remove-if-not
			    (lambda (hr) (string=? (first hr) vignette)) hr)))
		     (list vignette
			   (count-if fourth hr)
			   (count-if-not fourth hr))))
		   (remove-duplicates (map first hr))))))
      *verb-list*))

(define (swap-underscores-and-spaces string)
 (list->string (map (lambda (char)
		     (cond ((char=? char #\_) #\space)
			   ((char=? char #\space) #\_)
			   (else char)))
		    (string->list string))))

(define (exemplar vignette)
 (string-downcase (field-ref (swap-underscores-and-spaces vignette) 0)))

(define (park? vignette)
 (or (substring? "PARK" vignette) (substring? "Park" vignette)))

(define (urban? vignette)
 (or (substring? "URBAN" vignette)
     (substring? "Urban" vignette)
     (substring? "DOWNTOWN" vignette)
     ;; Not sure what this is.
     (substring? "STUDIO" vignette)))

(define (central? vignette) (substring? "_MC_" vignette))

(define (peripheral? vignette)
 (or (substring? "_BL_" vignette)
     (substring? "_BC_" vignette)
     (substring? "_BR_" vignette)
     (substring? "_ML_" vignette)
     (substring? "_MR_" vignette)
     (substring? "_FL_" vignette)
     (substring? "_FC_" vignette)
     (substring? "_FR_" vignette)))

(define (low? vignette)
 (or (substring? "_MORN_DARK_" vignette)
     (substring? "_MIDD_DARK_" vignette)
     (substring? "_AFTN_DARK_" vignette)
     ;; This is presumably evening but not sure that it is low contrast.
     (substring? "_EVEN_" vignette)))

(define (high? vignette)
 (or (and (substring? "_MORN_" vignette)
	  (not (substring? "_MORN_DARK_" vignette)))
     (and (substring? "_MIDD_" vignette)
	  (not (substring? "_MIDD_DARK_" vignette)))
     (and (substring? "_AFTN_" vignette)
	  (not (substring? "_AFTN_DARK_" vignette)))
     ;; Not sure what this is but presume that it is misspelling of MORN.
     (substring? "_NORM_" vignette)))

(define (boolean-variant vignette)
 (unless (and (not (eq? (park? vignette) (urban? vignette)))
	      (not (eq? (central? vignette) (peripheral? vignette)))
	      (not (eq? (low? vignette) (high? vignette))))
  (fuck-up))
 (list (park? vignette) (central? vignette) (low? vignette)))

(define (variant vignette)
 (unless (and (not (eq? (park? vignette) (urban? vignette)))
	      (not (eq? (central? vignette) (peripheral? vignette)))
	      (not (eq? (low? vignette) (high? vignette))))
  (fuck-up))
 (list (if (park? vignette) 'park 'urban)
       (if (central? vignette) 'central 'peripheral)
       (if (low? vignette) 'low 'high)))

(define (exemplify hr)
 (map (lambda (hr)
       (list (exemplar (first hr))
	     (second hr)
	     (third hr)
	     (fourth hr)))
      hr))

(define (exemplar-counts hr)
 (map (lambda (verb)
       (let ((hr (remove-if-not (lambda (hr) (string=? (third hr) verb)) hr)))
	(list verb
	      (map (lambda (vignette)
		    (let ((hr (remove-if-not
			       (lambda (hr)
				(string=? (exemplar (first hr))
					  (exemplar vignette)))
			       hr)))
		     (list vignette
			   (count-if fourth hr)
			   (count-if-not fourth hr))))
		   (remove-duplicates (map first hr))))))
      *verb-list*))

(define (exactly-n-exemplars-vignette-count hr n)
 (let* ((vignettes (remove-duplicates (map first hr)))
	(exemplars (remove-duplicates (map exemplar vignettes))))
  (* n
     (count-if (lambda (an-exemplar)
		(= (count-if (lambda (vignette)
			      (string=? an-exemplar (exemplar vignette)))
			     vignettes)
		   n))
	       exemplars))))

(define (exactly-n-exemplars hr n)
 (let* ((vignettes (remove-duplicates (map first hr)))
	(exemplars (remove-duplicates (map exemplar vignettes)))
	(exemplars-with-n-variants
	 (remove-if-not
	  (lambda (an-exemplar)
	   (= (count-if (lambda (vignette)
			 (string=? an-exemplar (exemplar vignette)))
			vignettes)
	      n))
	  exemplars)))
  (remove-if-not
   (lambda (hr)
    (some (lambda (an-exemplar) (string=? (exemplar (first hr)) an-exemplar))
	  exemplars-with-n-variants))
   hr)))

;;; (#f #t #f) urban center high
;;; OPEN9_A1_C2_Act8_URBAN_MC_AFTN_1a445f14-1e47-11e0-9fc9-e80688ca39a2
;;; OPEN9_A2_C1_Act2_URBAN_MC_AFTN_c1ce4290-3922-11e0-9343-1c6f65939a30
;;; OPEN9_A2_C1_Act6_STUDIO_MC_NORM_cc7fb962-3922-11e0-9343-1c6f65939a30

(define (check-exemplar-variants hr)
 (let ((vignettes (remove-duplicates (map first hr))))
  (for-each
   (lambda (an-exemplar)
    (let ((variants
	   (map variant
		(remove-if-not
		 (lambda (vignette) (string=? (exemplar vignette) an-exemplar))
		 vignettes))))
     ;; Since we don't know how to determine angle, we allow two variants per
     ;; exemplar.
     (when (some (lambda (variant) (> (count variant variants) 2))
		 (remove-duplicates variants))
      (display an-exemplar)
      (newline)
      (write (remove-if-not
	      ;; ditto
	      (lambda (variant-count) (> (second variant-count) 2))
	      (map (lambda (variant) (list variant (count variant variants)))
		   (remove-duplicates variants))))
      (newline)
      (newline))))
   (remove-duplicates (map exemplar vignettes)))))

(define (unanimous counts)
 (map (lambda (counts)
       (list (first counts)
	     (count-if
	      (lambda (vignette-counts)
	       (and (> (second vignette-counts) (third vignette-counts))
		    (zero? (third vignette-counts))))
	      (second counts))))
      counts))

(define (near-unanimous counts)
 (map (lambda (counts)
       (list (first counts)
	     (count-if
	      (lambda (vignette-counts)
	       (and (> (second vignette-counts) (third vignette-counts))
		    (<= (third vignette-counts) 1)))
	      (second counts))))
      counts))

(define (two-thirds counts)
 (map (lambda (counts)
       (list (first counts)
	     (count-if
	      (lambda (vignette-counts)
	       (>= (/ (second vignette-counts)
		      (+ (second vignette-counts) (third vignette-counts)))
		   (/ 2 3)))
	      (second counts))))
      counts))

(define (plurality counts)
 (map (lambda (counts)
       (list (first counts)
	     (count-if (lambda (vignette-counts)
			(> (second vignette-counts) (third vignette-counts)))
		       (second counts))))
      counts))

(define (one-third counts)
 (map (lambda (counts)
       (list (first counts)
	     (count-if
	      (lambda (vignette-counts)
	       (>= (/ (second vignette-counts)
		      (+ (second vignette-counts) (third vignette-counts)))
		   (/ 1 3)))
	      (second counts))))
      counts))

(define (anybody counts)
 (map (lambda (counts)
       (list (first counts)
	     (count-if (lambda (vignette-counts)
			(positive? (second vignette-counts)))
		       (second counts))))
      counts))

(define (lack-of-consensus counts)
 (map (lambda (counts)
       (list (first counts)
	     (count-if (lambda (vignette-counts)
			(<= (abs (- (second vignette-counts)
				    (third vignette-counts)))
			    1))
		       (second counts))))
      counts))

(define (quadruple counts)
 (map (lambda (counts)
       (list (first counts)
	     (count-if (lambda (vignette-counts)
			(> (second vignette-counts) (third vignette-counts)))
		       (second counts))
	     (count-if
	      (lambda (vignette-counts)
	       (>= (/ (second vignette-counts)
		      (+ (second vignette-counts) (third vignette-counts)))
		   (/ 2 3)))
	      (second counts))
	     (count-if
	      (lambda (vignette-counts)
	       (and (> (second vignette-counts) (third vignette-counts))
		    (<= (third vignette-counts) 1)))
	      (second counts))
	     (count-if
	      (lambda (vignette-counts)
	       (and (> (second vignette-counts) (third vignette-counts))
		    (zero? (third vignette-counts))))
	      (second counts))))
      counts))

;; (unanimous-select (exemplar-counts (exactly-n-exemplars (AMZ-C-D1-recognition-r19may2011) 16)))

(define (unanimous-select counts)
 (map (lambda (counts)
       (list (first counts)
	     (map first
		  (remove-if-not
		   (lambda (vignette-counts)
		    (and (> (second vignette-counts) (third vignette-counts))
			 (zero? (third vignette-counts))))
		   (second counts)))))
      counts))

(define (near-unanimous-select counts)
 (map (lambda (counts)
       (list (first counts)
	     (map first
		  (remove-if-not
		   (lambda (vignette-counts)
		    (and (> (second vignette-counts) (third vignette-counts))
			 (<= (third vignette-counts) 1)))
		   (second counts)))))
      counts))

(define (two-thirds-select counts)
 (map (lambda (counts)
       (list (first counts)
	     (map first
		  (remove-if-not
		   (lambda (vignette-counts)
		    (>= (/ (second vignette-counts)
			   (+ (second vignette-counts) (third vignette-counts)))
			(/ 2 3)))
		   (second counts)))))
      counts))

(define (plurality-select counts)
 (map (lambda (counts)
       (list (first counts)
	     (map first
		  (remove-if-not
		   (lambda (vignette-counts)
		    (> (second vignette-counts) (third vignette-counts)))
		   (second counts)))))
      counts))

(define (one-third-select counts)
 (map (lambda (counts)
       (list (first counts)
	     (map first
		  (remove-if-not
		   (lambda (vignette-counts)
		    (>= (/ (second vignette-counts)
			   (+ (second vignette-counts) (third vignette-counts)))
			(/ 1 3)))
		   (second counts)))))
      counts))

(define (anybody-select counts)
 (map (lambda (counts)
       (list (first counts)
	     (map first
		  (remove-if-not (lambda (vignette-counts)
				  (positive? (second vignette-counts)))
				 (second counts)))))
      counts))

(define (lack-of-consensus-select counts)
 (map (lambda (counts)
       (list (first counts)
	     (map first
		  (remove-if-not (lambda (vignette-counts)
				  (<= (abs (- (second vignette-counts)
					      (third vignette-counts)))
				      1))
				 (second counts)))))
      counts))

(define (conflicts hr1 hr2)
 ;;> (map length (conflicts (panel-r31mar2011) (roundtable-r31mar2011)))
 ;;(2 0 5 0 0 0 4 0 2 0 9 0 0 0 0 0)
 ;;> (map length (conflicts (panel-f15apr2011) (roundtable-f15apr2011)))
 ;;(2 0 5 0 0 0 4 0 3 0 8 0 0 0 0 0)
 ;;> (map length (conflicts (roundtable-r31mar2011) (roundtable-f15apr2011)))
 ;;(10 0 7 0 0 0 0 0 0 0 7 0 0 0 0 0)
 (map-n (lambda (subject)
	 (let ((hr1 (remove-if-not
		     ;; This only works if subjects are natural numbers.
		     (lambda (hr) (= (second hr) (+ subject 1))) hr1))
	       (hr2 (remove-if-not
		     ;; This only works if subjects are natural numbers.
		     (lambda (hr) (= (second hr) (+ subject 1))) hr2)))
	  (remove-if-not
	   (lambda (hr1)
	    (some (lambda (hr2)
		   (and (string=? (first hr1) (first hr2))
			(string=? (third hr1) (third hr2))
			(not (eq? (fourth hr1) (fourth hr2)))))
		  hr2))
	   hr1)))
	16))

(define (generate-data)
 ;; Still need to reference
 ;; darpa-collaboration/documentation/C-D1-recognition-annotations.csv
 ;; to discard composite and animated videos.
 ;; Perhaps should discard multiple copies with intensity variation but don't
 ;; know how.
 ;;  (We don't know whether all such are DARK. Should ask.)
 ;; Perhaps should discard multiple camera shots of same event occurrence but
 ;; don't know how.
 (let ((counts
	(exemplar-counts (exactly-n-exemplars (AMZ-C-D1-recognition-r19may2011) 16))))
  (write-object-to-file
   (unanimous-select counts) "/tmp/C-D1-recognition-unanimous.sc")
  (write-object-to-file
   (near-unanimous-select counts) "/tmp/C-D1-recognition-near-unanimous.sc")
  (write-object-to-file
   (two-thirds-select counts) "/tmp/C-D1-recognition-two-thirds.sc")
  (write-object-to-file
   (plurality-select counts) "/tmp/C-D1-recognition-plurality.sc")))

(define (generate-data0)
 ;; Still need to reference
 ;; darpa-collaboration/documentation/C-D1-recognition-annotations.csv
 ;; to discard composite and animated videos.
 ;; Perhaps should discard multiple copies with intensity variation but don't
 ;; know how.
 ;;  (We don't know whether all such are DARK. Should ask.)
 ;; Perhaps should discard multiple camera shots of same event occurrence but
 ;; don't know how.
 (let ((counts
	(exemplar-counts (exactly-n-exemplars (AMZ-C-D1-recognition-r19may2011) 16))))
  (write-object-to-file
   (unanimous-select counts) "/tmp/C-D1-recognition-unanimous.sc")))

(define (generate-data1)
 ;; Still need to reference
 ;; darpa-collaboration/documentation/C-D1-recognition-annotations.csv
 ;; to discard composite and animated videos.
 ;; Perhaps should discard multiple copies with intensity variation but don't
 ;; know how.
 ;;  (We don't know whether all such are DARK. Should ask.)
 ;; Perhaps should discard multiple camera shots of same event occurrence but
 ;; don't know how.
 (let ((counts
	(exemplar-counts (exactly-n-exemplars (AMZ-C-D1-recognition-r19may2011) 16))))
  (write-object-to-file
   (near-unanimous-select counts) "/tmp/C-D1-recognition-near-unanimous.sc")))

(define (generate-data2)
 ;; Still need to reference
 ;; darpa-collaboration/documentation/C-D1-recognition-annotations.csv
 ;; to discard composite and animated videos.
 ;; Perhaps should discard multiple copies with intensity variation but don't
 ;; know how.
 ;;  (We don't know whether all such are DARK. Should ask.)
 ;; Perhaps should discard multiple camera shots of same event occurrence but
 ;; don't know how.
 (let ((counts
	(exemplar-counts (exactly-n-exemplars (AMZ-C-D1-recognition-r19may2011) 16))))
  (write-object-to-file
   (two-thirds-select counts) "/tmp/C-D1-recognition-two-thirds.sc")))

(define (generate-data3)
 ;; Still need to reference
 ;; darpa-collaboration/documentation/C-D1-recognition-annotations.csv
 ;; to discard composite and animated videos.
 ;; Perhaps should discard multiple copies with intensity variation but don't
 ;; know how.
 ;;  (We don't know whether all such are DARK. Should ask.)
 ;; Perhaps should discard multiple camera shots of same event occurrence but
 ;; don't know how.
 (let ((counts
	(exemplar-counts (exactly-n-exemplars (AMZ-C-D1-recognition-r19may2011) 16))))
  (write-object-to-file
   (plurality-select counts) "/tmp/C-D1-recognition-plurality.sc")))

(define (generate-saenko-data)
 (let ((counts (exemplar-counts (AMZ-C-E1-recognition))))
  (write-object-to-file
   (unanimous-select counts) "/tmp/C-E1-recognition-unanimous.sc")
  (write-object-to-file
   (near-unanimous-select counts) "/tmp/C-E1-recognition-near-unanimous.sc")
  (write-object-to-file
   (two-thirds-select counts) "/tmp/C-E1-recognition-two-thirds.sc")
  (write-object-to-file
   (plurality-select counts) "/tmp/C-E1-recognition-plurality.sc")))

(define (all-VS)
 (remove-duplicates
  (map
   second
   (append
    (read-VS "tmp/VS_ARL_20110504_1_1_VS-FOR-AMZ-REC-DEV.csv")
    (read-VS "tmp/EvaluationTestSet/AMZDescription/VS_ARL_20110629_1_0_VS-AMZ-DES-Y1-TEST.csv")
    (read-VS "tmp/EvaluationTestSet/ARLDescription/VS_ARL_20110629_1_0_VS-ARL-DES-Y1-TEST.csv")
    (read-VS "tmp/EvaluationTestSet/ARLGapFilling/VS_ARL_20110629_1_0_VS-ARL-GAP-Y1-TEST.csv")
    (read-VS "tmp/EvaluationTestSet/AMZRecognition/VS_ARL_20110629_1_0_VS-AMZ-REC-Y1-TEST.csv")
    (read-VS "tmp/EvaluationTestSet/GS_Anomaly/VS_ARL_20110628_1_0_VS-ARL-ANM-Y1-TEST.csv")
    (read-VS "tmp/EvaluationTestSet/AMZGapFilling/VS_ARL_20110629_1_0_VS-AMZ-GAP-Y1-TEST.csv")
    (read-unix-VS "tmp/VS_ARL_20110427_1_1_VS-FOR-ARL-DES-DEV.csv")
    (read-VS "tmp/VS_ARL_D_ANM_20110506_1_0_VS-ARL-ANM-DEV.csv")
    (read-VS "tmp/VS_ARL_20110512_1_0_VS-FOR-AMZ-GAP-DEV.csv")
    (read-VS "tmp/VS_ARL_20110427_1_1_VS-FOR-ARL-GF-DEV.csv")
    (read-VS "tmp/VS_ARL_20110427_1_1_VS-FOR-ARL-GAP-DEV.csv")
    (read-VS "tmp/VS_ARL_20110520_1_1_VS-FOR-AMZ-DES-DEV.csv")
    (read-VS "tmp/VS_ARL_20110519_1_1_VS-FOR-AMZ-REC-DEV.csv")
    (read-VS "tmp/VS_ARL_20110415_1_2_VS-FOR-ARL-REC-DEV.csv")
    (read-VS "tmp/VS_ARL_20110513_1_0_VS-FOR-AMZ-DES-DEV.csv")
    (read-VS "tmp/VS_ARL_20110415_1_0_VS-FOR-ARL-REC-DEV.csv")
    (read-VS "tmp/VS_ARL_20110328_1_VS-FOR-ARL-REC.csv")
    (read-VS "tmp/zip/VS_ARL_20110516_1_2_VS-FOR-AMZ-GAP-DEV7.csv")
    (read-VS "tmp/zip/VS_ARL_20110516_1_1_VS-FOR-AMZ-GAP-DEV.csv")
    (read-unix-VS "tmp/zip/VS_ARL_20110427_1_1_VS-FOR-ARL-DES-DEV5.csv")
    (read-VS "tmp/zip/VS_ARL_20110415_1_0_VS-FOR-ARL-REC-DEV8.csv")
    (read-unix-VS "tmp/zip/VS_ARL_20110427_1_1_VS-FOR-ARL-DES-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_D_ANM_20110506_1_0_VS-ARL-ANM-DEV1.csv")
    (read-VS "tmp/zip/VS_ARL_20110516_1_2_VS-FOR-AMZ-GAP-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_D_ANM_20110506_1_0_VS-ARL-ANM-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_20110415_1_1_VS-FOR-ARL-REC-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_20110415_1_0_VS-FOR-ARL-REC-DEV9.csv")
    (read-VS "tmp/zip/VS_ARL_20110427_1_1_VS-FOR-ARL-GAP-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_20110520_1_1_VS-FOR-AMZ-DES-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_20110526_1_1_VS-FOR-ARL-DES-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_20110519_1_1_VS-FOR-AMZ-REC-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_20110516_1_2_VS-FOR-AMZ-GAP-DEV3.csv")
    (read-VS "tmp/zip/VS_ARL_20110415_1_0_VS-FOR-ARL-REC-DEV1.csv")
    (read-VS "tmp/zip/VS_ARL_20110519_1_1_VS-FOR-AMZ-REC-DEV4.csv")
    (read-VS "tmp/zip/VS_ARL_20110415_1_0_VS-FOR-ARL-REC-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_20110520_1_1_VS-FOR-AMZ-DES-DEV12.csv")
    (read-VS "tmp/zip/VS_ARL_D_ANM_20110506_1_1_VS-ARL-ANM-DEV.csv")
    (read-VS "tmp/zip/VS_ARL_20110415_1_1_VS-FOR-ARL-REC-DEV11.csv")
    (read-VS "tmp/zip/VS_ARL_20110520_1_1_VS-FOR-AMZ-DES-DEV2.csv")
    (read-VS "tmp/zip/VS_ARL_20110519_1_1_VS-FOR-AMZ-REC-DEV10.csv")))))

;; ls /aux/qobi/video-datasets/*/*/*.{mov,avi,wmv} >/tmp/darpa-files.text
(define (missing-vignettes)
 (for-each
  (lambda (file) (display file) (newline))
  (set-difference
   (all-VS)
   (map strip-directory
	(map strip-extension (read-file "/tmp/darpa-files.text"))))))

(define (percent p) (inexact->exact (round (* 100 p))))

(define (raw-intercoder-agreement corpus)
 (let ((vignettes (remove-duplicates (map first corpus))))
  (map
   (lambda (verb)
    (let* ((corpus
	    (remove-if-not (lambda (sample) (string=? (third sample) verb))
			   corpus))
	   (counts (map (lambda (vignette)
			 (count-if (lambda (sample)
				    (and (string=? (first sample) vignette)
					 (fourth sample)))
				   corpus))
			vignettes)))
     (list verb
	   (/ (+ (countv 0 counts) (countv 5 counts)) (length counts))
	   (/ (+ (countv 1 counts) (countv 4 counts)) (length counts))
	   (/ (+ (countv 2 counts) (countv 3 counts)) (length counts)))))
   *verb-list*)))

(define (intercoder-agreement corpus)
 (let ((vignettes (remove-duplicates (map first corpus))))
  (map
   (lambda (verb)
    (let* ((corpus
	    (remove-if-not (lambda (sample) (string=? (third sample) verb))
			   corpus))
	   (counts (map (lambda (vignette)
			 (count-if (lambda (sample)
				    (and (string=? (first sample) vignette)
					 (fourth sample)))
				   corpus))
			vignettes)))
     (list
      verb
      (percent (/ (+ (countv 0 counts) (countv 5 counts)) (length counts)))
      (percent (/ (+ (countv 1 counts) (countv 4 counts)) (length counts)))
      (percent (/ (+ (countv 2 counts) (countv 3 counts)) (length counts))))))
   *verb-list*)))

(define (undarkify vignette)
 (let ((vignette (substring vignette 0 (- (string-length vignette) 37))))
  (if (string-ci=? (substring vignette
			      (- (string-length vignette) 5)
			      (string-length vignette))
		   "_DARK")
      (substring vignette 0 (- (string-length vignette) 5))
      vignette)))

(define (dark-variants? vignette1 vignette2)
 (string-ci=? (undarkify vignette1) (undarkify vignette2)))

(define (raw-dark-intercoder-agreement corpus)
 (let ((vignettes
	(map first
	     (remove-if-not
	      (lambda (class)
	       (unless (<= (length class) 2) (fuck-up))
	       (= (length class) 2))
	      (equivalence-classesp
	       dark-variants? (remove-duplicates (map first corpus)))))))
  (map (lambda (verb)
	(let* ((corpus
		(remove-if-not (lambda (sample) (string=? (third sample) verb))
			       corpus))
	       (counts
		(map (lambda (vignette)
		      (count-if (lambda (sample)
				 (and (dark-variants? (first sample) vignette)
				      (fourth sample)))
				corpus))
		     vignettes)))
	 (list verb
	       (/ (+ (countv 0 counts) (countv 2 counts)) (length counts))
	       (/ (countv 1 counts) (length counts)))))
       *verb-list*)))

(define (dark-intercoder-agreement corpus)
 (let ((vignettes
	(map first
	     (remove-if-not
	      (lambda (class)
	       (unless (<= (length class) 2) (fuck-up))
	       (= (length class) 2))
	      (equivalence-classesp
	       dark-variants? (remove-duplicates (map first corpus)))))))
  (map (lambda (verb)
	(let* ((corpus
		(remove-if-not (lambda (sample) (string=? (third sample) verb))
			       corpus))
	       (counts
		(map (lambda (vignette)
		      (count-if (lambda (sample)
				 (and (dark-variants? (first sample) vignette)
				      (fourth sample)))
				corpus))
		     vignettes)))
	 (list
	  verb
	  (percent (/ (+ (countv 0 counts) (countv 2 counts)) (length counts)))
	  (percent (/ (countv 1 counts) (length counts))))))
       *verb-list*)))

(define (map-percent accuracies)
 (map (lambda (accuracy) (list (first accuracy) (percent (second accuracy))))
      accuracies))

(define (baseline-judgement sample)
 (if (member (third sample) '("move" "touch")) #t #f))

(define (memorize-judgement indexed-corpus)
 (lambda (sample)
  (let ((judgements (assoc (first sample) indexed-corpus)))
   (if judgements
       (vector-ref (second judgements) (- (verb->verb-code (third sample)) 1))
       (baseline-judgement sample)))))

(define (memorize-undarkify-judgement indexed-corpus)
 (lambda (sample)
  (let ((judgements (assoc (first sample) indexed-corpus)))
   (if judgements
       (vector-ref (second judgements) (- (verb->verb-code (third sample)) 1))
       (let ((indexed-corpus
	      (remove-if-not
	       (lambda (entry) (dark-variants? (first sample) (first entry)))
	       indexed-corpus)))
	(if (null? indexed-corpus)
	    (baseline-judgement sample)
	    (if (> (length indexed-corpus) 1)
		(fuck-up)
		(vector-ref (second (first indexed-corpus))
			    (- (verb->verb-code (third sample)) 1)))))))))

(define (dtw-detector-judgement indexed-likelihoods1 thresholds1
				indexed-likelihoods2 thresholds2
				dtw-distances dtw-threshold memorized-corpus)
 (lambda (sample)
  (let ((verb-code (verb->verb-code (third sample)))
	(dtw-result (assoc (first sample) dtw-distances)))
   (if (and dtw-result (third dtw-result) dtw-threshold
	    (< (third dtw-result) dtw-threshold))
       ((memorize-judgement memorized-corpus) sample)
       (let* ((verb-code (verb->verb-code (third sample)))
	      (likelihood2
	       (likelihood indexed-likelihoods2 (first sample) verb-code))
	      (threshold2 (threshold thresholds2 verb-code)))
	(if (and likelihood2 threshold2)
	    (> likelihood2 threshold2)
	    (let ((likelihood1
		   (likelihood indexed-likelihoods1 (first sample) verb-code))
		  (threshold1 (threshold thresholds1 verb-code)))
	     (if (and likelihood1 threshold1)
		 (> likelihood1 threshold1)
		 (baseline-judgement sample)))))))))

;;; Assumes likelihoods is a list of lists of the form
;;; (vignette verb likelihood).

(define (index-likelihoods likelihoods)
 (let* ((vignettes (remove-duplicates (map first likelihoods)))
	(index (map (lambda (vignette)
		     (list vignette (make-vector (vector-length *verbs*) #f)))
		    vignettes)))
  (for-each (lambda (likelihood)
	     (vector-set! (second (assoc (first likelihood) index))
			  (- (verb->verb-code (second likelihood)) 1)
			  (third likelihood)))
	    likelihoods)
  index))

(define (index-corpus corpus)
 (let* ((vignettes (remove-duplicates (map first corpus)))
	(index (map (lambda (vignette)
		     (list vignette
			   (make-vector (vector-length *verbs*) #f)))
		    vignettes)))
  (for-each (lambda (sample)
	     (vector-set! (second (assoc (first sample) index))
			  (- (verb->verb-code (third sample)) 1)
			  (fourth sample)))
	    corpus)
  index))

(define (likelihood indexed-likelihoods vignette verb-code)
 (let ((likelihood (assoc vignette indexed-likelihoods)))
  (and likelihood (vector-ref (second likelihood) (- verb-code 1)))))

(define (threshold thresholds verb-code)
 (vector-ref thresholds (- verb-code 1)))

(define (detector-judgement
	 indexed-likelihoods1 thresholds1 indexed-likelihoods2 thresholds2)
 (lambda (sample)
  (let* ((verb-code (verb->verb-code (third sample)))
	 (likelihood2
	  (likelihood indexed-likelihoods2 (first sample) verb-code))
	 (threshold2 (threshold thresholds2 verb-code)))
   (if (and likelihood2 threshold2)
       (> likelihood2 threshold2)
       (let ((likelihood1
	      (likelihood indexed-likelihoods1 (first sample) verb-code))
	     (threshold1 (threshold thresholds1 verb-code)))
	(if (and likelihood1 threshold1)
	    (> likelihood1 threshold1)
	    (baseline-judgement sample)))))))

(define (verb-accuracy judgement corpus verb-code)
 (/ (count-if (lambda (sample)
	       (and (string=? (third sample) (verb-code->verb verb-code))
		    (eq? (fourth sample) (judgement sample))))
	      corpus)
    (count-if (lambda (sample)
	       (string=? (third sample) (verb-code->verb verb-code)))
	      corpus)))

(define (verb-mcc judgement corpus verb-code)
 (let ((tp (exact->inexact
	    (count-if
	     (lambda (sample)
	      (and (string=? (third sample) (verb-code->verb verb-code))
		   (eq? (fourth sample) #t)
		   (eq? (judgement sample) #t)))
	     corpus)))
       (fp (exact->inexact
	    (count-if
	     (lambda (sample)
	      (and (string=? (third sample) (verb-code->verb verb-code))
		   (eq? (fourth sample) #f)
		   (eq? (judgement sample) #t)))
	     corpus)))
       (tn (exact->inexact
	    (count-if
	     (lambda (sample)
	      (and (string=? (third sample) (verb-code->verb verb-code))
		   (eq? (fourth sample) #f)
		   (eq? (judgement sample) #f)))
	     corpus)))
       (fn (exact->inexact
	    (count-if
	     (lambda (sample)
	      (and (string=? (third sample) (verb-code->verb verb-code))
		   (eq? (fourth sample) #t)
		   (eq? (judgement sample) #f)))
	     corpus))))
  (write (list verb-code tp fp tn fn)) (newline)
  (/ (- (* tp tn) (* fp fn))
     (if (zero? (sqrt (* (+ tp fp) (+ tp fn) (+ tn fp) (+ tn fn))))
	 1
	 (sqrt (* (+ tp fp) (+ tp fn) (+ tn fp) (+ tn fn)))))))

(define (map-verb-codes f)
 (map-n (lambda (i) (f (+ i 1))) (vector-length *verbs*)))

(define (per-verb-accuracies judgement corpus)
 (map-verb-codes (lambda (verb-code)
		  (list (verb-code->verb verb-code)
			(verb-accuracy judgement corpus verb-code)))))

(define (per-verb-mcc judgement corpus)
 (map-verb-codes (lambda (verb-code)
		  (list (verb-code->verb verb-code)
			(verb-mcc judgement corpus verb-code)))))

(define (aggregate-accuracy judgement corpus)
 (/ (count-if (lambda (sample) (eq? (fourth sample) (judgement sample)))
	      corpus)
    (length corpus)))

(define (aggregate-mcc judgement corpus)
 (let ((tp (exact->inexact
	    (count-if (lambda (sample)
		       (and (eq? (fourth sample) #t)
			    (eq? (judgement sample) #t)))
		      corpus)))
       (fp (exact->inexact
	    (count-if (lambda (sample)
		       (and (eq? (fourth sample) #f)
			    (eq? (judgement sample) #t)))
		      corpus)))
       (tn (exact->inexact
	    (count-if (lambda (sample)
		       (and (eq? (fourth sample) #f)
			    (eq? (judgement sample) #f)))
		      corpus)))
       (fn (exact->inexact
	    (count-if (lambda (sample)
		       (and (eq? (fourth sample) #t)
			    (eq? (judgement sample) #f)))
		      corpus))))
  (write (list tp fp tn fn)) (newline)
  (/ (- (* tp tn) (* fp fn))
     (if (zero? (sqrt (* (+ tp fp) (+ tp fn) (+ tn fp) (+ tn fn))))
	 1
	 (sqrt (* (+ tp fp) (+ tp fn) (+ tn fp) (+ tn fn)))))))

(define (candidate-thresholds indexed-likelihoods verb-code)
 (let ((likelihoods (sort (remove-duplicatesv
			   (removeq
			    #f
			    (map (lambda (likelihood)
				  (vector-ref (second likelihood)
					      (- verb-code 1)))
				 indexed-likelihoods)))
			  <
			  identity)))
  (if (null? likelihoods)
      '()
      (cons minus-infinity
	    (append (map (lambda (likelihood1 likelihood2)
			  (/ (+ likelihood1 likelihood2) 2))
			 (but-last likelihoods)
			 (rest likelihoods))
		    (list infinity))))))

(define (argmax f l)
 (if (null? l)
     #f
     (let loop ((x (first l)) (l (rest l)) (c (f (first l))))
      (if (null? l)
	  x
	  (let ((c1 (f (first l))))
	   (if (>= c1 c) (loop (first l) (rest l) c1) (loop x (rest l) c)))))))

(define (train-threshold corpus indexed-likelihoods verb-code)
 ;; an optimization
 (let ((corpus (remove-if-not
		(lambda (sample)
		 (string=? (third sample) (verb-code->verb verb-code)))
		corpus)))
  (argmax
   (lambda (threshold)
    (verb-accuracy
     (lambda (sample)
      (let ((likelihood
	     (likelihood indexed-likelihoods (first sample) verb-code)))
       (if likelihood (> likelihood threshold) (baseline-judgement sample))))
     corpus
     verb-code))
   (candidate-thresholds indexed-likelihoods verb-code))))

(define (fast-train-threshold corpus indexed-likelihoods verb-code)
 (let* ((likelihoods (sort (remove-if
			    (lambda (likelihood) (not (second likelihood)))
			    (map (lambda (indexed-likelihood)
				  (list (first indexed-likelihood)
					(vector-ref (second indexed-likelihood)
						    (- verb-code 1))))
				 indexed-likelihoods))
			   <
			   second))
	(corpus (remove-if-not
		 (lambda (sample)
		  (string=? (third sample) (verb-code->verb verb-code)))
		 corpus))
	;; This assumes that there is an entry in the corpus for every
	;; likelihood.
	(corpus (map (lambda (likelihood)
		      (find-if (lambda (sample)
				(string=? (first sample) (first likelihood)))
			       corpus))
		     likelihoods)))
  (if (null? likelihoods)
      #f
      (let loop ((likelihoods (rest likelihoods))
		 (corpus corpus)
		 (previous-likelihood (second (first likelihoods)))
		 (current-accuracy (count-if fourth corpus))
		 (best-threshold minus-infinity)
		 (best-accuracy (count-if fourth corpus)))
       (if (null? likelihoods)
	   (let* ((this-threshold infinity)
		  (this-judgement (fourth (first corpus)))
		  (current-accuracy
		   (+ current-accuracy (if this-judgement -1 1))))
	    (if (>= current-accuracy best-accuracy)
		this-threshold
		best-threshold))
	   (let* ((this-likelihood (second (first likelihoods)))
		  (this-threshold (/ (+ previous-likelihood this-likelihood) 2))
		  (this-judgement (fourth (first corpus)))
		  (current-accuracy
		   (+ current-accuracy (if this-judgement -1 1))))
	    (loop (rest likelihoods)
		  (rest corpus)
		  this-likelihood
		  current-accuracy
		  (if (and (not (= previous-likelihood this-likelihood))
			   (>= current-accuracy best-accuracy))
		      this-threshold
		      best-threshold)
		  (if (and (not (= previous-likelihood this-likelihood))
			   (>= current-accuracy best-accuracy))
		      current-accuracy
		      best-accuracy))))))))

(define (train-thresholds corpus indexed-likelihoods)
 (list->vector
  (map-verb-codes
   (lambda (verb-code)
    (fast-train-threshold corpus indexed-likelihoods verb-code)))))

(define (threshold-per-verb-accuracies thresholds1
				       thresholds2
				       test-corpus
				       test-likelihoods1
				       test-likelihoods2)
 (let ((indexed-test-likelihoods1 (index-likelihoods test-likelihoods1))
       (indexed-test-likelihoods2 (index-likelihoods test-likelihoods2)))
  (per-verb-accuracies (detector-judgement indexed-test-likelihoods1
					   thresholds1
					   indexed-test-likelihoods2
					   thresholds2)
		       test-corpus)))

(define (threshold-aggregate-accuracy thresholds1
				      thresholds2
				      test-corpus
				      test-likelihoods1
				      test-likelihoods2)
 (let ((indexed-test-likelihoods1 (index-likelihoods test-likelihoods1))
       (indexed-test-likelihoods2 (index-likelihoods test-likelihoods2)))
  (aggregate-accuracy (detector-judgement indexed-test-likelihoods1
					  thresholds1
					  indexed-test-likelihoods2
					  thresholds2)
		      test-corpus)))

(define (trained-per-verb-accuracies training-corpus
				     training-likelihoods1
				     training-likelihoods2
				     test-corpus
				     test-likelihoods1
				     test-likelihoods2)
 (let* ((indexed-training-likelihoods1
	 (index-likelihoods training-likelihoods1))
	(indexed-training-likelihoods2
	 (index-likelihoods training-likelihoods2))
	(indexed-test-likelihoods1 (index-likelihoods test-likelihoods1))
	(indexed-test-likelihoods2 (index-likelihoods test-likelihoods2))
	(thresholds1
	 (train-thresholds training-corpus indexed-training-likelihoods1))
	(thresholds2
	 (train-thresholds training-corpus indexed-training-likelihoods2)))
  (per-verb-accuracies (detector-judgement indexed-test-likelihoods1
					   thresholds1
					   indexed-test-likelihoods2
					   thresholds2)
		       test-corpus)))

(define (trained-aggregate-accuracy training-corpus
				    training-likelihoods1
				    training-likelihoods2
				    test-corpus
				    test-likelihoods1
				    test-likelihoods2)
 (let* ((indexed-training-likelihoods1
	 (index-likelihoods training-likelihoods1))
	(indexed-training-likelihoods2
	 (index-likelihoods training-likelihoods2))
	(indexed-test-likelihoods1 (index-likelihoods test-likelihoods1))
	(indexed-test-likelihoods2 (index-likelihoods test-likelihoods2))
	(thresholds1
	 (train-thresholds training-corpus indexed-training-likelihoods1))
	(thresholds2
	 (train-thresholds training-corpus indexed-training-likelihoods2)))
  (aggregate-accuracy (detector-judgement indexed-test-likelihoods1
					  thresholds1
					  indexed-test-likelihoods2
					  thresholds2)
		      test-corpus)))

(define (dtw-threshold-per-verb-accuracies
	 thresholds1 thresholds2
	 test-corpus
	 test-likelihoods1 test-likelihoods2
	 dtw-distances dtw-threshold dtw-memorized-corpus)
 (let ((indexed-test-likelihoods1 (index-likelihoods test-likelihoods1))
       (indexed-test-likelihoods2 (index-likelihoods test-likelihoods2)))
  (per-verb-accuracies (dtw-detector-judgement indexed-test-likelihoods1 thresholds1
					       indexed-test-likelihoods2 thresholds2
					       dtw-distances dtw-threshold dtw-memorized-corpus)
		       test-corpus)))

(define (dtw-threshold-aggregate-accuracy
	 thresholds1 thresholds2
	 test-corpus
	 test-likelihoods1 test-likelihoods2
	 dtw-distances dtw-threshold dtw-memorized-corpus)
 (let ((indexed-test-likelihoods1 (index-likelihoods test-likelihoods1))
       (indexed-test-likelihoods2 (index-likelihoods test-likelihoods2)))
  (aggregate-accuracy (dtw-detector-judgement indexed-test-likelihoods1 thresholds1
					      indexed-test-likelihoods2 thresholds2
					      dtw-distances dtw-threshold dtw-memorized-corpus)
		      test-corpus)))

(define (human-judgement-vector indexed-corpus vignette)
 (assoc vignette indexed-corpus))

(define (system-response-vector indexed-test-likelihoods1
				thresholds1
				indexed-test-likelihoods2
				thresholds2
				vignette)
 (map (lambda (verb)
       ((detector-judgement indexed-test-likelihoods1
			    thresholds1
			    indexed-test-likelihoods2
			    thresholds2)
	(list vignette #f verb #f)))
      *verb-list*))

(define (system-likelihood-vector
	 indexed-likelihoods1 indexed-likelihoods2 vignette)
 (map-verb-codes
  (lambda (verb-code)
   (let ((likelihood2 (likelihood indexed-likelihoods2 vignette verb-code)))
    (if likelihood2
	likelihood2
	(let ((likelihood1
	       (likelihood indexed-likelihoods1 vignette verb-code)))
	 (if likelihood1 likelihood1 0)))))))

(define (booleans->csv booleans)
 (reduce (lambda (s1 s2) (string-append s1 "," s2))
	 (map (lambda (boolean) (if boolean "1" "0")) booleans)
	 ""))

(define (numbers->csv numbers)
 (reduce (lambda (s1 s2) (string-append s1 "," s2))
	 (map number->string numbers)
	 ""))

(define (classify indexed-test-likelihoods1 indexed-test-likelihoods2 vignette)
 (let ((likelihoods
	(system-likelihood-vector
	 indexed-test-likelihoods1 indexed-test-likelihoods2 vignette)))
  (verb-code->verb
   (+ (positionv (reduce min likelihoods infinity) likelihoods) 1))))

(define (REC-system-response run-index	     ;"001"
			     completion-date ;"20110826"
			     thresholds1
			     thresholds2
			     test-corpus
			     test-likelihoods1
			     test-likelihoods2
			     output-directory)
 (let* ((indexed-test-likelihoods1 (index-likelihoods test-likelihoods1))
	(indexed-test-likelihoods2 (index-likelihoods test-likelihoods2)))
  (write-file
   (append
    (list
     "[HEADER]"
     "System_Response_Spec_Version=001"
     "System_Response_Spec_Revision=005"
     ;;Response_Set_Global_ID
     ;;Response_Set_Local_ID
     "Stimulus_Def_Global_ID=7fc9152c-d7a1-4c4d-ac26-a71d675440e8"
     "Stimulus_Def_Local_ID=SD-AMZ-REC-Y1-TEST"
     "Developer_Organization=PDU"
     "Evaluator_Organization=ARL"
     "Stimulus_Def_Organization=ARL"
     "System_ID=001"
     "System_Version=001"
     "System_Revision=000"
     "CPU_Type=\"Opteron\""
     "CPU_Quantity=416"
     "CPU_Utilization=99"
     "Purpose=E"
     (string-append "Date=" completion-date)
     (string-append "Run_Index=" run-index)
     "Task=REC"
     "Protocol=SVPA"
     (string-append "Run_Start=" completion-date "165959")
     (string-append "Run_End=" completion-date "165959")
     "Presentation_Order=SEQUENTIAL"
     ;;Developer_Notes
     ;;Evaluator_Notes
     ;;Protocol_Notes
     "[FIELDS]"
     "Required_Fields=Stimulus_ID,Stimulus_Onset,Response_Completion,Detection"
     "Required_Types=CUID,DATE_TIME,DATE_TIME,PRESENT_ABSENT[48]"
     "Optional_Fields=Signal_Strength"
     "Optional_Types=NORMAL[48]"
     "Custom_Fields="
     "Custom_Types="
     "[DATA]")
    (map (lambda (sample)
	  (string-append
	   (first sample)
	   ","
	   (string-append completion-date "165959")
	   ","
	   (string-append completion-date "165959")
	   ","
	   (booleans->csv
	    (system-response-vector indexed-test-likelihoods1
				    thresholds1
				    indexed-test-likelihoods2
				    thresholds2
				    (second sample)))
	   ","
	   (numbers->csv
	    (system-likelihood-vector
	     indexed-test-likelihoods1 indexed-test-likelihoods2 (second sample)))))
	 test-corpus))
   (string-append
    output-directory "/SR_PDU_001_E_REC_SVPA_" completion-date "_" run-index ".CSV"))))

(define (REC-system-response-v2 run-index	;"001"
				completion-date ;"20110826"
				thresholds1
				thresholds2
				test-corpus
				test-likelihoods1
				test-likelihoods2
				output-directory)
 (let* ((indexed-test-likelihoods1 (index-likelihoods test-likelihoods1))
	(indexed-test-likelihoods2 (index-likelihoods test-likelihoods2)))
  (write-file
   (append
    '("Vignette_File,Stimulus_Onset,Response_Completion,detection_approach,detection_arrive,detection_attach,detection_bounce,detection_bury,detection_carry,detection_catch,detection_chase,detection_close,detection_collide,detection_dig,detection_drop,detection_enter,detection_exchange,detection_exit,detection_fall,detection_flee,detection_fly,detection_follow,detection_get,detection_give,detection_go,detection_hand,detection_haul,detection_have,detection_hit,detection_hold,detection_JUMP,detection_KICK,detection_leave,detection_lift,detection_move,detection_open,detection_pass,detection_pick_up,detection_push,detection_put_down,detection_raise,detection_receive,detection_replace,detection_run,detection_snatch,detection_stop,detection_take,detection_throw,detection_touch,detection_turn,detection_walk,detection_pull,opt_signal_strength_approach,opt_signal_strength_arrive,opt_signal_strength_attach,opt_signal_strength_bounce,opt_signal_strength_bury,opt_signal_strength_carry,opt_signal_strength_catch,opt_signal_strength_chase,opt_signal_strength_close,opt_signal_strength_collide,opt_signal_strength_dig,opt_signal_strength_drop,opt_signal_strength_enter,opt_signal_strength_exchange,opt_signal_strength_exit,opt_signal_strength_fall,opt_signal_strength_flee,opt_signal_strength_fly,opt_signal_strength_follow,opt_signal_strength_get,opt_signal_strength_give,opt_signal_strength_go,opt_signal_strength_hand,opt_signal_strength_haul,opt_signal_strength_have,opt_signal_strength_hit,opt_signal_strength_hold,opt_signal_strength_JUMP,opt_signal_strength_KICK,opt_signal_strength_leave,opt_signal_strength_lift,opt_signal_strength_move,opt_signal_strength_open,opt_signal_strength_pass,opt_signal_strength_pick_up,opt_signal_strength_push,opt_signal_strength_put_down,opt_signal_strength_raise,opt_signal_strength_receive,opt_signal_strength_replace,opt_signal_strength_run,opt_signal_strength_snatch,opt_signal_strength_stop,opt_signal_strength_take,opt_signal_strength_throw,opt_signal_strength_touch,opt_signal_strength_turn,opt_signal_strength_walk,opt_signal_strength_pull")
    (map (lambda (sample)
	  (string-append
	   (second sample)
	   ","
	   (string-append completion-date "165959")
	   ","
	   (string-append completion-date "165959")
	   ","
	   (booleans->csv
	    (system-response-vector indexed-test-likelihoods1
				    thresholds1
				    indexed-test-likelihoods2
				    thresholds2
				    (second sample)))
	   ",." ;; for pull
	   ","
	   (numbers->csv
	    (system-likelihood-vector
	     indexed-test-likelihoods1 indexed-test-likelihoods2 (second sample)))
	   ",." ;; for pull
	   ))
	 test-corpus))
   (string-append
    output-directory "/FLAT_PDU_001_E_REC_SVPA_" completion-date "_" run-index ".CSV"))))

(define (DES-system-response run-index	     ;"001"
			     completion-date ;"20110826"
			     test-corpus
			     sentences
			     output-directory)
 (write-file
  (append
   (list
    "[HEADER]"
    "System_Response_Spec_Version=001"
    "System_Response_Spec_Revision=005"
    ;;Response_Set_Global_ID
    ;;Response_Set_Local_ID
    "Stimulus_Def_Global_ID=1acb40bb-5dfd-4255-9fa1-e81022e4a3e4"
    "Stimulus_Def_Local_ID=SD-AMZ-DES-Y1-TEST"
    "Developer_Organization=PDU"
    "Evaluator_Organization=ARL"
    "Stimulus_Def_Organization=ARL"
    "System_ID=001"
    "System_Version=001"
    "System_Revision=000"
    "CPU_Type=\"Opteron\""
    "CPU_Quantity=416"
    "CPU_Utilization=99"
    "Purpose=E"
    (string-append "Date=" completion-date)
    (string-append "Run_Index=" run-index)
    "Task=DES"
    "Protocol=240HP"
    (string-append "Run_Start=" completion-date "165959")
    (string-append "Run_End=" completion-date "165959")
    "Presentation_Order=SEQUENTIAL"
    ;;Developer_Notes
    ;;Evaluator_Notes
    ;;Protocol_Notes
    "[FIELDS]"
    "Required_Fields=Stimulus_ID,Stimulus_Onset,Response_Completion,Description"
    "Required_Types=CUID,DATE_TIME,DATE_TIME,TEXT_MESSAGE"
    "Optional_Fields="
    "Optional_Types="
    "Custom_Fields="
    "Custom_Types="
    "[DATA]")
   (map (lambda (sample)
	 (string-append (first sample)
			","
			(string-append completion-date "165959")
			","
			(string-append completion-date "165959")
			","
			"\""
			(first (list-ref
				(second (assoc (second sample) sentences))
				(- (string->number run-index) 1)))
			"\""))
	test-corpus))
  (string-append
   output-directory "/SR_PDU_001_E_DES_240HP_" completion-date "_" run-index ".CSV")))

(define (DES-system-response-v2 run-index	;"001"
				completion-date ;"20110826"
				test-corpus
				sentences
				output-directory)
 (write-file
  (cons
   "Vignette_File,Stimulus_Onset,Response_Completion,Description,opt_Start_Frame,opt_End_Frame,opt_Start_Time,opt_End_Time"
   (map-concat
    (lambda (sample)
     (map
      (lambda (sentence)
       (string-append (second sample)
		      ","
		      (string-append completion-date "165959")
		      ","
		      (string-append completion-date "165959")
		      ","
		      "\""
		      (first sentence)
		      "\""
		      ",." 
		      ",."
		      ",."
		      ",."))
      (second (assoc (second sample) sentences))))
    test-corpus))
  (string-append
   output-directory "/FLAT_PDU_001_E_DES_240HP_" completion-date "_" run-index ".CSV")))

(define (flat-system-response sr)
 (write-file (map (lambda (sample)
		   (string-append
		    (first sample)
		    ","
		    (second sample)
		    ","
		    (third sample)
		    ","
		    (booleans->csv (map (lambda (judgment)
					 (cond ((string=? judgment "0") #f)
					       ((string=? judgment "1") #t)
					       (else (fuck-up))))
					(sublist sample 3 51)))
		    ","
		    "."
		    ","
		    (numbers->csv (map string->number (sublist sample 51 99)))
		    ","
		    "."))
		  sr)
	     "FLAT_PDU_001_E_REC_SVPA_20110826_001.CSV"))

(define (short-flat-system-response sr)
 (write-file (map (lambda (sample)
		   (string-append
		    (first sample)
		    ","
		    (second sample)
		    ","
		    (third sample)
		    ","
		    (booleans->csv (map (lambda (judgment)
					 (cond ((string=? judgment "0") #f)
					       ((string=? judgment "1") #t)
					       (else (fuck-up))))
					(sublist sample 3 51)))))
		  sr)
	     "FLAT_PDU_001_E_REC_SVPA_20110826_001.CSV"))

(define (hyphens->spaces string)
 (list->string (map (lambda (char) (if (char=? char #\-) #\space char))
		    (string->list string))))

(define (spaces->hyphens string)
 (list->string (map (lambda (char) (if (char=? char #\space) #\- char))
		    (string->list string))))

(define (amichaux-verb->verb verb)
 (let ((verb (string-downcase verb)))
  (cond ((string=? verb "pick") "pick up")
	((string=? verb "put") "put down")
	(else verb))))

(define (result->likelihood result)
 (case (vector-length result)
  ((6) (list (vector-ref result 1)
	     (amichaux-verb->verb (hyphens->spaces (vector-ref result 4)))
	     (vector-ref result 5)))
  ((7) (list (vector-ref result 1)
	     (amichaux-verb->verb (hyphens->spaces (vector-ref result 4)))
	     (vector-ref result 6)))
  (else (fuck-up))))

(define (read-likelihood-file pathname)
 (if (can-open-file-for-input? pathname)
     (map result->likelihood (removeq #f (read-object-from-file pathname)))
     '()))

(define (dtw-result->distance result)
 (let ((distance (vector-ref result 4)))
 (list (vector-ref result 1) (vector-ref result 3)
       (cond ((eq? distance 'inf.) infinity)
		 ((eq? distance '-inf.) minus-infinity)
		 (else distance)))))

(define (condition-likelihood verb condition likelihoods)
 (if (null? likelihoods)
     #f
     (let ((i (position
	       condition
	       (vector-ref
		(cond
		 ((member verb '("attach" "flee"))
		  '#(("two-thirds") ("plurality" "two-thirds")))
		 ((member verb '("chase" "dig" "enter"  "get" "replace"))
		  '#(("near-unanimous")
		     ("near-unanimous" "two-thirds")
		     ("near-unanimous" "plurality" "two-thirds")))
		 (else
		  '#(("unanimous")
		     ("near-unanimous" "unanimous")
		     ("near-unanimous" "two-thirds" "unanimous")
		     ("near-unanimous" "plurality" "two-thirds" "unanimous"))))
		(- (length likelihoods) 1)))))
      (if i (list-ref likelihoods i) #f))))

(define (best-likelihood likelihoods)
 (if (null? likelihoods)
     #f
     (list-ref likelihoods
	       (position (map-reduce max minus-infinity third likelihoods)
			 (map third likelihoods)))))

(define (read-alternate-likelihood-file pathname condition features)
 (let ((object (read-object-from-file pathname)))
  (case features
   ((1)
    (let ((likelihoods (map result->likelihood (first object))))
     (removeq #f
	      (map-verb-codes
	       (lambda (verb-code)
		(let ((verb (spaces->hyphens (verb-code->verb verb-code))))
		 (condition-likelihood
		  verb
		  condition
		  (remove-if-not
		   (lambda (likelihood) (string=? (second likelihood) verb))
		   likelihoods))))))))
   ((2)
    (removeq
     #f
     (map-verb-codes
      (lambda (verb-code)
       (let ((verb (spaces->hyphens (verb-code->verb verb-code))))
	(best-likelihood
	 (removeq
	  #f
	  (map (lambda (object)
		(condition-likelihood
		 verb
		 condition
		 (remove-if-not
		  (lambda (likelihood) (string=? (second likelihood) verb))
		  (map result->likelihood object))))
	       (rest object)))))))))
   (else (fuck-up)))))

(define (read-likelihoods corpus condition limbs features)
 (cond
  ((string=? corpus "C-D1-recognition")
   (map-reduce append
	       '()
	       (lambda (verb)
		(read-likelihood-file (string-append "/aux/qobi/likelihoods/"
						     corpus
						     "-"
						     limbs
						     "/"
						     (spaces->hyphens verb)
						     "-"
						     (number->string features)
						     "-"
						     condition)))
	       *verb-list*))
  ((string=? corpus "C-E1-recognition")
   (map-reduce
    append
    '()
    (lambda (vignette)
     (read-alternate-likelihood-file
      (string-append "/aux/qobi/likelihoods/" corpus "-" limbs "/" vignette)
      condition
      features))
    (directory-list (string-append "/aux/qobi/likelihoods/" corpus "-" limbs))))
  (else (fuck-up))))

(define (read-likelihoods-experiment4 condition n features)
 (map-reduce
  append
  '()
  (lambda (verb)
   (read-likelihood-file (string-append "/aux/qobi/likelihoods/experiment4/"
					(spaces->hyphens verb)
					"-"
					(number->string features)
					"-"
					condition
					"-"
					(number->string n))))
  *verb-list*))

(define (read-thresholds condition limbs features)
 (let* ((object (read-object-from-file
		 (string-append "/aux/qobi/likelihoods/cd1-best-thresholds-"
				condition
				".sc")))
	(object ((cond ((string=? limbs "with-limbs") first)
		       ((string=? limbs "without-limbs") second)
		       (else (fuck-up)))
		 object)))
  (list->vector
   (map-verb-codes
    (lambda (verb-code)
     (let ((verb (spaces->hyphens (verb-code->verb verb-code))))
      (if (assoc verb object)
	  (let ((threshold
		 (second
		  (list-ref (second (assoc verb object)) (- features 1)))))
	   (cond ((eq? threshold 'inf.) infinity)
		 ((eq? threshold '-inf.) minus-infinity)
		 (else threshold)))
	  #f)))))))

(define (experiment1 condition limbs)
 (let ((training-corpus (AMZ-C-D1-recognition-r19may2011))
       (training-likelihoods1
	(read-likelihoods "C-D1-recognition" condition limbs 1))
       (training-likelihoods2
	(read-likelihoods "C-D1-recognition" condition limbs 2))
       (test-corpus (AMZ-C-E1-recognition))
       (test-likelihoods1
	(read-likelihoods "C-E1-recognition" condition limbs 1))
       (test-likelihoods2
	(read-likelihoods "C-E1-recognition" condition limbs 2)))
  (pp (trained-per-verb-accuracies
       training-corpus
       training-likelihoods1
       training-likelihoods2
       test-corpus
       test-likelihoods1
       test-likelihoods2))
  (newline)
  (write (trained-aggregate-accuracy
	  training-corpus
	  training-likelihoods1
	  training-likelihoods2
	  test-corpus
	  test-likelihoods1
	  test-likelihoods2))
  (newline)))

(define (experiment2 condition limbs)
 (let ((test-corpus (AMZ-C-E1-recognition))
       (thresholds1 (read-thresholds condition limbs 1))
       (thresholds2 (read-thresholds condition limbs 2))
       (test-likelihoods1
	(read-likelihoods "C-E1-recognition" condition limbs 1))
       (test-likelihoods2
	(read-likelihoods "C-E1-recognition" condition limbs 2)))
  (pp (threshold-per-verb-accuracies thresholds1
				     thresholds2
				     test-corpus
				     test-likelihoods1
				     test-likelihoods2))
  (newline)
  (write (threshold-aggregate-accuracy thresholds1
				       thresholds2
				       test-corpus
				       test-likelihoods1
				       test-likelihoods2))
  (newline)))

(define (experiment3 condition limbs)
 (let ((training-corpus (AMZ-C-D1-recognition-r19may2011))
       (training-likelihoods1
	(read-likelihoods "C-D1-recognition" condition limbs 1))
       (training-likelihoods2
	(read-likelihoods "C-D1-recognition" condition limbs 2)))
  (pp (trained-per-verb-accuracies
       training-corpus
       training-likelihoods1
       training-likelihoods2
       training-corpus
       training-likelihoods1
       training-likelihoods2))
  (newline)
  (write (trained-aggregate-accuracy
	  training-corpus
	  training-likelihoods1
	  training-likelihoods2
	  training-corpus
	  training-likelihoods1
	  training-likelihoods2))
  (newline)))

(define (experiment4 condition n)
 (let ((training-corpus (AMZ-C-D1-recognition-r19may2011))
       (training-likelihoods1 (read-likelihoods-experiment4 condition n 1))
       (training-likelihoods2 (read-likelihoods-experiment4 condition n 2)))
  (pp (trained-per-verb-accuracies
       training-corpus
       training-likelihoods1
       training-likelihoods2
       training-corpus
       training-likelihoods1
       training-likelihoods2))
  (newline)
  (write (trained-aggregate-accuracy
	  training-corpus
	  training-likelihoods1
	  training-likelihoods2
	  training-corpus
	  training-likelihoods1
	  training-likelihoods2))
  (newline)))

(define (alist->latex alist sort-kind columns mean-over-corpus pathname)
 (write-file
  (let ((alist (case sort-kind
		((by-verb) (sort alist string<? first))
		((by-rank) (sort alist > second))
		((unsorted) alist)
		(else (fuck-up))))
	(rows (inexact->exact (round (/ (length alist) columns))))
	(mean-by-verb (/ (map-reduce + 0 second alist) (length alist))))
   (append
    (list
     "\\begin{center}"
     (string-append
      "\\begin{tabular}{"
      (reduce string-append (map-n (lambda (column) "lr") columns) "")
      "}"))
    (reduce
     append
     (map-n (lambda (row)
	     (map-n (lambda (column)
		     (let* ((entry (list-ref alist (+ (* column rows) row)))
			    (verb (first entry))
			    (number (second entry))
			    (highlight (if (> (length entry) 2)
					   (third entry)
					   #f)))
		      (string-append "\\emph{"
				     verb
				     "}&"
				     (cond  ((equal? highlight 'red) "{\\red ")
					    (highlight "{\\green ")
					    (else ""))
				     (number->string-of-length-and-precision
				      (/ (round (* 1000 number)) 10) 5 1)
				     (if highlight "}" "")
				     "\\%"
				     (if (= column (- columns 1)) "\\\\" "&"))))
		    columns))
	    rows)
     '())
    (list "\\end{tabular}\\\\"
	  (string-append "mean over corpus: "
			 (if (list? mean-over-corpus)
			     (string-append
			      (if (second mean-over-corpus)
				  (cond ((equal? (second mean-over-corpus) 'red) "{\\red ")
					((second mean-over-corpus) "{\\green ")
					(else ""))
				  "")
			      (number->string-of-length-and-precision
			       (/ (round (* 1000 (first mean-over-corpus))) 10) 5 1)
			      (if (second mean-over-corpus)
				  "}"
				  ""))
			     (number->string-of-length-and-precision
			      (/ (round (* 1000 mean-over-corpus)) 10) 5 1))
			 "\\%, mean by verb: "
			 (if (list? mean-over-corpus)
			     (string-append
			      (if (second mean-over-corpus)
				  (cond ((equal? (second mean-over-corpus) 'red) "{\\red ")
					((second mean-over-corpus) "{\\green ")
					(else ""))
				  "")
			      (number->string-of-length-and-precision
			       (/ (round (* 1000 mean-by-verb)) 10) 5 1)
			      (if (second mean-over-corpus)
				  "}"
				  ""))
			     (number->string-of-length-and-precision
			      (/ (round (* 1000 mean-by-verb)) 10) 5 1))
			 "\\%")
	  "\\end{center}")))
  (default-extension pathname "tex")))

(define (agreements->latex agreements pathname)
 (define (percent n)
  (number->string-of-length-and-precision (/ (round (* 1000 n)) 10) 5 1))
 (define (mean l) (/ (reduce + l 0) (length l)))
 (write-file
  (append
   (list "\\begin{center}"
	 "\\begin{tabular}{lrrrrrr}"
	 "&(a)&(b)&(c)&(d)&(e)&(f)\\\\"
	 "\\hline")
   (map (lambda (agreement)
	 (let* ((verb (first agreement)))
	  (string-append "\\emph{"
			 verb
			 "}&"
			 (percent (second agreement))
			 "\\%&"
			 (percent (third agreement))
			 "\\%&"
			 (percent (fourth agreement))
			 "\\%&"
			 (percent (fifth agreement))
			 "\\%&"
			 (percent (sixth agreement))
			 "\\%&"
			 (percent (seventh agreement))
			 "\\%\\\\")))
	agreements)
   (list "\\hline"
	 (string-append "&"
			(percent (mean (map second agreements)))
			"\\%&"
			(percent (mean (map third agreements)))
			"\\%&"
			(percent (mean (map fourth agreements)))
			"\\%&"
			(percent (mean (map fifth agreements)))
			"\\%&"
			(percent (mean (map sixth agreements)))
			"\\%&"
			(percent (mean (map seventh agreements)))
			"\\%\\\\")
	 "\\end{tabular}"
	 "\\end{center}"))
  (default-extension pathname "tex")))

(define-command (main1)
 (let* ((training-corpus (AMZ-C-D1-recognition-r19may2011))
	(test-corpus (AMZ-C-E1-recognition))
	(indexed-training-corpus (index-corpus training-corpus)))
  (pp (per-verb-accuracies baseline-judgement training-corpus))
  (newline)
  (write (aggregate-accuracy baseline-judgement training-corpus))
  (newline)
  (pp (per-verb-accuracies baseline-judgement test-corpus))
  (newline)
  (write (aggregate-accuracy baseline-judgement test-corpus))
  (newline)
  (pp (per-verb-accuracies (memorize-judgement indexed-training-corpus)
			   test-corpus))
  (newline)
  (write (aggregate-accuracy (memorize-judgement indexed-training-corpus)
			     test-corpus))
  (newline)
  (pp (per-verb-accuracies (memorize-undarkify-judgement indexed-training-corpus)
			   test-corpus))
  (write (aggregate-accuracy (memorize-undarkify-judgement indexed-training-corpus)
			     test-corpus))
  (newline)))

(define-command (main2 (required (condition "condition" string-argument))
		       (required (limbs "limbs" string-argument)))
 (experiment1 condition limbs))

(define-command (main3)
 (for-each (lambda (condition)
	    (for-each (lambda (limbs)
		       (display condition)
		       (display " ")
		       (display limbs)
		       (newline)
		       (experiment1 condition limbs))
		      '("with-limbs" "without-limbs")))
	   '("unanimous" "near-unanimous" "two-thirds" "plurality")))

(define-command (main4)
 (let* ((training-corpus (AMZ-C-D1-recognition-r19may2011))
	(test-corpus (AMZ-C-E1-recognition))
	(indexed-training-corpus (index-corpus training-corpus)))
  (for-each
   (lambda (sample)
    (let ((record (assoc (first sample) indexed-training-corpus)))
     (when record
      (unless (eq? (fourth sample)
		   (vector-ref (second record)
			       (- (verb->verb-code (third sample)) 1)))
       (write (list (first sample)
		    (third sample)
		    (fourth sample)
		    (vector-ref (second record)
				(- (verb->verb-code (third sample)) 1))))
       (newline)))))
   test-corpus)))

(define-command (main5 (required (condition "condition" string-argument))
		       (required (limbs "limbs" string-argument)))
 (experiment2 condition limbs))

(define-command (main6)
 (for-each (lambda (condition)
	    (for-each (lambda (limbs)
		       (display condition)
		       (display " ")
		       (display limbs)
		       (newline)
		       (experiment2 condition limbs))
		      '("with-limbs" "without-limbs")))
	   '("unanimous" "near-unanimous" "two-thirds" "plurality")))

(define-command (main7 (required (condition "condition" string-argument))
		       (required (limbs "limbs" string-argument)))
 (let ((test-corpus (AMZ-C-E1-recognition))
       (thresholds1 (read-thresholds condition limbs 1))
       (thresholds2 (read-thresholds condition limbs 2))
       (indexed-test-likelihoods1
	(index-likelihoods
	 (read-likelihoods "C-E1-recognition" condition limbs 1)))
       (indexed-test-likelihoods2
	(index-likelihoods
	 (read-likelihoods "C-E1-recognition" condition limbs 2))))
  (for-each
   (lambda (vignette)
    (write
     (list vignette
	   (system-likelihood-vector
	    indexed-test-likelihoods1 indexed-test-likelihoods2 vignette)))
    (newline))
   (remove-duplicates (map first test-corpus)))))

(define-command (main8 (required (condition "condition" string-argument))
		       (required (limbs "limbs" string-argument)))
 (let ((training-corpus (AMZ-C-D1-recognition-r19may2011))
       (thresholds1 (read-thresholds condition limbs 1))
       (thresholds2 (read-thresholds condition limbs 2))
       (indexed-training-likelihoods1
	(index-likelihoods
	 (read-likelihoods "C-D1-recognition" condition limbs 1)))
       (indexed-training-likelihoods2
	(index-likelihoods
	 (read-likelihoods "C-D1-recognition" condition limbs 2))))
  (map-verb-codes
   (lambda (verb-code)
    (display (verb-code->verb verb-code))
    (newline)
    (let ((slow (train-threshold training-corpus
				 indexed-training-likelihoods1
				 verb-code))
	  (fast (fast-train-threshold training-corpus
				      indexed-training-likelihoods1
				      verb-code))
	  (old (vector-ref thresholds1 (- verb-code 1))))
     (write slow)
     (newline)
     (write fast)
     (newline)
     (write old)
     (newline)
     (write (list (equal? slow fast) (equal? slow old) (equal? fast old)))
     (newline))
    (let ((slow (train-threshold training-corpus
				 indexed-training-likelihoods2
				 verb-code))
	  (fast (fast-train-threshold training-corpus
				      indexed-training-likelihoods2
				      verb-code))
	  (old (vector-ref thresholds2 (- verb-code 1))))
     (write slow)
     (newline)
     (write fast)
     (newline)
     (write old)
     (newline)
     (write (list (equal? slow fast) (equal? slow old) (equal? fast old)))
     (newline))
    (newline)))))

(define-command (main9)
 (for-each
  (lambda (condition)
   (for-each
    (lambda (limbs)
     (display condition)
     (display " ")
     (display limbs)
     (display " ")
     (let ((training-corpus (AMZ-C-D1-recognition-r19may2011))
	   (indexed-training-likelihoods1
	    (index-likelihoods
	     (read-likelihoods "C-D1-recognition" condition limbs 1)))
	   (indexed-training-likelihoods2
	    (index-likelihoods
	     (read-likelihoods "C-D1-recognition" condition limbs 2))))
      (display
       (countq
	#t
	(map-verb-codes
	 (lambda (verb-code)
	  (let ((threshold (fast-train-threshold training-corpus
						 indexed-training-likelihoods1
						 verb-code)))
	   (or (not threshold)
	       (and (not (= threshold infinity))
		    (not (= threshold minus-infinity)))))))))
      (display " ")
      (display
       (countq
	#t
	(map-verb-codes
	 (lambda (verb-code)
	  (let ((threshold (fast-train-threshold training-corpus
						 indexed-training-likelihoods2
						 verb-code)))
	   (or (not threshold)
	       (and (not (= threshold infinity))
		    (not (= threshold minus-infinity)))))))))
      (newline)))
    '("with-limbs" "without-limbs")))
  '("unanimous" "near-unanimous" "two-thirds" "plurality")))

(define-command (main10)
 (for-each (lambda (condition)
	    (for-each (lambda (limbs)
		       (display condition)
		       (display " ")
		       (display limbs)
		       (newline)
		       (experiment3 condition limbs))
		      '("with-limbs" "without-limbs")))
	   '("unanimous" "near-unanimous" "two-thirds" "plurality")))

(define-command (main11)
 (for-each (lambda (condition)
	    (for-each (lambda (n)
		       (display condition)
		       (display " ")
		       (display n)
		       (newline)
		       (experiment4 condition n))
		      '(10 20 30 40)))
	   '("unanimous")))

(define-command (main12)
 (let* ((training-corpus (AMZ-C-D1-recognition-r19may2011))
	(training-likelihoods
	 (read-likelihood-file
	  "/home/qobi/darpa-collaboration/documentation/C-D1-recognition-hand-detector-scores.sc"))
	(test-corpus (AMZ-C-E1-recognition))
	(test-likelihoods
	 (read-likelihood-file
	  "/home/qobi/darpa-collaboration/documentation/C-E1-recognition-hand-detector-scores.sc")))
  (pp (trained-per-verb-accuracies
       training-corpus
       training-likelihoods
       training-likelihoods
       test-corpus
       test-likelihoods
       test-likelihoods))
  (newline)
  (write (trained-aggregate-accuracy
	  training-corpus
	  training-likelihoods
	  training-likelihoods
	  test-corpus
	  test-likelihoods
	  test-likelihoods))
  (newline)))

(define (main13)
 (when #f
  (set! *official-directory*
	"/home/qobi/darpa-collaboration/documentation/official/old-official/")
  (set! *old-AMZ-C-D1-recognition* (AMZ-C-D1-recognition-r19may2011))
  (set! *old-AMZ-C-E1-recognition* (AMZ-C-E1-recognition))
  (set! *official-directory*
	"/home/qobi/darpa-collaboration/documentation/official/new-official/")
  (set! *new-AMZ-C-D1-recognition* (AMZ-C-D1-recognition-m15aug2011))
  (set! *new-AMZ-C-E1-recognition* (AMZ-C-E1-recognition-m15aug2011))))

(define-command (main14)
 (set! *official-directory*
       "/home/qobi/darpa-collaboration/documentation/official/new-official/")
 (let* ((training-corpus (AMZ-C-D1-recognition-m15aug2011))
	(test-corpus (AMZ-C-E1-recognition-m15aug2011))
	(indexed-training-corpus (index-corpus training-corpus)))
  (alist->latex
   (per-verb-accuracies baseline-judgement training-corpus)
   'by-verb
   3
   (aggregate-accuracy baseline-judgement training-corpus)
   "/home/qobi/darpa2010a/documentation/evaluation-report-f26aug2011/blind-baseline-C-D1")
  (alist->latex
   (per-verb-accuracies baseline-judgement test-corpus)
   'by-verb
   3
   (aggregate-accuracy baseline-judgement test-corpus)
   "/home/qobi/darpa2010a/documentation/evaluation-report-f26aug2011/blind-baseline-C-E1")
  (alist->latex
   (per-verb-accuracies (memorize-judgement indexed-training-corpus)
			test-corpus)
   'by-verb
   3
   (aggregate-accuracy (memorize-judgement indexed-training-corpus)
		       test-corpus)
   "/home/qobi/darpa2010a/documentation/evaluation-report-f26aug2011/memorizing-baseline-C-E1")))

(define-command (main15)
 (let* ((corpus1 (roundtable-r31mar2011))
	(corpus2 (roundtable-f15apr2011)))
  (set! *official-directory*
	"/home/qobi/darpa-collaboration/documentation/official/new-official/")
  (let* ((corpus3 (AMZ-C-D1-recognition-m15aug2011))
	 (corpus4 (AMZ-C-E1-recognition-m15aug2011))
	 (agreements1 (raw-intercoder-agreement corpus1))
	 (agreements2 (raw-intercoder-agreement corpus2))
	 (agreements3 (raw-dark-intercoder-agreement corpus3))
	 (agreements4 (raw-dark-intercoder-agreement corpus4)))
   (unless (= (length agreements1)
	      (length agreements2)
	      (length agreements3)
	      (length agreements4))
    (fuck-up))
   (let ((agreements
	  (map (lambda (agreement1 agreement2 agreement3 agreement4)
		(unless (and (string=? (first agreement1) (first agreement2))
			     (string=? (first agreement1) (first agreement3))
			     (string=? (first agreement1) (first agreement4)))
		 (fuck-up))
		(list (first agreement1)
		      (fourth agreement1)
		      (+ (fourth agreement1) (third agreement1))
		      (fourth agreement2)
		      (+ (fourth agreement2) (third agreement2))
		      (third agreement3)
		      (third agreement4)))
	       agreements1
	       agreements2
	       agreements3
	       agreements4)))
    (agreements->latex
     agreements
     "/home/qobi/darpa2010a/documentation/evaluation-report-f26aug2011/intercoder-disagreement")))))

(define-command (main16)
 (let* ((runs (read-file "/home/qobi/darpa2010a/documentation/evaluation-report-f26aug2011/table.text"))
	(roc (map (lambda (line)
		   (first
		    (read-object-from-file
		     (string-append "/net/perisikan/aux/abarbu/data/"
				    (field-ref line 1)
				    "/ce1-areas-roc.sc"))))
		  runs))
	(pr (map (lambda (line)
		  (first
		   (read-object-from-file
		    (string-append "/net/perisikan/aux/abarbu/data/"
				   (field-ref line 1)
				   "/ce1-areas-pr.sc"))))
		 runs)))
  (let ((alist (map-verb-codes
		(lambda (verb-code)
		 (let ((verb (verb-code->verb verb-code)))
		  (list verb
			(map-reduce
			 max
			 minus-infinity
			 (lambda (roc) (second (assoc verb roc)))
			 roc)))))))
   (alist->latex
    alist
    'by-rank
    3
    (/ (map-reduce + 0 second alist) (length alist))
    "/home/qobi/darpa2010a/documentation/evaluation-report-f26aug2011/best-roc"))
  (let ((alist (map-verb-codes
		(lambda (verb-code)
		 (let ((verb (verb-code->verb verb-code)))
		  (list verb
			(map-reduce
			 max
			 minus-infinity
			 (lambda (pr) (second (assoc verb pr)))
			 pr)))))))
   (alist->latex
    alist
    'by-rank
    3
    (/ (map-reduce + 0 second alist) (length alist))
    "/home/qobi/darpa2010a/documentation/evaluation-report-f26aug2011/best-pr"))))

(define-command (main17)
 (let* ((runs
	 (map (lambda (line) (field-ref line 1))
	      (read-file "/home/qobi/darpa2010a/documentation/evaluation-report-f26aug2011/table.text")))
	(areas (map (lambda (run)
		     (first
		      (read-object-from-file
		       (string-append "/net/perisikan/aux/abarbu/data/"
				      run
				      "/ce1-cd1-areas-pr.sc"))))
		    runs)))
  (pp (map-verb-codes
       (lambda (verb-code)
	(let* ((verb (verb-code->verb verb-code))
	       (best (map-reduce
		      max
		      minus-infinity
		      (lambda (areas) (second (assoc verb areas)))
		      areas)))
	 (list verb
	       (list-ref
		runs
		(position best
			  (map (lambda (areas) (second (assoc verb areas)))
			       areas))))))))
  (newline)))

;;; Tam V'Nishlam Shevah L'El Borei Olam
