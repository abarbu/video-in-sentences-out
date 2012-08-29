(MODULE
  ANNOTATION-GUI-LIMBS
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
(include "annotation-gui-limbs.sch")

(set! *program* "annotation-gui-limbs")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

;;; (run (string->darpa-video "Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a") 1)
;;; ./annotation-gui-limbs -darpa Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a 1

(define *first-video-frame* #f)
(define *imlib-image* #f)
(define *limb-annotation* #f)
(define *mode* #f)
(define *resize?* #t)
(define *selected-vertex* #f)
(define *videos-frames* #f)
(define *width* #f)

(define *visible?* #t)

(define (*frame-number*) (second (first *videos-frames*)))
(define (*large-gui?*) (or *resize?* (> *width* 650)))
(define (*video-name*) (first (first *videos-frames*)))

(define (redraw)
 (redraw-buttons)
 (redraw-display-pane))

(define (get-next-limb-for-annotation annotation)
 (cond
  ((not (limb-annotation-l-arm annotation)) set-limb-annotation-l-arm!)
  ((not (limb-annotation-l-elbow annotation)) set-limb-annotation-l-elbow!)
  ((not (limb-annotation-l-shoulder annotation)) set-limb-annotation-l-shoulder!)
  ((not (limb-annotation-r-shoulder annotation)) set-limb-annotation-r-shoulder!)
  ((not (limb-annotation-r-elbow annotation)) set-limb-annotation-r-elbow!)
  ((not (limb-annotation-r-arm annotation)) set-limb-annotation-r-arm!)
  ((not (limb-annotation-l-foot annotation)) set-limb-annotation-l-foot!)
  ((not (limb-annotation-l-knee annotation)) set-limb-annotation-l-knee!)
  ((not (limb-annotation-l-pelvis annotation)) set-limb-annotation-l-pelvis!)
  ((not (limb-annotation-r-pelvis annotation)) set-limb-annotation-r-pelvis!)
  ((not (limb-annotation-r-knee annotation)) set-limb-annotation-r-knee!)
  ((not (limb-annotation-r-foot annotation)) set-limb-annotation-r-foot!)
  (else #f)))

(define (next-limb-for-annotation annotation)
 (cond
  ((not (limb-annotation-l-arm annotation)) "l-arm")
  ((not (limb-annotation-l-elbow annotation)) "l-elbow")
  ((not (limb-annotation-l-shoulder annotation)) "l-shoulder")
  ((not (limb-annotation-r-shoulder annotation)) "r-shoulder")
  ((not (limb-annotation-r-elbow annotation)) "r-elbow")
  ((not (limb-annotation-r-arm annotation)) "r-arm")
  ((not (limb-annotation-l-foot annotation)) "l-foot")
  ((not (limb-annotation-l-knee annotation)) "l-knee")
  ((not (limb-annotation-l-pelvis annotation)) "l-pelvis")
  ((not (limb-annotation-r-pelvis annotation)) "r-pelvis")
  ((not (limb-annotation-r-knee annotation)) "r-knee")
  ((not (limb-annotation-r-foot annotation)) "r-foot")
  (else #f)))

(define (load-frame!)
 (set! *imlib-image*
       (imlib-load-image! (ppm-pathname (*video-name*) (*frame-number*))))
 (imlib-context-set-image! *imlib-image*)
 (set! *width* (imlib-get-image-width))
 (reset!)
 (load!))

(define (load!)
 (when (file-exists?
	(human-limb-annotation-pathname (*video-name*) (*frame-number*)))
  (set!
   *limb-annotation*
   (read-object-from-file
    (human-limb-annotation-pathname (*video-name*) (*frame-number*)))))
 (unless (get-next-limb-for-annotation *limb-annotation*)
  (set! *mode* 'select)))

(define (save!)
 (write-object-to-file
  *limb-annotation*
  (human-limb-annotation-pathname (*video-name*) (*frame-number*))))

;;;

(define (save-button!)
 (save!)
 (statistics-collect-and-write! (*video-name*) (*frame-number*) 'limb))

(define (load-button!)
 (load!)
 (statistics-reset!)
 (redraw))

(define (next!)
 (set! *videos-frames* (ring-forward *videos-frames*))
 (load-frame!))

(define (previous!)
 (set! *videos-frames* (ring-backward *videos-frames*))
 (load-frame!))

(define (new-number->string-of-length-and-precision number length precision)
 (let* ((negative? (negative? number))
	(integer-part (inexact->exact (floor (abs number))))
	(fraction-part
	 (inexact->exact
	  (floor (* (expt 10 precision) (- (abs number) integer-part)))))
	(integer-part-string (number->string integer-part))
	(fraction-part-string (number->string fraction-part)))
  (if negative?
      (string-append
       (make-string
	(- length (string-length integer-part-string) 2 precision) #\space)
       "-"
       integer-part-string
       "."
       (make-string (- precision (string-length fraction-part-string)) #\0)
       fraction-part-string)
      (string-append
       (make-string
	(- length (string-length integer-part-string) 1 precision) #\space)
       integer-part-string
       "."
       (make-string (- precision (string-length fraction-part-string)) #\0)
       fraction-part-string))))

(define (format-statistics-position-and-time)
 (format #f "~a@~a ~a% ~as"
	 (darpa-video->string (*video-name*))
	 (*frame-number*)
	 (new-number->string-of-length-and-precision
	  (* (/ (position *first-video-frame* *videos-frames*)
		(length *videos-frames*))
	     100)
	  6 2)
	 (new-number->string-of-length-and-precision (statistics-time-elapsed) 8 4)))

(define (next-button!)
 (next!)
 (redraw)
 (message (string-append "Next: " (format-statistics-position-and-time)))
 (statistics-reset!))

(define (previous-button!)
 (previous!)
 (redraw)
 (message (string-append "Previous: " (format-statistics-position-and-time)))
 (statistics-reset!))

(define (save-and-next-button!)
 (save!)
 (let ((msg (format-statistics-position-and-time)))
  (statistics-collect-and-write! (*video-name*) (*frame-number*) 'limb)
  (next!)
  (redraw)
  (message (string-append "Save&Next: " msg))))

(define (set-videos-frames! l)
 (set! *videos-frames* l)
 (set! *first-video-frame* (first l)))

(define (run video-name frame)
 (set-videos-frames! `((,video-name ,frame)))
 (statistics-reset!)
 (load-frame!)
 (viewer '()))

(define (run-list videos-frames)
 (set-videos-frames! videos-frames)
 (statistics-reset!)
 (load-frame!)
 (viewer '()))

;;;

(define (reset!)
 (set! *mode* 'add)
 (set! *selected-vertex* #f)
 (set! *limb-annotation* (make-limb-annotation #f #f #f #f #f #f #f #f #f #f #f #f))
 (unless (get-next-limb-for-annotation *limb-annotation*)
  (set! *mode* 'select)))

(define (draw-selected-vertex annotation selected gc-line gc-circle)
 (let ((radius 2))
  (for-each
   (lambda (l)
    (let ((l (make-line-segment
	      (if (eq? (p l) (limb-position ((selected-getter selected) annotation)))
		  (selected-point selected)
		  (p l))
	      (if (eq? (q l) (limb-position ((selected-getter selected) annotation)))
		  (selected-point selected)
		  (q l)))))
     (xdrawline *display* *display-pane* gc-line
		(x (p l)) (y (p l)) (x (q l)) (y (q l)))))
   (remove-if-not (lambda (a) (or (eq? (limb-position ((selected-getter selected) annotation)) (p a))
			     (eq? (limb-position ((selected-getter selected) annotation)) (q a))))
		  (limbs->lines annotation)))
  (xdrawarc *display*
	    *display-pane*
	    gc-circle
	    (- (x (selected-point selected)) radius)
	    (- (y (selected-point selected)) radius)
	    (+ (* 2 radius) 1)
	    (+ (* 2 radius) 1)
	    (* 0 360)
	    (* 64 360))))

(define-application viewer (if (*large-gui?*) 1300 735) (if (*large-gui?*) 700 390) 5 2 6
 (lambda ()
  ;; (setup-*thick-red-gc*)
  (setup-extra-x-gcs)
  (define-button 0 0 "Help" #f help-command)
  (define-button 5 0 "Quit" #f quit)
  (define-button 1 0
   (lambda ()
    (if (next-limb-for-annotation *limb-annotation*)
	(string-append "Next:" (next-limb-for-annotation *limb-annotation*))
	"All limbs placed"))
   #f
   (lambda () #f))
  (define-button 2 0
   (lambda ()
    (cond ((equal? *mode* 'add) "Add Limb")
	  ((equal? *mode* 'select)
	   (if *selected-vertex*
	       (format #f "Selected: ~a" (selected-label *selected-vertex*))
	       "Select Limb"))
	  (else "None")))
   #f
   (lambda ()
    (cond ((equal? *mode* 'add) (set! *mode* 'select))
	  ((equal? *mode* 'select) (set! *mode* 'none))
	  ((equal? *mode* 'none) (set! *mode* 'add))
	  (else (set! *mode* 'select)))
    (redraw)))
  (define-button 3 0
   (lambda () (if *visible?* "Visible" "Occluded"))
   #f
   (lambda ()
    (set! *visible?* (not *visible?*))
    (when *selected-vertex*
     (set-limb-visible?! ((selected-getter *selected-vertex*) *limb-annotation*)
			 *visible?*))
    (redraw)))
  (define-button 4 0
   "Reset"
   #f
   (lambda () (reset!) (redraw)))
  ;;
  (define-button 0 1 "Load" #f load-button!)
  (define-button 1 1 "Save" #f save-button!)
  (define-button 2 1 "Save & Next" #f save-and-next-button!)
  (define-button 3 1 "Previous" #f previous-button!)
  (define-button 4 1 "Next" #f next-button!)
  ;;
  (define-key (list (control #\x) (control #\c)) "Quit" quit)
  (define-key #\v "Visibility" (lambda ()
				(set! *visible?* (not *visible?*))
				(when *selected-vertex*
				 (set-limb-visible?! ((selected-getter *selected-vertex*) *limb-annotation*)
						     *visible?*))
				(redraw)))
  (define-key #\n "Next" next-button!)
  (define-key #\s "Save" save-button!)
  (define-key #\f "Save & Next" save-and-next-button!)
  (define-key #\p "Prev" previous-button!)
  (define-key (control #\h) "Help" help-command))
 (lambda () #f)
 (lambda () (free-extra-x-gcs))
 (lambda ()
  (draw-imlib-pixmap *imlib-image* 0 0)
  (draw-limb-annotation *limb-annotation*)
  (when *selected-vertex*
   (draw-selected-vertex *limb-annotation* *selected-vertex*
			 *medium-orange-gc*
			 (if (limb-visible? ((selected-getter *selected-vertex*) *limb-annotation*))
			     *medium-green-gc*
			     *medium-red-gc*)))
  (define-region 0 0 *display-pane-width* *display-pane-height*
   (lambda (x1 y1)
    (statistics-click!)
    (cond ((equal? *mode* 'select)
	   (set! *selected-vertex* #f)
	   (let ((closest (remove-if
			   (lambda (a) (> (distance (vector x1 y1) (selected-point a)) 10))
			   (limbs->selected *limb-annotation*))))
	    (unless (null? closest)
	     (set! *selected-vertex* (minimump
				      closest
				      (lambda (a) (distance (vector x1 y1) (selected-point a)))))
	     (draw-limb-annotation *limb-annotation*)
	     (when *selected-vertex*
	      (draw-selected-vertex *limb-annotation* *selected-vertex*
				    *medium-orange-gc*
				    (if (limb-visible? ((selected-getter *selected-vertex*) *limb-annotation*))
					*medium-green-gc*
					*medium-red-gc*))
	      (set! *visible?* (limb-visible? ((selected-getter *selected-vertex*) *limb-annotation*)))
	      ((selected-setter! *selected-vertex*) *limb-annotation*
	       (make-limb
		(list->vector
		 (tracking-pointer
		  #t
		  #f
		  (lambda (x2 y2)
		   (set-selected-point! *selected-vertex* (vector x2 y2))
		   (draw-selected-vertex *limb-annotation* *selected-vertex* *thin-flipping-gc* *thin-flipping-gc*))))
		(limb-visible? ((selected-getter *selected-vertex*) *limb-annotation*))))))
	    (redraw)))
	  ((equal? *mode* 'add)
	   ((get-next-limb-for-annotation *limb-annotation*)
	    *limb-annotation*
	    (make-limb (vector x1 y1) *visible?*))
	   (unless (get-next-limb-for-annotation *limb-annotation*)
	    (set! *mode* 'select))
	   (redraw))
	  (else #f))))))

(define-command
 (main (exactly-one ("standard" standard?
		     (corpus "corpus" string-argument "")
		     (sequence "sequence" string-argument "")
		     (person "person" string-argument "")
		     (location "location" string-argument "")
		     (n "n" integer-argument 0)
		     (s-frame "frame" integer-argument 0))
		    ("darpa" darpa?
		     (name "name" string-argument "")
		     (d-frame "frame" integer-argument 0))
		    ("list" list-filename? (list-filename "filename" string-argument "")))
       (at-most-one ("resize" resize?)))
 (set! *resize?* resize?)
 (if list-filename?
     (run-list
      (map
       (lambda (a)
	(list (string->darpa-video (field-ref a 0)) (string->number (field-ref a 1))))
       (reverse (read-file list-filename))))
     (let* ((video-name
	     (cond (standard?
		    (standard-corpus-video corpus sequence person location n))
		   (darpa? (string->darpa-video name))
		   (else (fuck-up)))))
      (run video-name (cond (standard? s-frame)
			    (darpa? d-frame)
			    (else (fuck-up)))))))
