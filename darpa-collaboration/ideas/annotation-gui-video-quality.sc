(MODULE
  ANNOTATION-GUI-VIDEO-QUALITY
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
(include "annotation-gui-video-quality.sch")

(set! *program* "annotation-gui-video-quality")
(set! *panic?* #f)

;;; Macros

;;; Structures

;;; Variables

(define *videos* #f)
(define *quality-annotation* #f)
(define (*video-name*) (first *videos*))
(define (*frame-number*) 0)
(define *first-video* #f)

(define-structure video-quality-annotation score)

(define (redraw)
 (redraw-buttons)
 (redraw-display-pane))

(define (load-frame!)
 (reset!)
 (load!))

(define (load!)
 (when (file-exists?
	(human-annotation-video-quality-pathname (*video-name*)))
  (set!
   *quality-annotation*
   (read-object-from-file
    (human-annotation-video-quality-pathname (*video-name*))))))

(define (save!)
 (write-object-to-file
  *quality-annotation*
  (human-annotation-video-quality-pathname (*video-name*))))

;;;

(define (save-button!)
 (save!)
 (statistics-collect-and-write! (*video-name*) 'limb))

(define (load-button!)
 (load!)
 (statistics-reset!)
 (redraw))

(define (next!)
 (set! *videos* (ring-forward *videos*))
 (load-frame!))

(define (previous!)
 (set! *videos* (ring-backward *videos*))
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
	  (* (/ (position *first-video* *videos*)
		(length *videos*))
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
  (statistics-collect-and-write! (*video-name*) 'limb)
  (next!)
  (redraw)
  (message (string-append "Save&Next: " msg))))

(define (save-and-previous-button!)
 (save!)
 (let ((msg (format-statistics-position-and-time)))
  (statistics-collect-and-write! (*video-name*) 'limb)
  (previous!)
  (redraw)
  (message (string-append "Save&Next: " msg))))


(define (run video-name)
 (set! *videos* (list video-name))
 (set! *first-video* (first *videos*))
 (statistics-reset!)
 (load-frame!)
 (viewer '()))

(define (run-list videos)
 (set! *videos* videos)
 (set! *first-video* (first *videos*))
 (statistics-reset!)
 (load-frame!)
 (viewer '()))

;;;

(define (set-annotation value)
  (set-video-quality-annotation-score! *quality-annotation* value)
  (redraw))
  
(define (reset!)
 (set! *quality-annotation* (make-video-quality-annotation 0)))

(define (watch-button!)
 (message "Playing video")
 (system (format #f "mplayer ~a" (mov-pathname (*video-name*)))))

(define-application viewer 1300 700 0 2 6
 (lambda ()
  (setup-extra-x-gcs)
  (define-button 0 0 "Help" #f help-command)
  (define-button 5 0 "Quit" #f quit)  
  (define-button 4 0 "Reset" #f (lambda () (reset!) (redraw)))
  (define-button 1 0 "Watch" #f watch-button!)
  (define-spinner-buttons 2 0
   "Quality"
   (lambda () (set-video-quality-annotation-score!
	       *quality-annotation*
	       (+ 1 (video-quality-annotation-score *quality-annotation*))))
   (lambda () (set-video-quality-annotation-score!
	       *quality-annotation*
	       (- (video-quality-annotation-score *quality-annotation*) 1)))
   (lambda () (number->string (video-quality-annotation-score *quality-annotation*))))
  ;;
  (define-button 0 1 "Load" #f load-button!)
  (define-button 1 1 "Save" #f save-button!)
  (define-button 2 1 "Save & Next" #f save-and-next-button!)
  (define-button 3 1 "Previous" #f previous-button!)
  (define-button 4 1 "Next" #f next-button!)
  ;;
  (define-key (list (control #\x) (control #\c)) "Quit" quit)
  (define-key #\n "Next" next-button!)
  (define-key #\s "Save" save-button!)
  (define-key #\f "Save & Next" save-and-next-button!)
  (define-key #\- "Save & Prev" (lambda () (save-and-previous-button!) (watch-button!)))
  (define-key #\= "Save & Next" (lambda () (save-and-next-button!) (watch-button!)))
  (define-key #\p "Prev" previous-button!)
  (define-key #\0 "Zero" (lambda () (set-annotation 0)))
  (define-key #\1 "One" (lambda () (set-annotation 1)))
  (define-key #\2 "Two" (lambda () (set-annotation 2)))
  (define-key #\3 "Three" (lambda () (set-annotation 3)))
  (define-key (control #\h) "Help" help-command)
  (define-key #\w "Watch" watch-button!))
 (lambda () #f)
 (lambda () (free-extra-x-gcs))
 (lambda () 
  (xdrawstring *display* *display-pane* *roman-gc*
	       10
	       20
	       (darpa-video->string (*video-name*))
	       (string-length (darpa-video->string (*video-name*))))
  #f))


(define (run-by-corpus-verb corpus verb)
 (let ((video-list (videos-by-verb corpus verb)))
  (run-list video-list)))

(define-command
 (main (required (corpus "corpus" string-argument))
       (required (verb "verb" string-argument)))
  (run-by-corpus-verb corpus verb))



;;; (run (string->darpa-video "Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a"))
;; (run-list (list (string->darpa-video "Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a")
;;                 (string->darpa-video "Attach3_A1_C2_Act4_URBAN3_ML_MIDD_DARK_442bafcf-c5af-11df-ab61-e80688cb869a")))
;;; ./annotation-gui-video-quality -darpa Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a 1
