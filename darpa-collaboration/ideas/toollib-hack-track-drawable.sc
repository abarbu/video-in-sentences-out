(MODULE TOOLLIB-HACK-TRACK-DRAWABLE)


(include "QobiScheme.sch")
(include "toollib-c-macros.sch")
(include "toollib-c-externals.sch")
(include "toollib-hack-track.sch")

;;; Drawable management

(c-include "toollib-hack-track-c.h")

(define hack-track-invalid-magic (c-value int HACK_TRACK_INVALID))
(define hack-track-arc-magic (c-value int HACK_TRACK_ARC))
(define hack-track-line-magic (c-value int HACK_TRACK_LINE))
(define hack-track-rectangle-magic (c-value int HACK_TRACK_RECTANGLE))

(define hack-track-nth-drawable-type
 (c-function int ("hack_track_nth_drawable_type" int)))
(define hack-track-nth-arc (c-function pointer ("hack_track_nth_arc" int)))
(define hack-track-nth-line (c-function pointer ("hack_track_nth_line" int)))
(define hack-track-nth-rectangle
 (c-function pointer ("hack_track_nth_rectangle" int)))
(define hack-track-free-nth-drawable
 (c-function void ("hack_track_free_nth_drawable" int)))
(define hack-track-allocate-nth-drawable
 (c-function void ("hack_track_allocate_nth_drawable" int int)))

(define-structure drawable-arc d gc x y width height angle1 angle2)
(define-structure drawable-line d gc x1 y1 x2 y2)
(define-structure drawable-rectangle d gc x y width height)

(c-define-struct-field "hack_track_drawable_tag" "tag" int)

(c-define-struct-field "hack_track_arc" "tag" int)
(c-define-struct-field "hack_track_arc" "d" int)
(c-define-struct-field "hack_track_arc" "gc" pointer)
(c-define-struct-field "hack_track_arc" "x" int)
(c-define-struct-field "hack_track_arc" "y" int)
(c-define-struct-field "hack_track_arc" "width" int)
(c-define-struct-field "hack_track_arc" "height" int)
(c-define-struct-field "hack_track_arc" "angle1" int)
(c-define-struct-field "hack_track_arc" "angle2" int)

(c-define-struct-field "hack_track_line" "tag" int)
(c-define-struct-field "hack_track_line" "d" int)
(c-define-struct-field "hack_track_line" "gc" pointer)
(c-define-struct-field "hack_track_line" "x1" int)
(c-define-struct-field "hack_track_line" "y1" int)
(c-define-struct-field "hack_track_line" "x2" int)
(c-define-struct-field "hack_track_line" "y2" int)

(c-define-struct-field "hack_track_rectangle" "tag" int)
(c-define-struct-field "hack_track_rectangle" "d" int)
(c-define-struct-field "hack_track_rectangle" "gc" pointer)
(c-define-struct-field "hack_track_rectangle" "x" int)
(c-define-struct-field "hack_track_rectangle" "y" int)
(c-define-struct-field "hack_track_rectangle" "width" int)
(c-define-struct-field "hack_track_rectangle" "height" int)

(define hack-track-max-drawables (c-value int HACK_TRACK_MAX_DRAWABLES))

(define (hack-track-update-drawables! ds)
 (with-hack-track-paused
  (lambda ()
   (map-n
    (lambda (n) (hack-track-free-nth-drawable n))
    hack-track-max-drawables)
   (map-indexed
    (lambda (d i)
     (hack-track-allocate-nth-drawable i (scheme-drawable->tag d))
     (hack-track-update-drawable! d i))
    ds))))

(define (scheme-drawable->tag d)
 (cond ((drawable-arc? d) hack-track-arc-magic)
       ((drawable-line? d) hack-track-line-magic)
       ((drawable-rectangle? d) hack-track-rectangle-magic)
       (else (fuck-up))))

(define (hack-track-update-drawable! d i)
 (cond
  ((= (scheme-drawable->tag d) hack-track-arc-magic)
   (let ((arc (hack-track-nth-arc i)))
    (hack-track-arc-d-set! arc (drawable-arc-d d))
    (hack-track-arc-gc-set! arc (cdr (drawable-arc-gc d)))
    (hack-track-arc-x-set! arc (drawable-arc-x d))
    (hack-track-arc-y-set! arc (drawable-arc-y d))
    (hack-track-arc-width-set! arc (drawable-arc-width d))
    (hack-track-arc-height-set! arc (drawable-arc-height d))
    (hack-track-arc-angle1-set! arc (drawable-arc-angle1 d))
    (hack-track-arc-angle2-set! arc (drawable-arc-angle2 d))))
  ((= (scheme-drawable->tag d) hack-track-line-magic)
   (let ((line (hack-track-nth-line i)))
    (hack-track-line-d-set! line (drawable-line-d d))
    (hack-track-line-gc-set! line (cdr (drawable-line-gc d)))
    (hack-track-line-x1-set! line (drawable-line-x1 d))
    (hack-track-line-y1-set! line (drawable-line-y1 d))
    (hack-track-line-x2-set! line (drawable-line-x2 d))
    (hack-track-line-y2-set! line (drawable-line-y2 d))))
  ((= (scheme-drawable->tag d) hack-track-rectangle-magic)
   (let ((rectangle (hack-track-nth-rectangle i)))
    (hack-track-rectangle-d-set! rectangle (drawable-rectangle-d d))
    (hack-track-rectangle-gc-set! rectangle (cdr (drawable-rectangle-gc d)))
    (hack-track-rectangle-x-set! rectangle (drawable-rectangle-x d))
    (hack-track-rectangle-y-set! rectangle (drawable-rectangle-y d))
    (hack-track-rectangle-width-set! rectangle (drawable-rectangle-width d))
    (hack-track-rectangle-height-set!
     rectangle
     (drawable-rectangle-height d))))
  (else
   (format #t "~a ~a ~a~%" d i (scheme-drawable->tag d))
   (fuck-up))))
