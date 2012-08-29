;; define external camera-get-<control> and camera-set-<control>
(define-macro define-control
 (lambda (f e)
  (e (let* ((control (second f))
	    (v4l2-cid (third f))
	    (scheme-cid (string->pretty-symbol
			 (schemeify-name (symbol->string v4l2-cid)))))
      `(begin
	(define-external (,(append-symbols 'camera-set- control '!) camera value) toollib-camera)
	(define-external ,scheme-cid toollib-camera)
	(define-external (,(append-symbols 'camera-get- control) camera) toollib-camera))) e)))

(include "toollib-control-data.sch")
