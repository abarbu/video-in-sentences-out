(MODULE CUPEDRO-BINDINGS)

;;; -------------------------------------------------------------------- README
;;;
;;; Scheme interface to irobot cuda implementation
;;; See (run-cupedro video ...) function for a "howto"
;;;
;;; Aaron Michaux, November 2011
;;;

(include "QobiScheme-AD.sch")

(include "cupedro-bindings.sch")

(include "toollib-c-macros.sch")

(c-include "cupedro.h")

(define print-cuda-device-information
 ;; Prints device information
 (c-function void ("print_cuda_device_information")))

;;; -------------------------------------------------------- construct/destruct

(define (cupedro-new model-filenames pca-filename)
 ;; Creates a new cupedro object. /All/ filenames must be extant files, or the
 ;; irobot driver will call system.exit
 (when (null? model-filenames)
  (panic "Must specify at least one model filename!"))
 (let* ((c-prepend-to-string
         (c-function pointer ("prepend_to_string_list" pointer string)))
        (c-free-strings (c-function void ("free_string_list" pointer)))
        (c-cupedro-new (c-function pointer ("cupedro_new" pointer string)))
        (filenames
         (foldl (lambda (strings rhs) (c-prepend-to-string strings rhs))
                (rest model-filenames)
                (c-prepend-to-string 0 (first model-filenames))))
        (cupedro (c-cupedro-new filenames pca-filename)))
  (c-free-strings filenames)
  cupedro))

(define cupedro-delete! (c-function void ("cupedro_delete" pointer)))

;;; ---------------------------------------------------------------- Quiet mode

(define (cupedro-quiet? cupedro)
 ;; Only errors are printed when in quiet mode
 (define c-cupedro-quiet (c-function int ("cupedro_is_quiet" pointer)))
 (if (= 1 (c-cupedro-quiet cupedro)) #t #f))

(define (cupedro-set-quiet! cupedro quiet?)
 (define c-cupedro-set-quiet!
  (c-function void ("cupedro_set_quiet" pointer int)))
 (c-cupedro-set-quiet! cupedro (if quiet? 1 0)))

;;; --------------------------------------------------------- Model information

(define cupedro-number-of-models
 (c-function int ("cupedro_get_n_models" pointer)))

(define cupedro-model-filename
 (c-function string ("cupedro_get_model_filename" pointer int)))

(define cupedro-model-name
 (c-function string ("cupedro_get_model_name" pointer int)))

;;; ---------------------------------------------- Setting the output directory

(define cupedro-output-directory
 ;; If set, then boxes files are automatically written to the specified
 ;; directory whenever the detector is run. One boxes file per model/video.
 ;; By default, no output directory is set, and results must be read using
 ;; (cupedro-detect-recults cupedro model-index)
 (c-function string ("cupedro_get_output_dir" pointer)))

(define cupedro-set-output-directory!
 (c-function void ("cupedro_set_output_dir" pointer string)))

;;; ---------------------------------------------------------------- Thresholds

(define cupedro-threshold-adjustment
 (c-function double ("cupedro_get_thres_adj" pointer int)))

(define cupedro-set-threshold-adjustment!
 ;; negative values imply overdetect
 (c-function void ("cupedro_set_thres_adj" pointer int double)))

(define cupedro-set-global-threshold-adjustment!
 (c-function void ("cupedro_set_global_thres_adj" pointer double)))

(define cupedro-final-cascade-threshold
 (c-function double ("cupedro_get_final_cascade_thres" pointer int)))

(define cupedro-set-final-cascade-threshold!
 (c-function void ("cupedro_set_final_cascade_thres" pointer int double)))

(define cupedro-set-global-final-cascade-threshold!
 (c-function void ("cupedro_set_global_final_cascade_thres" pointer double)))

(define cupedro-max-detections
 (c-function int ("cupedro_get_max_results" pointer)))

(define cupedro-set-max-detections!
 (c-function void ("cupedro_set_max_results" pointer int)))

;;; ----------------------------------------------------- Detect/pickup results

(define cupedro-detect!
 ;; Process a single image/frame as follows:
 ;; (cupedro-detect cupedro pointer-to-argb-pixels width height)
 (c-function void ("cupedro_detect" pointer pointer int int)))

(define (cupedro-last-detection-results cupedro model-index)
 ;; Returns the voc4 boxes for the last detection run for model model-index
 (define cupedro-number-of-results
  (c-function int ("cupedro_n_results" pointer int)))
 (define cupedro-result-size
  (c-function int ("cupedro_result_size" pointer int int)))
 ;; Don't call free on the returned pointer -- cupedro manages this memory
 (define cupedro-load-result
  (c-function pointer ("cupedro_load_result" pointer int int)))
 ;; Gets a array in index into a c-double-pointer
 (define (double-ref doubles index)
  (c-double-ref doubles (* index c-sizeof-double)))
 (let ((model-name (cupedro-model-name cupedro model-index)))
  ;; Map across all the results for a given model
  (map-n
   (lambda (result-index)
    (let ((number-of-results
		   (cupedro-result-size cupedro model-index result-index)))
     (if (= 0 number-of-results)
         '()
         (let* ((doubles
				 (cupedro-load-result cupedro model-index result-index))
                (parts (map-n (lambda (index)
                               (double-ref doubles (+ index 4)))
                              (- number-of-results 7))))
          (make-voc4-detection (double-ref doubles 0) (double-ref doubles 1)
                               (double-ref doubles 2) (double-ref doubles 3)
                               (split-into-lists-of-n parts 4)
                               (double-ref doubles (- number-of-results 3))
                               (double-ref doubles (- number-of-results 2))
                               (double-ref doubles (- number-of-results 1))
                               #f
			       model-name)))))
   (cupedro-number-of-results cupedro model-index))))

;;; --------------------------------------------- Example of how to use cupedro

(define (run-cupedro-example video
			     model-files
			     pca-file
			     final-cascade-adjustment
			     threshold-adjustment
			     nms-threshold
			     top-n
			     max-detections)
 ;; An example that shows how to run cupedro across a video file. The following
 ;; snippet of code executes this function
 ;; (let*
 ;;  ((cuda-model-dir
 ;;    "/net/arivu/aux/qobi/video-datasets/C-D1/voc4-models/CUDA-1.1/")
 ;;   (some-models `("person.irobot-felz-model" "bag.irobot-felz-model"))
 ;;   (model-filenames
 ;;    (map (lambda (m) (string-append cuda-model-dir m)) some-models))
 ;;   (video-name
 ;;    "APPROACH7_A1_C1_Act2_URBAN_BR_AFTN_b42e5bae-07b6-11e0-ab3c-e80688cb869a")
 ;;   (video (string->darpa-video-from-corpus video-name "C-D1/recognition")))
 ;;  (run-cupedro-example video
 ;;                       model-filenames
 ;;                       (string-append cuda-model-dir "felz_pca_coeff.csv")
 ;;                       0
 ;;                       0
 ;;                       0
 ;;                       100
 ;;                       30000)))

 (when (null? model-files) (panic "Must specify at least 1 model file"))

 (let ((cupedro (cupedro-new model-files pca-file))
       ;; We marshall the results into the following vector
       (results (make-vector (length model-files))))

  ;; We want to display timing/detect information
  (cupedro-set-quiet! cupedro #f)

  ;; Show the loaded models
  (unless (cupedro-quiet? cupedro)
   (display "Loaded models:")
   (for-each-n
    (lambda (index)
     (display " ")
     (display (cupedro-model-name cupedro index)))
    (cupedro-number-of-models cupedro))
   (newline))

  ;; Set the two thresholds
  (cupedro-set-global-final-cascade-threshold! cupedro final-cascade-adjustment)
  (cupedro-set-global-threshold-adjustment! cupedro threshold-adjustment)

  ;; Set the maximum number of detections
  (cupedro-set-max-detections! cupedro max-detections)

  (unless (cupedro-quiet? cupedro)
   (format #t "Running cuda-felzenszwalb on ~a frames~%" (video-length video))
   (format #t "  Final-cascade-adjustment = ~a~%" final-cascade-adjustment)
   (format #t "  Threshold-adjustment     = ~a~%" threshold-adjustment)
   (format #t "  Non-maximal-suppresion   = ~a~%" nms-threshold)
   (format #t "  Max detections           = ~a~%" max-detections))

  ;; Process each frame of video v1
  (for-each-imlib-frame-from-video-indexed
   (lambda (video-frame-number index imlib-image-handle)
    (imlib-context-set-image! imlib-image-handle)
    (let ((width (imlib-get-image-width))
	  (height (imlib-get-image-height))
	  (raw-data (imlib-get-data-ptr-read-only)))

     ;; Runs the detector
     (unless (cupedro-quiet? cupedro)
      (format #t "frame=~a " index))
     (cupedro-detect! cupedro raw-data width height)

     ;; Be a good citizen
     (imlib-free-image)

     ;; Marshall the results
     (for-each-n
      (lambda (model-index)
       (let ((model-name (cupedro-model-name cupedro model-index))
	     (boxes (top-n-non-maximal-suppression
		     (cupedro-last-detection-results cupedro model-index)
		     nms-threshold
		     top-n)))
	(vector-set! results
		     model-index
		     (cons (take-if-possible
			    top-n
			    (sort boxes > voc4-detection-strength))
			   (vector-ref results model-index)))))
      (cupedro-number-of-models cupedro))))
   video)

  ;; Write the raw boxes files
  (format #t "Detection complete~%")
  (for-each-n
   (lambda (model-index)
    (let ((model-name (cupedro-model-name cupedro model-index))
	  (boxes (reverse (vector-ref results model-index))))
     (format #t "Writing ~a boxes across ~a frames to file: ~a~%"
	     (foldl + (map length boxes) 0)
	     (length boxes)
	     (per-video-box-pathname video "voc4" model-name 1 "boxes"))
     (write-voc4-boxes-movie boxes video model-name)))
   (cupedro-number-of-models cupedro))

  ;; Deallocate cupedro
  (cupedro-delete! cupedro)))
