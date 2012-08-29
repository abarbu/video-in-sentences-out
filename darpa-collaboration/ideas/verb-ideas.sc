;;;
;;; Verb-ideas
;;;
;;; @author Aaron Michaux
;;; @date May, 2011
;;;

;;; (principled-verb-operations) is the main entry point. Perhaps this should be decomposed
;;; into specific functions.


;;; ---------------------------------------------------------------------------- The Model

;;; Parameter-block for "principled-verb-detect" routines
(define-structure detect-model
 name ; Used for generating principled-verb-detect filenames
 lookahead ; Assume the model uses the same lookahead
 video-width video-height ; The assumed width and height of each video
 roc-threshold-fidelity ; Number of values to try when finding an optimal value from a Roc curve 
 required-frames ; The number of required frames for a given action
 percentage-required-frames ; Some actions require a percentage search window
 r-threshold ; pearson-r used as a threshold
 velocity-threshold-stationary ; Below this is a "walk"
 velocity-threshold-walk ; Above this is a "run"
 overlap-threshold ; below this, and two objects are considered on top of each other
 )

;;; Convenience constructor for making a model with the most sensitive parameters changed
(define (make-simple-model lookahead frame-len stationary-v-t run-v-t overlap-t)
 (make-detect-model (format #f "model_lh=~a_fl=~a_sv=~a_rv=~a_olap=~a"
			    lookahead frame-len stationary-v-t run-v-t overlap-t)
		    lookahead
		    1280 720 ; Assumed width and height
		    250   ; Roc-threshold-fidelity
		    frame-len ; required-frames
		    0.4   ; percentage-required-frames
		    0.5   ; pearson-r threshold
		    stationary-v-t run-v-t   ; velocity thresholds
		    overlap-t ; overlap threshold
		    ))

(define *default-detect-model*
 (make-simple-model 7 ; lookahead
		    45 ; required-frames
		    3 8 ; velocity thresholds
		    150)) ; overlap threshold

(define-structure principled-score-data verb best-score arg-names-list scores-list)

;;; To check if read principled-score-data objects are corrupted
(define (is-principled-score-data? score-data)
 (cond
  ((not (vector? score-data)) #f)
  ((not (equal? (vector-ref score-data 0) 'PRINCIPLED-SCORE-DATA)) #f)
  ((not (= (vector-length score-data) 5)) #f)
  (else #t)))

;;; ---------------------------------------------------------------------------- Verb IO
(define (principled-verb-data-files-root)
 "/home/amichaux/public_html/pvd/verbops-data")

;;; The pathname to a verb text-file
(define (principled-verb-pathname video model-name verb)
 (string-append (principled-verb-data-files-root)
		"/scores/" (darpa-video-corpus video) "/" (any-video->string video)
		"/" model-name "/" verb "-scores.sc.gz"))

;;; The pathname to a model-threshold file
(define (principled-model-theshold-pathname corpus model-name verb)
  (string-append (principled-verb-data-files-root)
		 "/thresholds/" corpus "/" model-name "/" verb "-thresholds.sc"))

;;; Returns the video quality rating
;;; (0 => not rated, 1 ==> wtf?, 2 ==> beyond current technology, 3 ==> good)
(define (video-quality video)
 (let* ((filename (human-annotation-video-quality-pathname video))
	(file-data (safe-read-object-from-file filename '())))
  (cond
   ((not (vector? file-data)) 0)
   ((not (= (vector-length file-data) 2))
    (panic "Annotation file ~a corrupted" filename))
   ((not (integer? (vector-ref file-data 1)))
    (panic "Expected #(SYMBOL, integer) in file ~a" filename))
   ((or (< (vector-ref file-data 1) 0) (> (vector-ref file-data 1) 3))
    (panic "Invalid annotation in file ~a" filename))
   (else (vector-ref file-data 1)))))

;;; Writes a score file: (("Run" best-score ((("Person-1") (score1 score2 ...)) (...))) ...)
(define (write-verb-scores model verb video scores score-arg-names)
 (let* ((gz-filename (principled-verb-pathname video (detect-model-name model) verb))
	(best-score (if (null? scores)
			0
			(maximum (map (lambda (v) (maximum (vector->list v))) scores))))
	(verb-data (make-principled-score-data verb best-score score-arg-names scores)))
  (system (format #f "mkdir -p $(dirname \"~a\")" gz-filename))
  (with-temporary-file
   "write-verb-scores"
   (lambda (filename)
    (write-object-to-file verb-data filename)
    (system (format #f "cat ~a | gzip -c > ~a" filename gz-filename))))))

;;; Returns the score-data for a given model-verb-video combination
(define (read-verb-scores model verb video)
 (with-temporary-file
  "read-verb-scores"
  (lambda (filename)
   (let* ((gz-filename (principled-verb-pathname video (detect-model-name model) verb)))
    (system (format #f "cat ~a 2>/dev/null | gunzip -dc 2>/dev/null > ~a" gz-filename filename))
    (safe-read-object-from-file filename '())))))

;;; (read-verb-best-score *default-detect-model* "Run" video) ==> score or '() if there is none
(define (read-verb-best-score model verb video)
 (let ((score-data (read-verb-scores model verb video)))
  (if (null? score-data)
      '()
      (principled-score-data-best-score score-data))))

;;; Saves to file the "best" threshold for a given model-verb
(define (write-to-principled-threshold-file corpus model verb best-threshold roc-score
					    thresholds roc-points)
 (let* ((filename (principled-model-theshold-pathname corpus (detect-model-name model) verb))
	(file-data (safe-read-object-from-file filename '()))
	(mkdir-cmd (format #f "mkdir -p $(dirname \"~a\")" filename)))
  (system mkdir-cmd)
  (write-object-to-file
   (cons (list verb best-threshold roc-score thresholds roc-points)
	 (remove-if (lambda (data) (equal? verb (first data))) file-data))
   filename)))

;;; (read-verb-theshold-file *default-detect-model* "Run")
;;;     ==> ("Run" threshold (list-of-thresholds) (roc-points-for-those-thresholds))
(define (read-verb-threshold-file corpus model verb)
 (let* ((filename (principled-model-theshold-pathname corpus (detect-model-name model) verb))
	(file-data (safe-read-object-from-file filename '()))
	(verb-data (remove-if-not (lambda (data) (equal? verb (first data))) file-data)))
  (if (null? verb-data)
      '()
      (first verb-data))))

;;; (read-verb-best-threshold *default-detect-model* "Run") ==> "0.5"
(define (read-verb-best-threshold corpus model verb)
 (let ((data (read-verb-threshold-file corpus model verb)))
  (if (null? data)
      '()
      (second data))))

;;; (read-verb-best-roc-score *default-detect-model* "Run") ==> "0.5"
(define (read-verb-best-roc-point corpus model verb)
 (let ((data (read-verb-threshold-file corpus model verb)))
  (if (null? data)
      '#(1 0)
      (third data))))

;;; (read-verb-roc-points *default-detect-model* "Run") ==> (#(x y) #(x y) ...)
(define (read-verb-roc-points corpus model verb)
 (let ((data (read-verb-threshold-file corpus model verb)))
  (if (null? data)
      '()
      (fifth data))))


;;; ---------------------------------------------------------------------------- Utilities

;;; Converts *most* scheme objects to json
(define (scheme->json value)
 (cond
  ((null? value) "null")
  ((boolean? value) (if value "true" "false"))
  ((number? value) (format #f "~a" value))
  ((symbol? value) (scheme->json (symbol->string value)))
  ((string? value)
   (format #f "\"~a\""
	   (pregexp-replace* "\"" (pregexp-replace* "\\\\" value "\\\\\\\\") "\\\\\"")))
  ((list? value) (scheme->json (list->vector value)))
  ((vector? value)
   (format #f "[~a]" (reduce-vector (lambda (u v) (format #f "~a, ~a" u (scheme->json v))) value "")))
  (else (panic "Unsupported value type: ~a" value))))

;;; Returns a list of all permutations of subset of "lis" which are length "len"
;;; (permutate '(a b c d) 2)
;;;     ==> ((A B) (A C) (A D) (B A) (B C) (B D) (C A) (C B) (C D) (D A) (D B) (D C))
(define (permutate lis len)
 (let ((len (if (> len (length lis)) 0 len)))
  (let loop ((n 0) (res '()))
   (if (equal? n len)
       res
       (loop (+ n 1)
	     (if (equal? n 0)
		 (map list lis)
		 (remove-if-not identity
				(join (map (lambda (e)
					    (map (lambda (n)
						  (if (member e n) '() (cons e n)))
						 res))
					   lis)))))))))

;;; Returns a list of all permutations of subset of "lis" which are length "len"
;;; (permutate '(a b c d) 2)
;;;     ==> ((C D) (D C) (B D) (D B) (A D) (D A) (B C) (C B) (A C) (C A) (A B) (B A))
(define (permutate-nd lis len)
 (define max-list-len 9)
 (unless (<= (length lis) max-list-len)
  (panic "List length too long for permutate, ~a > ~a" (length lis) max-list-len))
 (all-values
  (let ((p (a-permutation-of (a-subset-of lis))))
   (unless (= (length p) len) (fail))
   p)))


;;; (implode ", " '("a" "b" "c" "d")) ==> "a, b, c, d"
(define (implode glue l)
 (foldl (lambda (u v) (format #f "~a~a~a" u glue v)) (rest l) (first l)))

;;; Divides the list l into n equal portions
(define (equal-portions l n)
 (let* ((len (length l))
	(size (inexact->exact (/ len n)))
	(size-int (if (< size 1) 1 size)))
  (let loop ((src l) (res '()) (counter 0))
   (if (= n counter)
       (reverse res)
       (let* ((src-len (length src))
	      (to-take (if (> size-int src-len) src-len size-int)))
	(loop (drop to-take src) (cons (take to-take src) res) (+ 1 counter)))))))

;; (map-indexed (lambda (elem i) (if (equal? (remainder i 3) 0) elem '())) zzzz)
(define (every-other-nth-o l nth offset)
 (map second
      (remove-if-not
       (lambda (e) (first e))
       (map-indexed (lambda (elem i) (list (if (equal? (remainder i nth) offset) #t #f) elem)) l))))



;;; ---------------------------------------------------------------------------- AD
;;; Slope between two points
(define (slope p1 p2)
 (if (= (x p2) (x p1))
     infinity ; A big number
     (/ (- (y p2) (y p1)) (- (x p2) (x p1)))))

;;; Differentiates fun to find a local minima/maxima between minx and maxx
;;; Recurses a maximum of max-itr.
;;; Yes this code is redundant (in a sense)
(define (ad-line fun minx maxx max-itr)
 (define (fun-w-log x)
  (let ((y (fun x)))
   (display `#(,x ,y)) (newline)
   y))
 (define (ad-line-worker p1 p2 itr max-itr)
 (let* ((mid-x (/ (+ (x p1) (x p2)) 2))
	(mid-p (vector mid-x (fun-w-log mid-x)))
	(s-0 (abs (slope p1 p2)))
        (slope1 (slope p1 mid-p))
	(slope2 (slope mid-p p2)))
  (cond
   ((> itr max-itr) mid-x) ; irregular curve
   ((< (abs slope1) (abs slope2)) (ad-line-worker p1 mid-p (+ itr 1) max-itr))
   ((<= (abs slope2) (abs slope1)) (ad-line-worker mid-p p2 (+ itr 1) max-itr))
   (else (fuck-up)))))
 (ad-line-worker (vector minx (fun-w-log minx)) (vector maxx (fun-w-log maxx)) 0 max-itr))

;;; ---------------------------------------------------------------------------- Graph Utilities

;;; Directs matlab to produce a line-plot, and save it to 'filename'
(define (save-line-plot x y filename)
 (start-matlab!)
 (scheme->matlab! "X" x)
 (scheme->matlab! "Y" y)
 (let ((matlab-cmd
	(format #f
		"figure('visible', 'off'); plot(X, Y); saveas(gcf, '~a'); delete(gcf);"
		filename )))
  (matlab-eval-string matlab-cmd)))

;;; (plot-object-feature-property "person-1" feature-matrix "velocity" 5 "/aux/user/graph")
(define (plot-object-feature-property feature-matrix property-name property-index
				      directory basename)
 (let ((filename (format #f "~a/~a_~a.png" directory basename property-name)))
  (if (file-exists? filename)
      #f ; Nothing to do
      (let* ((XY (remove-if
		  (lambda (XY) (null? (second XY)))
		  (map-n
		   (lambda (n)
		    (let* ((feature (vector-ref feature-matrix n))
			   (Y (if (>= (vector-ref feature 1) 0)
				  (vector-ref feature property-index)
				  '())))
		     (list n Y)))
		   (vector-length feature-matrix)))))
       (unless (> property-index 1)
	(format #t (string-append "Plotting features: " directory))
	(newline))
       (if (> (length XY) 0) (save-line-plot (map first XY) (map second XY) filename))))))

;;; Iterates over single-object features fields, plotting a graph for each
(define (plot-single-object-features feature-matrix directory basename)
 (let ((fields
	'#("position-x" "position-y" "aspect" "aspect-derivative"
	   "velocity" "velocity-orientation" "acceleration" "acceleration-orientation"
	   "area" "area-derivative" "pose-index")))
  (for-each
   (lambda (n)
    (plot-object-feature-property feature-matrix (vector-ref fields n) (+ n 1) directory basename))
   (enumerate (vector-length fields)))))

;;; Iterates over pairwise-object feature fields, plotting a graph for each
(define (plot-pairwise-object-features feature-matrix directory basename)
 (let ((fields '#("center-distance" "center-velocity" "center-orientation")))
  (for-each
   (lambda (n)
    (plot-object-feature-property feature-matrix (vector-ref fields n) (+ n 1) directory basename))
   (enumerate (vector-length fields)))))


;;; Dumps graphs of all the features into directory-name
(define (plot-all-track-features video lookahead directory basename)
 (let ((single-features (all-verb-single-features video lookahead #f))
       (pairwise-features (all-verb-pairwise-features video lookahead #f)))
  (for-each
   (lambda (track-name matrix)
    (plot-single-object-features matrix directory (format #f "~a~a" basename track-name)))
   (map first single-features)
   (map second single-features))
  (for-each
   (lambda (track-name matrix)
    (plot-pairwise-object-features matrix directory (format #f "~a~a" basename track-name)))
   (map first pairwise-features)
   (map second pairwise-features))))

;;; Generates all the graphs for all of the listed verbs for a given corpus
(define (generate-feature-graphs list-of-videos lookahead directory recalculate)
 (for-each 
  (lambda (video)
   (let ((basename (format #f "lh=~a_" lookahead))
	 (video-directory (string-append directory "/" (any-video->string video))))
    (unless (not recalculate) ; Delete existing png files
     (system (format #f "rm -rf ~a/lh=~a_*.png" video-directory lookahead)))
    (system (format #f "mkdir -p ~a" video-directory)) ; Make sure directory exists
    (plot-all-track-features video lookahead video-directory basename))) ; Plot
  list-of-videos))
 
;;; Generates a (fpr,tpr) pair for a list of scores at threshold t,
;;; where matches is a list of #t and #f indicating the positive or negative
;;; state of a particular score
(define (roc-curve-point scores matches t)
 (let* ((n-positive (length (remove-if-not identity matches)))
        (n-negative (- (length matches) n-positive))
        (n-tp (reduce + (map (lambda (s m) (if (and m (>= s t)) 1 0)) scores matches) 0))
        (n-fp (reduce + (map (lambda (s m) (if (and (not m) (>= s t)) 1 0)) scores matches) 0))
	(fpr (if (= n-negative 0) 1 (/ n-fp n-negative)))
	(tpr (if (= n-positive 0) 1 (/ n-tp n-positive))))
  (vector fpr tpr)))

(define (start-roc-plot)
 (start-matlab!)
 (let ((matlab-cmd "figure('visible', 'off'); hold on; axis([0 1 0 1]); plot([0 1], [0 1], '--r.'); xlabel('1 - specificity'); ylabel('sensitivity');"))
  (matlab-eval-string matlab-cmd)))

(define (end-roc-plot filename)
 (matlab-eval-string (format #f "saveas(gcf, '~a'); hold off; delete(gcf);" filename)))

;;; Uses matlab to create an roc-plot, which is saved to filename
(define (save-roc-plot x y filename)
 (scheme->matlab! "X" x)
 (scheme->matlab! "Y" y)
 (start-roc-plot)
 (matlab-eval-string "scatter(X, Y);")
 (end-roc-plot filename))

 ;;; (let ((matlab-cmd (format #f "figure('visible', 'off'); scatter(X, Y); axis([0 1 0 1]); hold on; plot([0 1], [0 1], '--r.'); xlabel('1 - specificity'); ylabel('sensitivity'); saveas(gcf, '~a'); hold off; delete(gcf);" filename)))

;;; Generates a plot for the video-verb score
(define (generate-video-verb-score-plot model video verb videos-dirname . recalc-option)
 (let* ((recalculate (if (not (null? recalc-option)) (equal? #t (first recalc-option))))
	(video-name (any-video->string video))
	(model-name (detect-model-name model))
	(video-verb-score (read-verb-scores model verb video))
	(score-graph-filename (string-append videos-dirname "/"
					     video-name "/score_" verb "_" model-name "_")))
  ;; Lazily recalculate the verb-score if required
  (unless (not (null? video-verb-score)) 
   (principled-verb-detect model video (list verb))
   (set! video-verb-score (read-verb-scores model verb video)))
  ;; We want to create a score-graph
  (for-each
   (lambda (arg-names scores)
    (let* ((filename
	    (string-append score-graph-filename
	                   (reduce (lambda (u v) (string-append u "_" v)) arg-names "")
	                   ".png")))
     (system (format #f "mkdir -p \"$(dirname \"~a\")\"" filename))
     (unless (and (file-exists? filename) (not recalculate))
      (begin
       (let* ((best-threshold (read-verb-best-threshold (darpa-video-corpus video) model verb))
	      (best-score (read-verb-best-score model verb video))
	      (matlab-cmd (format #f
	        "figure('visible', 'off'); line(X, Y); axis([0 ~a 0 ~a]); hold on; plot([0 ~a], [~a ~a], '--r'); saveas(gcf, '~a'); hold off; delete(gcf);"
	        (vector-length scores) best-score
		(vector-length scores) best-threshold best-threshold
		filename)))
	(start-matlab!)
	(scheme->matlab! "X" (enumerate (vector-length scores)))
	(scheme->matlab! "Y" (vector->list scores))
	(format #t "Plotting ~a point score-graph ~a~%" (vector-length scores) filename)
       (matlab-eval-string matlab-cmd))))))
       ;;(save-line-plot (enumerate (vector-length scores)) (vector->list scores) filename))))))
   (principled-score-data-arg-names-list video-verb-score)
   (principled-score-data-scores-list video-verb-score))))

;;; Generates all the graphs for all of the listed verbs for a given corpus
(define (generate-score-graphs corpus model list-of-verbs list-of-videos
			       videos-dirname . recalc-option)
 (let ((recalculate (if (not (null? recalc-option)) (equal? #t (first recalc-option)))))
  (unless (find corpus (corpora))
   (panic "Could not find corpus '~a'. Valid corpora are: ~a." corpus (corpora)))
  (for-each
   (lambda (verb)
    (format #t "Processing verb ~a~%" verb)
    (for-each
     (lambda (video)
      (generate-video-verb-score-plot model video verb videos-dirname recalculate))
     list-of-videos))
   list-of-verbs)))
   
;;; ---------------------------------------------------------------------------- Math Utilities

;;; (linear-regression '(1 2 3) '(3 -2 -1)) ==> (-2 4 -0.7559289460184544)
;;; Which are (in order) (m, c, r) for the formula
;;; y = mx + c, with regression co-efficient 'r'
(define (linear-regression X Y)
 (let* ((X (if (list? X) (list->vector X) X))
	(Y (if (list? Y) (list->vector Y) Y))
	(mean-X (/ (reduce-vector + X 0) (vector-length X)))
	(mean-Y (/ (reduce-vector + Y 0) (vector-length Y)))
	(XX (reduce-vector + (map-vector (lambda (x) (* (- x mean-X) (- x mean-X))) X) 0))
	(YY (reduce-vector + (map-vector (lambda (y) (* (- y mean-Y) (- y mean-Y))) Y) 0))
	(XY (reduce-vector + (map-vector (lambda (x y) (* (- x mean-X) (- y mean-Y))) X Y) 0))
	(m (if (= 0 (abs XX)) infinity (/ XY XX)))
	(c (- mean-Y (* m mean-X)))
	(r (if (or (= 0 (abs XX)) (= 0 (abs YY))) 1 (/ XY (sqrt (* XX YY))))))
  (list m c r)))

;;; Returns a list (or vector) of z-scores for the passed list (or vector) of raw scores
(define (z-scores v)
 (let* ((X (if (list? v) (list->vector v) v))
	(mean-X (/ (reduce-vector + X 0) (vector-length X)))
	(XX (reduce-vector + (map-vector (lambda (x) (* (- x mean-X) (- x mean-X))) X) 0))
	(s (sqrt (/ XX (- (vector-length X) 1))))
	(Z (map-vector (lambda (x) (/ (- x mean-X) s)) X)))
  (if (list? v) (vector->list Z) Z)))

;;; (map-pairs < '(5 4 3 4 3 2)) ==> (#t #t #f #t #t)
;;; i.e.: applies '<' to successive pairs (5 4) (4 3) (3 4) (4 3) (3 2)
(define (map-pairs fun v)
 (let* ((X (if (vector? v) (vector->list v) v))
        (pairs (remove-if (lambda (p) (< (length p) 2)) (map-with-lookahead identity 2 X)))
	(mapped (map fun (map first pairs) (map second pairs))))
  (if (vector? v) (list->vector mapped) mapped)))

;;; (count-pairs < '(5 4 3 4 3 2)) ==> 4
;;; Counts pairs were the predicate 'fun' is true. See map-pairs
(define (count-pairs fun v)
 (length (remove-if-not identity (map-pairs fun v))))

;;; TODO: find and document a fuzzy-and like algorithm
(define (joint-score . scores)
 (apply * scores))

;;; Clamps the passed value between min-value and max-value
(define (clamp value min-value max-value)
 (cond
  ((< value min-value) min-value)
  ((> value max-value) max-value)
  (else value)))

;;; ----------------------------------------------------------- Processing Feature Matricies

;;; Takes a vector of either single-object-features or pairwise-object-features.
;;; Returns TRUE iff the feature-matrix represents a person
(define (person-features? feature-matrix)
 (let* ((f-p (if (vector? feature-matrix) (vector-ref feature-matrix 0) (first feature-matrix))))
  (not (= 0 (single-object-features-pose-index f-p)))))

;;; The pearson-r value for a linear regression across feature-matrix (or list of features).
;;; Accessor is used to derefence each feature to a numeric value.
;;; (feature-pearson-r '(#(SINGLE-OBJECT-FEATURE ....) #(...) ...) single-object-features-velocity)
;;;     ==> -0.2341
(define (feature-pearson-r feature-matrix accessor)
 (let ((feature-matrix (if (list? feature-matrix) (list->vector feature-matrix) feature-matrix)))
  (third (linear-regression
	  (map-vector (lambda (f) (accessor f)) feature-matrix)
	  (enumerate (vector-length feature-matrix))))))

;;; (feature-count feature-matrix single-object-velocity (lambda (n) (> n 4)))
;;;     ==> number of feature matrix frames where (velocity > 4)
;;; Run can return a boolean, or numeric expression
(define (feature-count feature-matrix accessor fun)
 (let ((feature-matrix (if (list? feature-matrix) (list->vector feature-matrix) feature-matrix)))
  (reduce-vector +
		 (map-vector
		  (lambda (n) (let ((res (fun n))) (if (boolean? res) (if res 1 0) res)))
		  (map-vector accessor feature-matrix))
		 0)))

;;; Counts the number of features in the feature-matrix (accessed via accessor)
;;; (feature-count-threshold feature-matrix single-object-features-velocity > 0.5)
;;;     ==> the number features (in feature-matrix) with velocity > 0.5
(define (feature-count-threshold feature-matrix accessor op threshold)
 (feature-count feature-matrix accessor (lambda (n) (op n threshold))))

;;; Calls count-pairs on feature-matrix by accessor. See count-pairs
(define (feature-count-pairs feature-matrix accessor op) 
 (count-pairs op ((if (vector? feature-matrix) map-vector map) accessor feature-matrix)))

;;; ---------------------------------------------------------------------------- Scoring Utils
(define (su-calc-window-size-percent model n-frames)
 (inexact->exact (* n-frames (detect-model-percentage-required-frames model))))

;;; Scores how well a line stays is below (op <), above (op >) or equal to (op =) a given threshold 
(define (su-line-threshold points op threshold window-size)
 (let* ((n-threshold (feature-count-threshold points identity op threshold))
	(percent-threshold (/ n-threshold window-size)))
  (sqr percent-threshold)))

;;; How much bigger (see below) (first points) is than (last points), adjusting for the model size
;;; Pass < for op if you want smaller, and > for bigger.
(define (su-magnitude-change points model op)
 (let* ((v-width (detect-model-video-width model))
        (v-height (detect-model-video-height model))
        (diagonal (sqrt (+ (sqr v-width) (sqr v-height))))
        (f-p (first points))
        (l-p (last points))
        (distance (if (equal? op <) (- l-p f-p) (- f-p l-p))))
  (cond
   ((< distance 0) 0)
   ((> distance diagonal) 1)
   (else (/ distance diagonal)))))

;;; Scores how well a line decreases from left-to right
(define (su-line-decrease points window-size)
 (if (null? points)
     0
     (let*
       ((first-d (first points))
	(last-d (last points))
	(n-decreasing (count-pairs > points)))
      (if (> last-d first-d)
	  0
	  (sqr (/ n-decreasing window-size))))))

;;; Scores how well a line increases from left-to right
(define (su-line-increase points window-size)
 (su-line-decrease (reverse points) window-size))
 
;;; Scores how well a line decreases for first half of points, then is below threshold for remainder
(define (su-line-decrease-then-below-threshold points threshold window-size)
 (let ((n-points (length points)))
  (if (< n-points 4) ;; we should have at least 4 poits for this algorithm
      0
      (let* ((half-n (inexact->exact (/ n-points 2)))
	     (half-window-size (/ window-size 2))
	     (first-half (take half-n points))
	     (second-half (drop half-n points))
	     (decrease-score (su-line-decrease first-half half-window-size))
	     (stationary-score (su-line-threshold second-half < threshold half-window-size)))
       (* decrease-score stationary-score)))))

;;; Scores how well a line is stationary for first half, and then increases for the remainder
(define (su-line-below-threshold-then-increase points threshold window-size)
 (su-line-decrease-then-below-threshold (reverse points) threshold window-size))

;;; Scores how well line is above threshold, and then quickly drops off
(define (su-line-cliff points model threshold window-size)
 (let* ((n-points (length points))
        (cliff-portion 6)
	(window-cliff-n (inexact->exact (/ window-size cliff-portion)))
	(window-stationary (- window-size window-cliff-n)))
  (if (< n-points 16) ;; we should have at least 16 poits for this algorithm
      0
      (let* ((cliff-n (inexact->exact (/ n-points cliff-portion)))
	     (stationary-n (- n-points cliff-n))
	     (first-part (take stationary-n points))
	     (second-part (drop stationary-n points))
	     (decrease-score (su-line-decrease second-part window-cliff-n))
	     (stationary-score (su-line-threshold first-part > threshold window-stationary)))
       (* decrease-score stationary-score (su-magnitude-change points model >))))))

;;; Scores how well the line quickly jumps up, and then stays above threshold
(define (su-cliff-then-line points model threshold window-size)
 (su-line-cliff (reverse points) model threshold window-size))

;;; Scores how well line is below threshold and then quickly rises
(define (su-line-flat-then-wall points model threshold window-size)
 (let ((inv-points (map (lambda (n) (* n -1)) points))
       (inv-threshold (* threshold -1)))
  (su-line-cliff inv-points model inv-threshold window-size)))

;;; Scores how well the line quilly drops and then stays below threshold
(define (su-line-drop-then-flat points model threshold window-size)
 (su-line-flat-then-wall (reverse points) model threshold window-size))

;;; Scores how v-shaped a set of points are
(define (su-line-is-v-shaped points window-size)
 (let* ((n-points (length points))
	(half-n (inexact->exact (/ n-points 2)))
	(first-half (take half-n points))
	(second-half (drop half-n points)))
  (* (su-line-decrease first-half window-size) (su-line-increase second-half window-size))))

;;; The complement of su-single-features-stable
(define (su-single-features-unstable feature-matrix)
 (- 1 (su-single-features-stable feature-matrix)))

;;; The passed single features is unstable
(define (su-single-features-stable feature-matrix)
 (let* ((feature-matrix (if (list? feature-matrix) (list->vector feature-matrix) feature-matrix))
	(velocity-score (su-feature-stable feature-matrix single-object-features-velocity))
	(aspect-score (su-feature-stable feature-matrix single-object-features-aspect))
	(x-pos-score (su-feature-stable feature-matrix single-object-features-position-x))
	(y-pos-score (su-feature-stable feature-matrix single-object-features-position-y)))
  (joint-score velocity-score aspect-score x-pos-score y-pos-score)))

;;; Returns if a given variable (accessed by accessor) has gradient close to 0
;;; and a fairly stable pearson-r
(define (su-feature-stable feature-matrix accessor)
 (let* ((feature-matrix (if (list? feature-matrix) (list->vector feature-matrix) feature-matrix))
	(feature (map-vector (lambda (f) (accessor f)) feature-matrix))
	(regression (linear-regression feature (enumerate (vector-length feature-matrix))))
	(gradient (first regression))
	(abs-angle (abs (atan gradient)))
	;; Gives how many decimal places of '9'
	(la (if (equal? 0 abs-angle) 10 (log (/ (/ pi 2) abs-angle))))
	(la (if (> la 10) 10 la))
	(integer (inexact->exact la))
	(decimal (- la integer))
	(zero-score (/ (+ (- (expt 10 integer) 1) decimal) (expt 10 integer)))
	(pearson-r (third regression)))
  (* zero-score (abs pearson-r))))

;;; How close does the feature come to the edge of the frame
(define (su-features-reach-video-edge model feature)
 (if (equal? 0 (length feature))
     0
     (let* ((video-w (detect-model-video-width model))
	    (video-h (detect-model-video-height model))
	    (x-pos (map single-object-features-position-x feature))
	    (y-pos (map single-object-features-position-y feature))
	    (max-x (maximum x-pos))
	    (max-y (maximum y-pos))
	    (min-x (minimum x-pos))
	    (min-y (minimum y-pos))
	    (best-x (minimum (list (- video-w max-x) min-x)))
	    (best-y (minimum (list (- video-h max-y) min-y)))
	    (best-x-score (/ (- video-w best-x) video-w))
	    (best-y-score (/ (- video-h best-y) video-h)))
      (maximum (list best-x-score best-y-score)))))

;;; Returns 1 iff A starts to the left of P and finishes on the right (or reverse). 0 otherwise
(define (su-crossover a-pos-x p-pos-x)
 (let ((a-pos-x-1 (first a-pos-x))
       (a-pos-x-n (last a-pos-x))
       (p-pos-x-1 (first p-pos-x))
       (p-pos-x-n (last p-pos-x)))
  (if (or (and (< a-pos-x-1 p-pos-x-1) (> a-pos-x-n p-pos-x-n))
	  (and (> a-pos-x-1 p-pos-x-1) (< a-pos-x-n p-pos-x-n)))
      1
      0)))

;;; Returns the average overlap in pairwise-feature-matrix
;;(su-line-threshold centers < overlap-threshold window-size)))
(define (su-overlap model a-p window-size)
 (/ (reduce +
	    (map
	     (lambda(f)
	      (if (< (pairwise-object-features-overlap f) 0)
		  0
		  (pairwise-object-features-overlap f)))
	     a-p)
	    0)
    window-size))

;;; ---------------------------------------------------------------------------- Score-null
(define (score-null model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Approach
(define (score-approach model actor patient actor-patient)
 (let* ((window-size (su-calc-window-size-percent model (vector-length actor))))
  (map-vector-with-lookahead
   (lambda (a p a-p) ; actor patient actor-patient
    (let* ((centers (map pairwise-object-features-center-distance a-p))
	   (decrease-score (su-line-decrease centers window-size))
	   (magnitude-score (su-magnitude-change centers model >)))
     (joint-score magnitude-score decrease-score)))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Arrive
(define (score-arrive model actor)
 (let* ((window-size (* 3 (detect-model-required-frames model)))
	(stationary-v (detect-model-velocity-threshold-stationary model)))
  (map-vector-with-lookahead
   (lambda (a) ; actor
    (if (not (person-features? a)) 
	0 ; must be a person
	(su-line-decrease-then-below-threshold
	 (map single-object-features-velocity a) stationary-v window-size)))
   window-size actor)))

;;; ---------------------------------------------------------------------------- Score-Attach
(define (score-window-attach model window-size a p a-p)
 (if (or (not (person-features? a)) (person-features? p))
     0 ; 'a' must be a person, and 'p' must be an object
     (let*
       ((half-window (inexact->exact (/ window-size 2)))
	(n-points (length a))
	(half-n (inexact->exact (/ n-points 2)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	;; Data
	(centers (map pairwise-object-features-center-distance a-p))
	(person-velocity (map single-object-features-velocity a))
	(object-velocity (map single-object-features-velocity p))
	(person-stationary-score (su-line-threshold person-velocity < stationary-v window-size))
	(object-stationary-score (su-line-threshold object-velocity < stationary-v window-size))
	;; Scores
	(come-together-score
	 (su-line-decrease-then-below-threshold centers overlap-threshold window-size))
	(overlap-score (su-overlap model a-p window-size))
	(person-move-score
	 (su-line-decrease-then-below-threshold person-velocity stationary-v window-size))
	(all-stationary-score ; 'a' and 'p' are stationary and overlap
	 (joint-score person-stationary-score object-stationary-score overlap-score))
	(move-attach-score ; 'a' moves to 'p' and then overlap and are both stationary
	 (joint-score object-stationary-score come-together-score person-move-score)))
      (max all-stationary-score move-attach-score))))

(define (score-attach model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-attach model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Bounce
(define (score-bounce model actor)
 ;; The tracks show no ossicilations
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Bury
(define (score-bury model actor patient actor-patient)
 (let* ((window-size (su-calc-window-size-percent model (vector-length actor)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model)))
  (map-vector-with-lookahead
   (lambda (a p a-p) ; actor patient actor-patient
    (if (or (not (person-features? a)) (person-features? p))
	0 ; 'a' must be a person, and 'p' must be an object
	(let*
	  ((pose-points (map single-object-features-pose-index a))
	   (object-velocity (map single-object-features-velocity p))
	   (centers (map pairwise-object-features-center-distance a-p))
	   (crouch-pose (su-line-threshold pose-points = 2 window-size))
	   (object-stationary-score (su-line-threshold object-velocity < stationary-v window-size))
	   (overlap-score (su-overlap model a-p window-size)))
	 (joint-score overlap-score object-stationary-score crouch-pose))))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Carry
(define (score-carry model actor patient actor-patient)
 (let* ((window-size (su-calc-window-size-percent model (vector-length actor)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model)))
  (map-vector-with-lookahead
   (lambda (a p a-p) ; actor patient actor-patient
    (if (or (not (person-features? a)) (person-features? p))
	0 ; 'a' must be a person, and 'p' must be an object
	(let*
	  ((centers (map pairwise-object-features-center-distance a-p))
	   (center-velocity (map pairwise-object-features-center-velocity a-p))
	   (overlap-score (su-overlap model a-p window-size))
	   (center-decrease-stationary-score
	    (su-line-decrease-then-below-threshold center-velocity stationary-v window-size))
	   (center-decrease-score (su-line-decrease center-velocity window-size))
	   (center-increase-score (su-line-increase center-velocity window-size))
	   (center-stationary-increase-score
	    (su-line-below-threshold-then-increase center-velocity stationary-v window-size)))
	 (joint-score overlap-score
		       (max center-increase-score center-decrease-score
			    center-stationary-increase-score center-decrease-stationary-score)))))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Catch
(define (score-catch model actor patient actor-patient)
 (let* ((window-size (* 2 (detect-model-required-frames model)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(max-walk-v (detect-model-velocity-threshold-walk model))
	(overlap-threshold (detect-model-overlap-threshold model)))
  (map-vector-with-lookahead
   (lambda (a p a-p) ; actor patient actor-patient
    (if (or (not (person-features? a)) (person-features? p))
	0 ; 'a' must be a person, and 'p' must be an object
	(let*
	  ((object-velocity (map single-object-features-velocity p))
	   (person-velocity (map single-object-features-velocity a))
	   (centers (map pairwise-object-features-center-distance a-p))
	   (center-velocity (map pairwise-object-features-center-velocity a-p))
	   (overlap-score (su-overlap model a-p window-size))
	   (center-decrease-stationary-score
	    (su-line-decrease-then-below-threshold center-velocity stationary-v window-size))
	   (object-decrease-stationary-score
	    (su-line-decrease-then-below-threshold object-velocity stationary-v window-size))
	   (object-starts-fast-score (su-line-threshold object-velocity > max-walk-v window-size))
	   (person-starts-slow-score (su-line-threshold object-velocity < stationary-v window-size)))
	 (joint-score overlap-score center-decrease-stationary-score
		       object-decrease-stationary-score
		       object-starts-fast-score person-starts-slow-score))))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Chase
(define (score-chase model actor patient actor-patient)
 (let* ((window-size (* 3(detect-model-required-frames model)))
	(overlap-threshold (detect-model-overlap-threshold model))
	(not-overlap-scores
	 (map-vector-with-lookahead
	  (lambda (a p a-p) ; actor patient actor-patient
	   (if (or (not (person-features? a)) (not (person-features? p)))
	       0 ; both 'a' and 'b' must be people
	       (- 1 (su-overlap model a-p window-size))))
	  window-size actor patient actor-patient)))
  (map-vector joint-score not-overlap-scores (score-run model actor) (score-run model patient))))

;;; ---------------------------------------------------------------------------- Score-Close
(define (score-close model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Collide
(define (score-collide model actor patient actor-patient)
 (let* ((window-size (* 2 (detect-model-required-frames model)))
	(half-window (/ window-size 2))
	(overlap-threshold (detect-model-overlap-threshold model))
	(stationary-v (detect-model-velocity-threshold-stationary model)))  
  (map-vector-with-lookahead
   (lambda (a p a-p) ; actor patient actor-patient
    (if (or (< (length a) 16))
	0 ; (we must also have 16 points)
	(let* ((half-n (inexact->exact (/ (length a) 2)))
	       (actor-velocity (map single-object-features-velocity (take half-n a)))
	       (patient-velocity (map single-object-features-velocity (take half-n p)))
	       (centers (map pairwise-object-features-center-distance a-p))
	       (actor-cliff-score (su-line-cliff actor-velocity model stationary-v half-window))
	       (patient-cliff-score (su-line-cliff patient-velocity model stationary-v half-window))
	       (center-v-score (su-line-is-v-shaped centers window-size)))
	 (joint-score center-v-score patient-cliff-score actor-cliff-score))))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Dig
(define (score-dig model actor patient actor-patient)
 (let* ((window-size (su-calc-window-size-percent model (vector-length actor)))
	(overlap-threshold (detect-model-overlap-threshold model))
	(stationary-v (detect-model-velocity-threshold-stationary model)))  
  (map-vector-with-lookahead
   (lambda (a p a-p) ; actor patient actor-patient
    (if (or (not (person-features? a)) (person-features? p))
	0 ; 'a' must be a person, and 'p' must be an object
	(let*
	  ((actor-velocity (map single-object-features-velocity a))
	   (patient-velocity (map single-object-features-velocity p))
	   (centers (map pairwise-object-features-center-distance a-p))
	   (center-velocity (map pairwise-object-features-center-velocity a-p))
	   (overlap-score (su-overlap model a-p window-size))
	   (actor-stationary-score (su-line-threshold actor-velocity < stationary-v window-size))
	   (patient-stationary-score (su-line-threshold patient-velocity < stationary-v window-size))
	   (center-stationary-score (su-line-threshold center-velocity < stationary-v window-size)))
	 (joint-score overlap-score actor-stationary-score
		       patient-stationary-score center-stationary-score))))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Drop
(define (score-window-drop model window-size a p a-p)
 (if (or (not (person-features? a)) (person-features? p) (< (length a) 16))
     0 ; 'a' must be a person, and 'p' must be an object
     (let*
       ((third-window (inexact->exact (/ window-size 3)))
	(overlap-threshold (detect-model-overlap-threshold model))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(third-n (inexact->exact (/ (length a) 3)))
	;; data
	(a-parts (equal-portions a 3))
	(p-parts (equal-portions p 3))
	(a-p-parts (equal-portions a-p 3))
	(p-velocity-parts (equal-portions (map single-object-features-velocity p) 3))
	(centre-parts (equal-portions (map pairwise-object-features-center-distance a-p) 3))
	(p-y-pos-2nd-third (map single-object-features-position-y (second p-parts)))
	(p-y-2nd-third-1 (first p-y-pos-2nd-third))
	(p-y-2nd-third-n (last p-y-pos-2nd-third))
	;; a is stable throughout
	(a-stable-score (su-single-features-stable a))
	;; Centres overlap for the 1st third of the window
	(olap-1st-third-score (su-overlap model (first a-p-parts) third-window))
	;; p stable, then unstable, then stable
	(p-stable-unstable-stable-score
	 (joint-score (su-single-features-stable (first p-parts))
		      (su-single-features-unstable (second p-parts))
		      (su-single-features-stable (third p-parts))))
	;; y position decreases in 2nd-third
	(y-pos-decrease-score (if (> p-y-2nd-third-1 p-y-2nd-third-n) 0 1)))
      (joint-score a-stable-score olap-1st-third-score p-stable-unstable-stable-score
		   y-pos-decrease-score))))

(define (score-drop model actor patient actor-patient)
 (let ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-drop model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Enter
(define (score-enter model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Exchange
(define (score-exchange model actor patient other actor-patient actor-other patient-other)
 ;; There is no difference between give and exchange, except that with exchange we
 ;; give in each direction. This would require /four/ tracks (2 people, 2 objects),
 ;; and is beyond what is possible
 (score-give model actor patient other actor-patient actor-other patient-other))

;;; ---------------------------------------------------------------------------- Score-Exit
(define (score-exit model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Fall
(define (score-window-fall model window-size a)
 (if (or (not (person-features? a)) (< (length a) 4))
     0 ; 'a' must be a person (and we must also have 4 points) 
     (let*
       ((half-window (inexact->exact (/ window-size 2)))
	(pose-is-flat 3)
	(pose-is-standing 1)
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(half-n (inexact->exact (/ (length a) 2)))
	;; data
	(velocity-1st-half (map single-object-features-velocity (take half-n a)))
	(velocity-2nd-half (map single-object-features-velocity (drop half-n a)))
	(pose-1st-half (map single-object-features-pose-index (take half-n a)))
	(pose-2nd-half (map single-object-features-pose-index (drop half-n a)))
	;; scores
	(first-half-cliff-score (su-line-cliff velocity-1st-half model stationary-v half-window))
	(first-half-flatwall-score
	 (su-line-flat-then-wall velocity-1st-half model stationary-v half-window))
	(second-half-dropflat-score
	 (su-line-drop-then-flat velocity-2nd-half model stationary-v half-window))
	(first-half-still-score (su-line-threshold velocity-1st-half < stationary-v half-window))
	(second-half-still-score (su-line-threshold velocity-2nd-half < stationary-v half-window))
	(first-half-pose-score (su-line-threshold pose-1st-half = pose-is-standing half-window))
	(second-half-pose-score (su-line-threshold pose-2nd-half = pose-is-flat half-window)))
      (- 1 (joint-score first-half-pose-score second-half-pose-score
		   (max
		    (joint-score first-half-cliff-score second-half-still-score)
		    (* 0.5
		       (joint-score first-half-flatwall-score second-half-dropflat-score))))))))

(define (score-fall model actor) 
 (let* ((window-size (* 4 (detect-model-required-frames model))))  
  (map-vector-with-lookahead
   (lambda (a) (score-window-fall model window-size a))
   window-size actor)))

;;; ---------------------------------------------------------------------------- Score-Flee
(define (score-window-flee model window-size a p a-p)
 (if (or (not (person-features? a)) (< (length a) 4))
     0 ; 'a' must be a person
     (let*
       ((half-window (inexact->exact (/ window-size 2)))
	(overlap-threshold (detect-model-overlap-threshold model))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(max-walk-threshold (detect-model-velocity-threshold-walk model))
	(half-n (inexact->exact (/ (length a) 2)))
	;; data
	(centres-1st-half (map pairwise-object-features-center-distance (take half-n a-p)))
	(centres-2nd-half (map pairwise-object-features-center-distance (drop half-n a-p)))
	(a-velocity (map single-object-features-velocity a))
	(p-velocity (map single-object-features-velocity p))
	;; a-p overlap in first half
	(a-p-olap-score-1
	 (su-overlap model (take half-n a-p) half-window))
	;; a-p do not overlap in 2nd half
	(a-p-olap-score-2
	 (su-overlap model (drop half-n a-p) half-window))
	(a-p-nolap-score-2 (- 1 a-p-olap-score-2))
	;; p is stationary throughout
	(p-stationary-score (su-line-threshold p-velocity < stationary-v window-size))
	;; a starts stationary and then moves
	(a-stationary-then-increase-score
	 (su-line-below-threshold-then-increase a-velocity stationary-v window-size))
	;; a is running in 2nd half
	(pearson-r
	 (feature-pearson-r (drop half-n a) single-object-features-velocity-orientation))
	(run-threshold-score
	 (su-line-threshold (drop half-n a-velocity) > max-walk-threshold half-window)))
      (joint-score a-p-olap-score-1 a-p-nolap-score-2 p-stationary-score
		   a-stationary-then-increase-score pearson-r run-threshold-score))))

(define (score-flee model actor patient actor-patient)
 (let ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-flee model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Fly
(define (score-fly model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Follow
(define (score-window-follow model window-size a p a-p)
 (if (< (length a) 4)
     0 
     (let*
       ((overlap-threshold (detect-model-overlap-threshold model))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	;; data
	(centres (map pairwise-object-features-center-distance a-p))
	(a-velocity (map single-object-features-velocity a))
	(p-velocity (map single-object-features-velocity p))
	;; Halve the score if (person-features? a) != (person-features? b)
	(score-same-type (if (equal? (person-features? a) (person-features? p)) 1 0.5))
	;; Is a and p overlapping (or close)
	(a-p-olap-score (su-overlap model a-p window-size))
	;; p is moving throughout
	(p-stationary-score (su-line-threshold p-velocity > stationary-v window-size))
	;; a is moving throughout
	(a-stationary-score (su-line-threshold a-velocity > stationary-v window-size)))
      (joint-score score-same-type a-p-olap-score p-stationary-score a-stationary-score))))

(define (score-follow model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-follow model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Get
(define (score-window-get model window-size a p a-p)
 (if (or (not (person-features? a)) (person-features? p) (< (length a) 16))
     0 ; 'a' must be a person, and 'p' must be an object
     (let*
       ((third-window (inexact->exact (/ window-size 3)))
	(third-n (inexact->exact (/ (length a) 3)))
	(overlap-threshold (detect-model-overlap-threshold model))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	;; Data
	(a-p-parts (equal-portions a-p 3))
	(a-vel-parts (equal-portions (map single-object-features-velocity a) 3))
	(p-vel-parts (equal-portions (map single-object-features-velocity p) 3))
	;; Is a and p not overlapping in 1st-third 
	(a-p-not-olap-1st-third-score (- 1 (su-overlap model (first a-p-parts) third-window)))
	;; Is a and p overlapping (or close) in 2nd-third
	(a-p-olap-2nd-third-score (su-overlap model (second a-p-parts) third-window))
	;; p is stationary in 1st-third
	(p-stationary-1st-third-score
	 (su-line-threshold (first p-vel-parts) < stationary-v third-window))
	;; a is stationary in 1st-third (clamped 0.5 1)
	(a-moving-1st-third-score
	 (clamp (su-line-threshold (first a-vel-parts) > stationary-v third-window) 0.5 1))
	(a-stationary-2nd-third-score
	 (clamp (su-line-threshold (second a-vel-parts) < stationary-v third-window) 0.5 1))
	(a-moving-3rd-third-score
	 (clamp (su-line-threshold (third a-vel-parts) > stationary-v third-window) 0.5 1)))
      (joint-score a-p-not-olap-1st-third-score a-p-olap-2nd-third-score
		   p-stationary-1st-third-score a-moving-1st-third-score
		   a-stationary-2nd-third-score a-moving-3rd-third-score))))
 
(define (score-get model actor patient actor-patient)
 (let* ((window-size (* 8 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-get model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Give
(define (score-window-give model window-size a p o a-p a-o p-o)
 (if (or (not (person-features? a)) (not (person-features? p))
	 (person-features? o) (< (length a) 4))
	0 ; 'a' and 'p' must be people, and 'o' must be an object
	(let*
	  ((half-window (inexact->exact (/ window-size 2)))
	   (overlap-threshold (detect-model-overlap-threshold model))
	   (stationary-v (detect-model-velocity-threshold-stationary model))
	   (half-n (inexact->exact (/ (length a) 2)))
	   ;; Data
	   (a-o-parts (equal-portions a-o 2))
	   (p-o-parts (equal-portions p-o 2))
	   (centers-a-p (map pairwise-object-features-center-distance a-p))
	   (a-velocity (map single-object-features-velocity a))
	   (p-velocity (map single-object-features-velocity p))
	   ;; Scores
	   (a-stationary-score (su-line-threshold a-velocity < stationary-v window-size))
	   (p-stationary-score (su-line-threshold p-velocity < stationary-v window-size))
	   ;; Overlap score, a-o, 1st half (etc.)
	   (a-o-olp-score-1 (su-overlap model (first a-o-parts) half-window))
	   (a-o-olp-score-2 (su-overlap model (second a-o-parts) half-window))
	   (p-o-olp-score-1 (su-overlap model (first p-o-parts) half-window))
	   (p-o-olp-score-2 (su-overlap model (second p-o-parts) half-window))
	   ;; The maximum of overlap between (a-o-1 and p-o-2) and (p-o-1 and a-o-2)
	   ;; As in, the object is passed from a=>p or p=>a
	   (a-o-p-olp-score
	    (maximum (list (joint-score a-o-olp-score-1 p-o-olp-score-2 (- 1 a-o-olp-score-2))
			   (joint-score p-o-olp-score-1 a-o-olp-score-1 (- 1 p-o-olp-score-2))))))
	 ;; 'a' and 'p' are stationary, and 'o' switches between them
	 (joint-score a-stationary-score p-stationary-score a-o-p-olp-score))))

(define (score-give model actor patient other actor-patient actor-other patient-other) 
 (let* ((window-size (* 3 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p o a-p a-o p-o) (score-window-give model window-size a p o a-p a-o p-o))
   window-size actor patient other actor-patient actor-other patient-other)))

;;; ---------------------------------------------------------------------------- Score-Go
(define (score-window-go model window-size a)
 (if (< (length a) 4)
     0 ; Must have 4 frames of data
     (let*
       ((stationary-v (detect-model-velocity-threshold-stationary model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	(half-window (inexact->exact (/ window-size 2)))
	(half-n (inexact->exact (/ n-frames 2)))
	;; Data
        (a-velocity (map single-object-features-velocity a))
	;; Scores
	(unstable-2nd-half (su-single-features-unstable (drop half-n a)))
	(stationary-1st-half
	 (su-line-threshold (take half-n a-velocity) < stationary-v half-window))
	(moving-2nd-half
	 (su-line-threshold (drop half-n a-velocity) < stationary-v half-window)))
      (joint-score unstable-2nd-half stationary-1st-half moving-2nd-half))))

(define (score-go model actor)
 (let* ((window-size (* 2 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a) (score-window-go model window-size a))
   window-size actor)))

;;; ---------------------------------------------------------------------------- Score-Hand
(define (score-window-hand model window-size a p a-p)
 (if (or (not (person-features? a)) (not (person-features? p)) (< (length a) 16))
     0 ; 'a' and 'p' must both be people, and must have 16 frames of data
     (let*
       ((stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	(third-window (inexact->exact (/ n-frames 3)))
	;; Data
        (a-velocity (map single-object-features-velocity a))
	(p-parts (equal-portions p 3))
	(a-p-parts (equal-portions a-p 3))
	;; Scores
	(a-stationary-score (su-line-threshold a-velocity < stationary-v window-size))
	(p-moving-1st-third
	 (su-line-threshold (map single-object-features-velocity (first p-parts)) >
			    stationary-v third-window))
	(p-moving-3rd-third
	 (su-line-threshold (map single-object-features-velocity (third p-parts)) >
			    stationary-v third-window))
	(a-p-olap-2nd-third (su-overlap model (second a-p-parts) third-window)))
      (joint-score a-stationary-score p-moving-1st-third p-moving-3rd-third a-p-olap-2nd-third))))

(define (score-hand model actor patient actor-patient)
 (let* ((window-size (* 8 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-hand model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Haul
(define (score-window-haul-push model window-size a p a-p is-haul)
 (if (or (not (person-features? a)) (person-features? p) (< (length a) 4))
     0 ; 'a' must be a person, and 'p' and object
     (let*
       ((stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	;; Data
        (a-velocity (map single-object-features-velocity a))
	(p-velocity (map single-object-features-velocity a))
	(a-p-centres (map pairwise-object-features-center-distance a-p))
	(a-pos-x (map single-object-features-position-x a))
	(p-pos-x (map single-object-features-position-x p))
	(a-pos-y (map single-object-features-position-y a))
	(p-pos-y (map single-object-features-position-y p))
	(a-x-minus-p-x (map (lambda (u v) (- u v)) a-pos-x p-pos-x))
	(a-y-minus-p-y (map (lambda (u v) (- u v)) a-pos-y p-pos-y))
	(moving-left-right (if (< (first a-pos-x) (last a-pos-x)) #t #f))
	;; Scores
	(a-moving-score (su-line-threshold a-velocity > stationary-v window-size))
	(p-moving-score (su-line-threshold p-velocity > stationary-v window-size))
	(a-p-olap-score (su-overlap model a-p window-size))
	;; If a is more than 50% above p, then we give a bonus score
        (a-above-p-score (maximum (list 0.5 (su-line-threshold a-y-minus-p-y > 0 window-size))))
	;; Is the a 'ahead' or p
	(a-leads-p-score
	 (su-line-threshold a-x-minus-p-x (if moving-left-right > <) 0 window-size)))
      (joint-score a-moving-score p-moving-score a-p-olap-score a-above-p-score
		   (if is-haul a-leads-p-score (- 1 a-leads-p-score))))))

(define (score-haul model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-haul-push model window-size a p a-p #t))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Have
(define (score-have model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Hit
(define (score-hit model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Hold
(define (score-window-hold model window-size a p a-p)
 (if (or (not (person-features? a)) (person-features? p) (< (length a) 4))
     0 ; 'a' must be a person, and 'p' and object
     (let*
       ((stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	;; Data
        (a-velocity (map single-object-features-velocity a))
	(a-p-centres (map pairwise-object-features-center-distance a-p))
	(a-pos-y (map single-object-features-position-y a))
	(p-pos-y (map single-object-features-position-y p))
	(a-y-minus-p-y (map (lambda (u v) (- u v)) a-pos-y p-pos-y))
	;; Scores
	(a-stationary-score (su-line-threshold a-velocity < stationary-v window-size))
	(a-p-olap-score (su-overlap model a-p window-size))
	;; If a is more than 50% above p, then we give a bonus score
        (a-above-p-score (maximum (list 0.5 (su-line-threshold a-y-minus-p-y > 0 window-size)))))
      (joint-score a-stationary-score a-p-olap-score a-above-p-score))))

(define (score-hold model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-hold model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Jump
(define (score-jump model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Kick
(define (score-window-kick model window-size a p a-p)
 (if (or (not (person-features? a)) (< (length a) 16))
     0 ; 'a' must be a person
     (let*
       ((half-window (inexact->exact (/ window-size 2)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	(half-n (inexact->exact (/ n-frames 2)))
	;; Data
        (a-velocity (map single-object-features-velocity a))
	(p-velocity (map single-object-features-velocity p))
	(a-p-centres (map pairwise-object-features-center-distance a-p))
	(a-pos-y (map single-object-features-position-y a))
	(p-pos-y (map single-object-features-position-y p))
	(a-y-minus-p-y (map (lambda (u v) (- u v)) a-pos-y p-pos-y))
	;; Scores
	(a-stationary-score (su-line-threshold a-velocity < stationary-v window-size))
	(p-stationary-1st-half-score
	 (su-line-threshold (take half-n p-velocity) < stationary-v half-window))
	(p-stable-1st-half-score (su-single-features-stable (take half-n p)))
	(p-unstable-2nd-half-score (su-single-features-unstable (drop half-n p)))
	(a-p-olap-score (su-overlap model a-p window-size))
	;; If a is more than 50% above p, then we give a bonus score
        (a-above-p-score (maximum (list 0.5 (su-line-threshold a-y-minus-p-y > 0 window-size)))))
      (joint-score a-stationary-score p-stationary-1st-half-score
		   p-stable-1st-half-score p-unstable-2nd-half-score
		   a-p-olap-score a-above-p-score))))

(define (score-kick model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-kick model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Leave
(define (score-window-leave model window-size a)
 (if (< (length a) 20)
     0 ; Must have 20 frames of data
     (let*
       ((half-window (inexact->exact (/ window-size 2)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
        (half-n (inexact->exact (/ n-frames 2)))
	;; Data
	(a-parts (equal-portions a 2))
        (a-velocity-1st-half (map single-object-features-velocity (first a-parts)))
	(a-velocity-2nd-half (map single-object-features-velocity (second a-parts)))
	;; Scores
	(move-score
	 (maximum (list (score-window-walk model half-window (second a-parts))
			(score-window-run model half-window (second a-parts)))))
	(stationary-1st-half-score
	 (su-line-threshold a-velocity-1st-half < stationary-v half-window))
        (reaches-edge-score (su-features-reach-video-edge model (second a-parts))))
      (joint-score move-score stationary-1st-half-score reaches-edge-score))))

(define (score-leave model actor)
 (let* ((window-size (* 8 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a) (score-window-leave model window-size a))
   window-size actor)))

;;; ---------------------------------------------------------------------------- Score-Lift
(define (score-window-lift model window-size a p a-p)
 (if (or (not (person-features? a)) (person-features? p) (< (length a) 16))
     0 ; 'a' must be a person, and 'p' and object
     (let*
       ((stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	(half-n (inexact->exact (/ n-frames 2)))
	;; Data
        (a-velocity (map single-object-features-velocity a))
	(p-velocity (map single-object-features-velocity a))
	(a-p-centres (map pairwise-object-features-center-distance a-p))
	(a-pos-y (map single-object-features-position-y a))
	(p-pos-y (map single-object-features-position-y p))
	(a-y-minus-p-y (map (lambda (u v) (- u v)) a-pos-y p-pos-y))
	;; Scores
	(a-stationary-score (su-line-threshold a-velocity < stationary-v window-size))
	(p-stationary-1st-half-score
	 (su-line-threshold (take half-n p-velocity) < stationary-v window-size))
	(a-p-olap-score (su-overlap model a-p window-size))
	;; If a is more than 50% above p, then we give a bonus score
        (a-above-p-score (maximum (list 0.5 (su-line-threshold a-y-minus-p-y > 0 window-size)))))
      (joint-score a-stationary-score p-stationary-1st-half-score a-p-olap-score a-above-p-score))))

(define (score-lift model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-lift model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Move
(define (score-window-move model window-size a)
 (if (or (< (length a) 16))
     0 ; (must have 16 points)
     (let*
       ((stationary-v (detect-model-velocity-threshold-stationary model))
	(n-points (length a))
	(third-n (inexact->exact (/ n-points 3)))
	(third-window (inexact->exact (/ window-size 3)))
	;; Data
	(a-velocity (map single-object-features-velocity a))
	(a-vel-1st-third (take third-n a-velocity))
	(a-vel-2nd-third (take third-n (drop third-n a-velocity)))
	(a-vel-3rd-third (drop third-n (drop third-n a-velocity)))
	;; Scores
	(a-stat-1st-third-score (su-line-threshold a-vel-1st-third < stationary-v third-window))
	(a-stat-2nd-third-score (su-line-threshold a-vel-2nd-third < stationary-v third-window))
	(a-stat-3rd-third-score (su-line-threshold a-vel-3rd-third < stationary-v third-window)))
      (joint-score a-stat-1st-third-score (- 1 a-stat-2nd-third-score) a-stat-3rd-third-score))))
	
(define (score-move model actor)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a) (score-window-move model window-size a))
   window-size actor)))

;;; ---------------------------------------------------------------------------- Score-Open
(define (score-window-open model window-size a p a-p)
 (if (or (not (person-features? a)) (person-features? p) (< (length a) 16))
     0 ; 'a' must be a person, and 'p' and object
     (let*
       ((stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	(half-n (inexact->exact (/ n-frames 2)))
	;; Data
        (a-velocity (map single-object-features-velocity a))
	(a-p-centres (map pairwise-object-features-center-distance a-p))
	;; Scores
	(a-stationary-score (su-line-threshold a-velocity < stationary-v window-size))
	(a-p-olap-score (su-overlap model a-p window-size))
	;;(a-p-olap-score (su-line-threshold a-p-centres < overlap-threshold window-size))
	(p-unstable-2nd-half-score (su-single-features-unstable (drop half-n p))))
      (joint-score a-stationary-score a-p-olap-score p-unstable-2nd-half-score))))

(define (score-open model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-open model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Pass
(define (score-window-pass model window-size a p a-p)
 (if (or (< (length a) 16))
     0 ; 
     (let*
       ((n-frames (length a))
	(third-window (inexact->exact (/ window-size 3)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	(third-n (inexact->exact (/ n-frames 3)))
	;; Data
	(a-p-centres (map pairwise-object-features-center-distance a-p))
	(a-p-parts (equal-portions a-p 3))
	(a-pos-x (map single-object-features-position-x a))
	(p-pos-x (map single-object-features-position-x p))
       	;; Scores
	(a-p-centre-v-score (su-line-is-v-shaped a-p-centres window-size))
	(a-p-middle-olap-score (su-overlap model (second a-p-parts) third-n))
	(a-p-left-right-score (su-crossover a-pos-x p-pos-x)))
      (joint-score a-p-centre-v-score a-p-middle-olap-score a-p-left-right-score))))

(define (score-pass model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-pass model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Pick
(define (score-pick model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Push
(define (score-push model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-haul-push model window-size a p a-p #f))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Put
(define (score-put model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Raise
(define (score-raise model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Receive
(define (score-receive model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Replace
(define (score-window-replace model window-size a p a-p)
 (if (< (length a) 16)
     0 
     (let*
       ((n-frames (length a))
	(third-window (inexact->exact (/ window-size 3)))
	(third-n (inexact->exact (/ n-frames 3)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	;; data
	(a-start-pos-x (single-object-features-position-x (first a)))
	(a-start-pos-y (single-object-features-position-y (first a)))
	(p-end-pos-x (single-object-features-position-x (last p)))
	(p-end-pos-y (single-object-features-position-y (last p)))
	(a-p-replace-dist (distance (vector a-start-pos-x a-start-pos-y)
					(vector p-end-pos-x p-end-pos-y)))
	(a-velocity-parts (equal-portions (map single-object-features-velocity a) 3))
	(p-velocity-parts (equal-portions (map single-object-features-velocity p) 3))
      	;; a and p are stationary in the first and third thrids
	(a-stationary-1st-third-score
	 (su-line-threshold (first a-velocity-parts) < stationary-v third-window))
	(a-stationary-3rd-third-score
	 (su-line-threshold (third a-velocity-parts) < stationary-v third-window))
	(p-stationary-1st-third-score
	 (su-line-threshold (first p-velocity-parts) < stationary-v third-window))
	(p-stationary-3rd-third-score
	 (su-line-threshold (third p-velocity-parts) < stationary-v third-window))
	;; A start-pos is close to P end-pos
	(a-p-replace-dist-score
	 (if (> a-p-replace-dist overlap-threshold)
	     0 ; No way should this be a replace
	     (- 1 (/ a-p-replace-dist overlap-threshold))))
	;; Bonus score if a and p are of the same type
	(score-same-type (if (equal? (person-features? a) (person-features? p)) 1 0.5)))
      (joint-score a-stationary-1st-third-score a-stationary-3rd-third-score
		   p-stationary-1st-third-score p-stationary-3rd-third-score
		   a-p-replace-dist-score score-same-type))))
		   
(define (score-replace model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-replace model window-size a p a-p))
   window-size actor patient actor-patient)))


;;; ---------------------------------------------------------------------------- Score-Run
(define (score-window-run model window-size a)
 (if (or (not (person-features? a)) (< (length a) 4))
     0 ; 'a' must be a person (and we must also have 4 points)
     (let*
       ((max-walk-threshold (detect-model-velocity-threshold-walk model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	;; Data
	(actor-velocity (map single-object-features-velocity a))
	;; Scores
	(frame-unstable-score (su-single-features-unstable a))
	(pearson-r-pos-x (feature-pearson-r a single-object-features-position-x))
	(run-threshold-score
	 (su-line-threshold actor-velocity > max-walk-threshold window-size)))
      (joint-score frame-unstable-score run-threshold-score
		   (abs pearson-r-pos-x)))))

(define (score-run model actor)
 (let* ((window-size (* 3 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a) (score-window-run model window-size a))
   window-size actor)))

;;; ---------------------------------------------------------------------------- Score-Snatch
(define (score-snatch model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Stop
(define (score-window-stop model window-size a)
 (if (or (< (length a) 16))
     0 ; 
     (let*
       ((stationary-v (detect-model-velocity-threshold-stationary model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	(half-n (inexact->exact (/ n-frames 2)))
	(half-window (inexact->exact (/ window-size 2)))
	;; Data
	(a-parts (equal-portions a 2))
	(a-velocity-parts (equal-portions (map single-object-features-velocity a) 2))
	;; Scores
	(frame-stabe-2nd-half-score (- 1 (su-single-features-unstable (second a-parts))))
	(moving-1st-half-score
	 (su-line-threshold (first a-velocity-parts) > stationary-v half-window))
	(not-moving-2nd-half-score
	 (su-line-threshold (second a-velocity-parts) < stationary-v half-window))
	(a-isa-person-bonus-score (if (person-features? a) 1 0.8)))
      (joint-score frame-stabe-2nd-half-score
		   moving-1st-half-score not-moving-2nd-half-score
		   a-isa-person-bonus-score))))

(define (score-stop model actor)
 (let* ((window-size (* 6 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a) (score-window-stop model window-size a))
   window-size actor)))

;;; ---------------------------------------------------------------------------- Score-Take
(define (score-take model actor patient actor-patient)
 (let* ((window-size (* 8 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-get model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Throw
(define (score-window-throw model window-size a p a-p)
 (if (or (not (person-features? a)) (< (length a) 16))
     0 ; 'a' must be a person
     (let*
       ((n-frames (length a))
	(half-window (inexact->exact (/ window-size 2)))
	(half-n (inexact->exact (/ n-frames 2)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(overlap-threshold (detect-model-overlap-threshold model))
	;; data
	(a-velocity (map single-object-features-velocity a))
	(p-velocity-parts (equal-portions (map single-object-features-velocity p) 2))
	(p-parts (equal-portions p 2))
	(a-p-parts (equal-portions a-p 2))
      	;; a stationary and stable throughout
	(a-stationary-score (su-line-threshold a-velocity < stationary-v window-size))
	(a-stable-score (su-single-features-stable a))
	;; p starionary 1st half, and moving 2nd half
	(p-stationary-1st-half-score
	 (su-line-threshold (first p-velocity-parts) < stationary-v half-window))
	(p-moving-2nd-half-score
	 (su-line-threshold (second p-velocity-parts) > stationary-v half-window))
	;; p stable 1st half, and unstable 2nd half 
	(p-stable-1st-half-score (su-single-features-stable (first p-parts)))
	(p-unstable-2nd-half-score (su-single-features-unstable (second p-parts)))
	;; a-p olap first half, and clamped not-olap 2nd half
	(a-p-olap-1st-half-score
	 (su-overlap model (first a-p-parts) half-window))
	(a-p-not-olap-2nd-half-score
	 (clamp
	  (- 1 (su-overlap model (second a-p-parts) half-window))
	  0.5 1)))
      (joint-score a-stationary-score a-stable-score
		   p-stationary-1st-half-score p-moving-2nd-half-score
		   p-stable-1st-half-score p-unstable-2nd-half-score
		   a-p-olap-1st-half-score a-p-not-olap-2nd-half-score))))
		   
(define (score-throw model actor patient actor-patient)
 (let* ((window-size (* 6 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-throw model window-size a p a-p))
   window-size actor patient actor-patient)))


;;; ---------------------------------------------------------------------------- Score-Touch
(define (score-window-touch model window-size a p a-p)
 (if (or (not (person-features? a)) (< (length a) 16))
     0 ; 'a' must be a person
     (let*
       ((n-frames (length a))
	(half-window (inexact->exact (/ window-size 2)))
	(half-n (inexact->exact (/ n-frames 2)))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	;; data
	(p-velocity (map single-object-features-velocity a))
	(a-p-parts (equal-portions a-p 2))
      	;; p stationary and throughout (it is being touched)
	(p-stationary-score (su-line-threshold p-velocity < stationary-v window-size))
	;; clamped no-olap in 1st half, and  olap in 2nd half
        (a-p-no-olap-1st-half-score
	 (clamp (- 1 (su-overlap model (first a-p-parts) half-window)) 0.5 1))
	(a-p-olap-2nd-half-score (su-overlap model (second a-p-parts) half-window)))
      (joint-score p-stationary-score 
		   a-p-no-olap-1st-half-score a-p-olap-2nd-half-score))))
		   
(define (score-touch model actor patient actor-patient)
 (let* ((window-size (* 4 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a p a-p) (score-window-touch model window-size a p a-p))
   window-size actor patient actor-patient)))

;;; ---------------------------------------------------------------------------- Score-Turn
(define (score-turn model actor)
 (map-vector (lambda (f) 0.0) actor))

;;; ---------------------------------------------------------------------------- Score-Walk
(define (score-window-walk model window-size a)
 (if (or (not (person-features? a)) (< (length a) 4))
     0 ; 'a' must be a person (and we must also have 4 points)
     (let*
       ((max-walk-threshold (detect-model-velocity-threshold-walk model))
	(stationary-v (detect-model-velocity-threshold-stationary model))
	(n-frames (length a)) ; n-frame < window-size towards end of feature-matrix
	;; Data
	(a-velocity (map single-object-features-velocity a))
	;; Scores
	(pearson-r-pos-x (feature-pearson-r a single-object-features-position-x))
	(moving-score
	 (su-line-threshold a-velocity > stationary-v window-size))
	(not-run-score
	 (su-line-threshold a-velocity < max-walk-threshold window-size)))
      (joint-score moving-score not-run-score (abs pearson-r-pos-x)))))

(define (score-walk model actor)
 (let* ((window-size (* 3 (detect-model-required-frames model))))
  (map-vector-with-lookahead
   (lambda (a) (score-window-walk model window-size a))
   window-size actor)))

;;; ---------------------------------------------------------------------------- Score-glue
;;; Uses score-fun to calculate the scores for the given video.
;;; n-args gives the number of "arguments" that score-fun takes, for example,
;;; if n-args=1, that means we score an 'actor'. n-args=2
;;; imples 'actor', 'patient' /and/ 'actor-patient' (pairwise)
;;; n-args=3 => ('a', 'p', 'z', 'a-p', 'a-z', 'p-z')
;;; In each case, a matrix of single-features or pairwise-features are sent
;;; as the arguments. In turn, score-fun must create a score for each frame
;;; of the video (i.e.: each feature-matrix argument as one row per video-frame).
;;; The 'verb' and 'feature-names' arguments are passed so that results can be
;;; cached (set *use-cached-results* to #f to force a regenerate), and also so
;;; that the cached file knows which arguments have been applied in what order
;;; in order to produce the given score.
(define (score-verb score-fun n-args model verb video
		    feature-names single-features pairwise-features . recalc-option)
 (let ((recalculate (if (not (null? recalc-option)) (equal? #t (first recalc-option))))
       (current-best-score (read-verb-best-score model verb video)))
  (if (and (not (null? current-best-score))  *use-cached-results* (not recalculate))
      (begin
       (format #t "Scored ~a ~a ~a~%" (any-video->string video) verb current-best-score)
       current-best-score)
      (begin
       (format #t "Scoring ~a ~a" (any-video->string video) verb)
       (let*
	 ((n-features (length single-features))
          (n-frames (if (> n-features 0) (vector-length (first single-features)) 0))
	  (arg-ordinal-permutations (permutate (enumerate n-features)  n-args)) ;((0 1 2) (0 3 4) ...)
	  (raw-scores
	   (map
	    (lambda (arg-ordinals) ; (0 1 2)
	     (let*
	       ((single-feature-args ; (matrix-0 matrix-1 matrix-4) of single-features
		 (map (lambda (ordinal) (list-ref single-features ordinal)) arg-ordinals))
		(pairwise-ordinal-pairs ; All pairs of arguments, where (first p) < (second p)
		 (remove-if-not (lambda (p) (< (first p) (second p))) (permutate arg-ordinals 2)))
		(pairwise-feature-args
		 (map
		  (lambda (u v) ; (ordinal-1 ordinal-2)
		   (let ((pairwise-index ; Maps ordinal pair (u v) to index in pair-wise features
			  ;; pairwise-features is like (len=4): ((0 1) (0 2) (0 3) (1 2) (1 3) (2 3))
			  (+ (- v 1) (apply + (sublist (reverse (enumerate (- n-features 1))) 0 u)))))
		    (list-ref pairwise-features pairwise-index)))
		  (map first pairwise-ordinal-pairs)
		  (map second pairwise-ordinal-pairs))))
	      (apply score-fun model (append single-feature-args pairwise-feature-args))))
	    arg-ordinal-permutations))
	  (scores raw-scores)
	  (score-arg-names ; (("person-1" "suv-1" "suv-2") ("person-1" "suv-2" "suv-1") ...)
	   (map
	    (lambda (arg-ordinals)
	     (map (lambda (ordinal) (list-ref feature-names ordinal)) arg-ordinals))
	    arg-ordinal-permutations)))
	
	;; Write results to detect-file
	(write-verb-scores model verb video scores score-arg-names)
	;; Returns the best score within all the argument sets
	(let ((best-score (maximum (map (lambda (v) (maximum (vector->list v))) scores))))
	 (format #t " ~a~%" best-score)
	 best-score))))))

;;; ---------------------------------------------------------------------------- Verb-manifest
;;; A list of (Verb PROCEDURE n-arg) mappings
;;; ("Run" ,score-run 1) means that the run verb uses the score-run procedure
;;; which takes 1 argument .
(define *principled-verbs*
 `(("Approach" ,score-approach 2)
   ("Arrive" ,score-arrive 1)
   ("Attach" ,score-attach 2)
   ("Bounce" ,score-bounce 1)
   ("Bury" ,score-bury 2)
   ("Carry" ,score-carry 2)
   ("Catch" ,score-catch 2)
   ("Chase" ,score-chase 2)
   ("Close" ,score-close 1)
   ("Collide" ,score-collide 2)
   ("Dig" ,score-dig 2)
   ("Drop" ,score-drop 2)
   ("Enter" ,score-enter 1)
   ("Exchange" ,score-exchange 3)
   ("Exit" ,score-exit 1)
   ("Fall" ,score-fall 1)
   ("Flee" ,score-flee 2)
   ("Fly" ,score-fly 1)
   ("Follow" ,score-follow 2)
   ("Get" ,score-get 2)
   ("Give" ,score-give 3)
   ("Go" ,score-go 1)
   ("Hand" ,score-hand 2)
   ("Haul" ,score-haul 2)
   ("Have" ,score-have 1)
   ("Hit" ,score-hit 1)
   ("Hold" ,score-hold 2)
   ("Jump" ,score-jump 1)
   ("Kick" ,score-kick 2)
   ("Leave" ,score-leave 1)
   ("Lift" ,score-lift 2)
   ("Move" ,score-move 1)
   ("Open" ,score-open 2)
   ("Pass" ,score-pass 2)
   ("Pick" ,score-pick 1)
   ("Push" ,score-push 2)
   ("Put" ,score-put 1)
   ("Raise" ,score-raise 1)
   ("Receive" ,score-receive 1)
   ("Replace" ,score-replace 2)
   ("Run" ,score-run 1)
   ("Snatch" ,score-snatch 1) 
   ("Stop" ,score-stop 1)
   ("Take" ,score-take 2)
   ("Throw" ,score-throw 2)
   ("Touch" ,score-touch 2)
   ("Turn" ,score-turn 1)
   ("Walk" ,score-walk 1)))

;;; The list of verbs with a score function
(define (written-verbs)
 (map first (remove-if (lambda (p) (equal? score-null (second p))) *principled-verbs*)))

;;; ---------------------------------------------------------------------------- Entry-point
;;; Lists the scores for each verb in list-of-verbs
(define (principled-verb-detect model video list-of-verbs . recalc-option)
 (let ((recalculate (if (not (null? recalc-option)) (equal? #t (first recalc-option))))
       (lookahead (detect-model-lookahead model))
       (fun-tuples ; ((verb score-verb-fun n-args) (verb score-verb-fun n-args) ...)
	(map
	 (lambda (verb)
	  (let ((matches
		 (remove-if-not (lambda (f) (equal? verb (first f))) *principled-verbs*)))
	   (unless (not (null? matches))
	    (panic "Invalid verb, '~a'" verb)) ; Check "verb" is okay
	   (first matches))) 
	 list-of-verbs))
       (single-features '())
       (pairwise-features '())
       (feature-names '())
       (single-args '())
       (pairwise-args '()))
  (map
   (lambda (verb fun n-args)
    (let ((current-best-score (read-verb-best-score model verb video)))
     (if (and (not (null? current-best-score))  *use-cached-results* (not recalculate))
	 current-best-score
	 (begin
	  (unless (not (null? single-features)) ; Lazy initialisation
	   (set! single-features (all-verb-single-features video lookahead #f))
	   (set! pairwise-features (all-verb-pairwise-features video lookahead #f))
	   (set! feature-names (map first single-features))
	   (set! single-args (map second single-features))
	   (set! pairwise-args (map second pairwise-features)))
	  (score-verb fun n-args model verb video feature-names
		      single-args pairwise-args recalculate)))))
   (map first fun-tuples)
   (map second fun-tuples)
   (map third fun-tuples))))

;;; Scores the detector for each passed verb (across the entire corpus), and then
;;; picks (and saves) an optimal threshold based on an roc curve.
(define (principled-verb-roc model corpus list-of-verbs recalculate . save-roc-graph-dirname)
 (let* ((quality-videos (videos-by-corpus corpus)))
  ;; Calcualte threshold for each verb
  (for-each
   (lambda (verb) 
    (unless (and (not recalculate)
		 (not (null? (read-verb-best-threshold corpus model verb))))
     (let*
       ((+ve-videos (videos-by-verb corpus verb))
	(n-+ve-videos (length +ve-videos))
	(all-ve-videos (videos-by-not-verb corpus verb))
	(n--ve-tests 600) ; We score the verb across 600 random negative videos
	(-ve-videos (n-random-elements-without-replacement n--ve-tests all-ve-videos))
	(test-videos (append +ve-videos -ve-videos))
	(video-verb-match (map-n (lambda (n) (< n n-+ve-videos)) (length test-videos))))
      ;; Score the model for each quality video (of the given verb) in the corpus
      (format #t "Scoring positives for verb ~a across ~a videos~%" verb(length +ve-videos))
      (for-each (lambda (video) (principled-verb-detect model video list-of-verbs recalculate))
                +ve-videos)
      (format #t "Scoring negatives for verb ~a across ~a videos~%" verb (length -ve-videos))
      (for-each (lambda (video) (principled-verb-detect model video list-of-verbs recalculate))
                -ve-videos)
      ;; Now calculate the thresholds
      (format #t "Calculating threshold for verb ~a~%" verb)
      (let*
	((best-scores (map (lambda (v) (read-verb-best-score model verb v)) test-videos))
	 (minimum-score (minimum best-scores))
	 (maximum-score (maximum best-scores))
	 (range (- maximum-score minimum-score))
	 (n-test-thresholds (length best-scores))
	 (roc-data
	  ;; calculate a roc-curve for a range of thresholds
	  (map-n
	   (lambda (n)
	    (let* ((test-threshold (+ minimum-score (/ (* n range) n-test-thresholds)))
		   (roc-point (roc-curve-point best-scores video-verb-match test-threshold)))
	     (list test-threshold roc-point)))
	   ;; Add 1, so that both minimum and maximum scores are considered
	   (+ n-test-thresholds 1)))
	 (best-roc (first (sort roc-data < (lambda (r) (distance (second r) '#(0 1))))))
	 (roc-score (second best-roc))
	 (best-threshold (first best-roc)))
       ;; Draw roc-curve
       (dtrace "BEST" best-threshold)
       ;; Write out the best-threshold to threshold file
       (write-to-principled-threshold-file corpus model verb best-threshold roc-score
					   (map first roc-data) (map second roc-data)))))
    ;; Generate the roc-graph
    (unless (null? save-roc-graph-dirname)
     (let ((roc-data (read-verb-roc-points corpus model verb))
	   (directory (first save-roc-graph-dirname))
	   (filename (string-append "roc_" (detect-model-name model) "_" verb ".png")))
      (system (string-append "mkdir -p '" directory "'")) ; make sure directory exists
      (format #t "Saving roc plot to ~a/~a~%" directory filename)
      (save-roc-plot (map x roc-data)
		     (map y roc-data)
		     (string-append directory "/" filename)))))
   list-of-verbs)))
	
;;; ---------------------------------------------------------------------------- Setup HTML

;;; Returns the score of the model -- 1 minus the average of the best roc distances
;;; Larger is better. Must call (principled-verb-roc model corpus list-of-verbs #f)
;;; to ensure that the roc-scores are available to be read
(define (score-model model corpus list-of-verbs)
 (principled-verb-roc model corpus list-of-verbs #f)
 (let* ((best-roc-pts (map (lambda (verb) (read-verb-best-roc-point corpus model verb)) list-of-verbs))
        (roc-scores (map (lambda (roc-point) (distance roc-point '#(0 1))) best-roc-pts)))
  (- 1 (/ (reduce + roc-scores 0) (length roc-scores)))))


;;; (re)calculates the best score for a model across the list-of-verbs
;;; CAUTION, this method relies on the "roc-me-job" bash script, which must
;;; be on path. All code must be working. See the roc-me-job bash script for
;;; more info.
(define (parallel-score-model corpus lookahead
			      frame-len stationary-v-t run-v-t overlap-t)
 (system (format #f "roc-me-job \"~a\" _a ~a ~a ~a ~a"
		 corpus lookahead frame-len stationary-v-t run-v-t overlap-t))
 (score-model (make-simple-model lookahead frame-len stationary-v-t run-v-t overlap-t)
	      corpus
	      (written-verbs))) 

(define (parallel-score-and-save corpus lookahead
				 frame-len stationary-v-t run-v-t overlap-t
				 filename x)
 (let ((dirname "/home/amichaux/Desktop/rundir/zz-scores")
       (score (parallel-score-model corpus lookahead frame-len
				    stationary-v-t run-v-t overlap-t)))
  (system (format #f "mkdir -p ~a; echo \"~a ~a\" >> ~a/~a" dirname x score dirname filename))))
  

(define (auto-diff-lookahead corpus)
 (define (diff-fun x) (parallel-score-and-save corpus x 50 2 8 150 "lookahead" x))
 (ad-line diff-fun 3 13 4))
					       
(define (auto-diff-frame-len corpus)
 (define (diff-fun x) (parallel-score-and-save corpus 7 x 2 8 150 "frame-len" x))
 (ad-line diff-fun 30 120 4))

(define (auto-diff-stationary-v corpus)
 (define (diff-fun x) (parallel-score-and-save corpus 7 50 x 8 150 "stationary" x))
 (ad-line diff-fun 1 7 4))

(define (auto-diff-run-v corpus)
 (define (diff-fun x) (parallel-score-and-save corpus 7 50 2 x 150 "run" x))
 (ad-line diff-fun 5 12 4))

(define (auto-diff-overlap-t corpus)
 (define (diff-fun x) (parallel-score-and-save corpus 7 50 2 8 x "overlap" x))
 (ad-line diff-fun 100 300 4))

;;; Generates a verb-manifest.php file, along with separate text files that
;;; list the videos falling under each verb in the corpus
(define (generate-php-video-manifest corpus dirname recalculate)
 (system (string-append "mkdir -p " dirname))
 (let ((all-verbs (map first *principled-verbs*)))
  ;; Write out the master verb file
  (write-file
   (append (list "<?php" "$_verb_list = array(")
	   (map (lambda (verb) (string-append "   \"" verb "\" => array(),")) all-verbs)
	   (list ");"))
   (string-append dirname "/../verb-manifest.php"))
  ;; Write out individual verb manifest files
  (for-each
   (lambda (verb)
    (let ((verb-filename (string-append dirname "/" verb "-manifest.text")))
     (unless (and (not recalculate) (file-exists? verb-filename))
      (write-file (map any-video->string (videos-by-verb corpus verb)) verb-filename))))
   all-verbs)))


(define (principled-verb-operations operation corpus html-root list-of-verbs
				    model every-nth offset recalculate)
 (let ((lookahead (detect-model-lookahead model))
       (old-use-cached-results *use-cached-results*)
       (data-videos-dir (string-append html-root "/data/videos"))
       (data-manifest-dir (string-append html-root "/data/manifests"))
       (data-roc-dir (string-append html-root "/data/roc/" corpus "/" (detect-model-name model)))
       (every-other-nth-video (every-other-nth-o (videos-by-corpus corpus) every-nth offset)))
  (unless (find corpus (corpora)) (panic "Could not find corpus ~a" corpus))
  (unless (and (> every-nth 0) (>= offset 0) (> every-nth offset))
   (panic "Every-nth must be > offset, and offset >= 0"))
  (cond
   ((equal? operation "list-verbs") ; -- Displays implemented verbs
    (for-each (lambda (verb) (display verb) (newline)) list-of-verbs))
   ((equal? operation "feature-vectors") ; -- feature vectors
    (for-each (lambda (video)
               (all-verb-single-features video lookahead recalculate)
	       (all-verb-pairwise-features video lookahead recalculate))
	      every-other-nth-video))
   ((equal? operation "feature-graphs") ; -- feature graphs
    (begin
     (format #t "Generating feature vector graphs~%")
     (generate-feature-graphs every-other-nth-video lookahead data-videos-dir recalculate)))
   ((equal? operation "manifest") ; -- php-manifest files
    (begin
     (format #t "Generating php manifest files~%")
     (set! *use-cached-results* (not recalculate))
     (generate-php-video-manifest corpus data-manifest-dir recalculate)
     (set! *use-cached-results* old-use-cached-results)))
   ((equal? operation "scores") ; -- generate scores (without graphs)
    (format #t "Generating scores~%")
    (for-each
     (lambda (video) (principled-verb-detect model video list-of-verbs recalculate))
	      every-other-nth-video))
   ((equal? operation "score-graphs") ; -- graph scores
    (begin
     (format #t "Generating score graphs~%")
     ;; TODO -- put thresholds on score graphs.
     ;; TODO -- output threshold and score information in json format.
     (generate-score-graphs corpus model
			    (written-verbs)
			    every-other-nth-video
			    data-videos-dir recalculate)))
   ((equal? operation "roc") ; -- thresholds (and roc)
    (format #t "Calculating rocs~%")
    (principled-verb-roc model corpus list-of-verbs recalculate))
   ((equal? operation "roc-graphs") ; -- thresholds (and roc)
    (format #t "Calculating rocs graphs~%")
    (principled-verb-roc model corpus list-of-verbs recalculate data-roc-dir))
   ((equal? operation "roc-graph-all-in-one") ; -- all-in-one roc graph
    (format #t "Calculating all-in-one roc graph~%")
    (principled-verb-roc model corpus list-of-verbs recalculate data-roc-dir)
    ;; Create the combined graph
    (start-roc-plot)
    (for-each
     (lambda (verb)
      (let* (;; We want to remove #(0 0) and #(1 1) points (to make the graph nicer)
	     (roc-data (remove-if (lambda (p) (or (and (= (x p) 0) (= (y p) 0))
						  (and (= (x p) 1) (= (y p) 1))))
				  (read-verb-roc-points corpus model verb))))
       ;;    (roc-data raw-roc-data))
       (unless (null? roc-data)
	(scheme->matlab! "X" (map x roc-data))
	(scheme->matlab! "Y" (map y roc-data))
	(matlab-eval-string "plot(X, Y);"))))
     list-of-verbs)
    (matlab-eval-string (format #f "title('~a :: score=~a');"
				(detect-model-name model)
				(score-model model corpus list-of-verbs)))
    (system (format #f "mkdir -p ~a" data-roc-dir))
    (end-roc-plot (format #f "~a/~a_zz-all-in-one.png" data-roc-dir (detect-model-name model))))
   ;; Panic, invalid operation
   ((equal? operation "roc-score") ; -- scores the entire graph
    (principled-verb-roc model corpus list-of-verbs recalculate)
    (format #t "BEST-SCORE on the next line:~%~a~%" (score-model model corpus list-of-verbs)))
   ((equal? operation "auto-diff-frame-len")
    (auto-diff-frame-len corpus))
   ((equal? operation "auto-diff-stationary-v")
    (auto-diff-stationary-v corpus))
   ((equal? operation "auto-diff-run-v")
    (auto-diff-run-v corpus))
   ((equal? operation "auto-diff-overlap-t")
    (auto-diff-overlap-t corpus))
   (else
    (panic
     "~a in an invalid operation. Valid operations are: ~a"
     operation
     (implode ", " '("list-verbs" "feature-vectors" "feature-graphs" "manifest" "scores" "score-graphs" "roc" "roc-graphs" "roc-graph-all-in-one" "roc-score")))))))

;;; ---------------------------------------------------------------------- Output results files
;;; (verb-threshold *default-detect-model* "Run") ==> 0.0784
(define (verb-threshold corpus model verb)
 (let ((threshold-data (read-verb-threshold-file corpus model verb)))
  (if (null? threshold-data)
      1
      (second threshold-data))))

(define (write-verb-threshold-file corpus model filename)
 (call-with-output-file filename
  (lambda (port)
   (format port "~a ~a~%" "Verb" "Threshold")
   (for-each
    (lambda (verb threshold) (format port "~a ~a~%" verb threshold))
    (written-verbs)
    (map (lambda (verb) (verb-threshold corpus *default-detect-model* verb)) (written-verbs))))))

(define (write-scores-file model corpus filename)
 (let ((corpus-videos (videos-by-corpus corpus))
       (verb-list (written-verbs)))
  (call-with-output-file filename
   (lambda (port)
    (format port "~a ~a ~a~%" "Video" "Verb" "Score")
    (for-each
     (lambda (video)
      ;; Make sure the video is scored for each verb
      (principled-verb-detect model video verb-list)
      ;; Write the scores for the video
      (for-each
       (lambda (verb)
	(let ((score (read-verb-best-score model verb video)))
	 (format port "~a ~a ~a~%" (any-video->string video) verb score)))
       verb-list))
     corpus-videos)))))
 
;;; ---------------------------------------------------------------------- Debug
(define (roc-me verb)
 (let* ((corpus "C-D1/recognition")
       (roc-dir (string-append "/home/amichaux/public_html/pvd/data/roc/"
			       corpus "/"
			       (detect-model-name *default-detect-model*))))
 (principled-verb-roc *default-detect-model* "C-D1/recognition" (list verb) #f roc-dir)))

(define (score-me verb)
 (p-op "C-D1/recognition" "scores" 1 0 (list verb) #t))

(define (score-vv verb video)
  (principled-verb-detect *default-detect-model* video (list verb) #t))

(define (all-in-one-roc)
 (p-op "C-D1/recognition" "roc-graph-all-in-one" 1 0 (written-verbs) #f))

(define (roc-graph verb)
 (p-op "C-D1/recognition" "roc-graphs" 1 0 (list verb) #f))

(define (available-roc-graphs)
 (for-each
  roc-graph
  (map (lambda (f) (first (pregexp-split "-" f))) (directory-list "/home/amichaux/public_html/pvd/verbops-data/thresholds/model_lh=7_fl=45_sv=3_rv=8_olap=150"))))

;;; ---------------------------------------------------------------------- More debug
(define (p-op operation corpus every-nth offset list-of-verbs . recalculate)
 (let* ((html-root "/home/amichaux/public_html/pvd")
	(model *default-detect-model*)
        (recalculate (or (null? recalculate) (not (equal? #f (first recalculate))))))
  (if recalculate
      (format #t "Recalculate is ON~%")
      (format #t "Recalculate if off~%"))
  (principled-verb-operations operation
			      corpus
			      html-root
			      list-of-verbs
			      model
			      every-nth
			      offset
			      recalculate)))

;;; Returns a 'darpa' video (accessed through the ~/video-datasets
;;; symlink), or a stand-alone video (from a  fully pathed video name).
(define (load-video video-name)
 (let ((video
	(cond
	 ((video-name->darpa-video-corpus video-name) (string->darpa-video video-name))
	 (else (make-stand-alone-video video-name)))))
  (if (video-first-frame video)
      video
      (dtrace (format #f "Failed to load video: '~a'" video-name) '()))))


(define c1 "PUSH7_A1_C2_Act1_PARK_MR_AFTN_DARK_43b070a8-1dc6-11e0-ad1b-e80688ca39a2")
(define c2 "THROW7_A1_C1_Act1_PARK_ML_AFTN_DARK_b47a2161-07b6-11e0-bcfe-e80688cb869a")
(define N1 (load-video c1))
(define N2 (load-video c2))

(define N1p (all-verb-pairwise-features N1 7 #f))
(define N2p (all-verb-pairwise-features N2 7 #f))
