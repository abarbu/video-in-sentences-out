;; This is standalone so that we don't need to compile darpa-collaboration-related things on steele

(load "idealib-pregexp.sc")

(define (take n l) (sublist l 0 n))
(define (drop n l) (sublist l n (length l)))
(define (drop-until p ls)
 (let loop ((ls ls)) (if (or (null? ls) (p (car ls))) ls (loop (cdr ls)))))

(define (getpid) 1)

(define *unique-temporary-file-nr* 0)
(define (unique-temporary-file file)
 (set! *unique-temporary-file-nr* (+ *unique-temporary-file-nr* 1))
 (format #f
		 "~a_~a_~a_~a.~a"
		 (strip-extension file)
		 (let ((u (getenv "USERNAME"))) (if u u "USERNAME"))
		 (getpid)
		 *unique-temporary-file-nr*
		 (if (has-extension? file)
			 (extension file)
			 "")))

(define (with-temporary-file prefix f)
 (let* ((filename (unique-temporary-file prefix))
	(result (f filename)))
  (rm-if-necessary filename)
  result))

(define (rm-if-necessary pathname)
 (system (format #f "rm -rf ~a" (quotify pathname))))


(define (system-output cmd)
 (with-temporary-file "system.out"
		      (lambda (file)
		       (system (format #f "~a &> ~s" cmd file))
		       (read-file file))))

(define (architecture-path) (car (system-output "architecture-path")))

(define (timestamp) (car (system-output "date +%s")))

(define (detection-job-counter-pathname video-name model)
 (format #f "~a/counters/~a-~a" (getenv "HOME") video-name model))

(define (generate-detection-job video-name model threshold-difference nms-threshold method look-ahead)
 (list
  (format #f "module load matlab ffmpeg-0.5 imagemagick~%")
  (format #f "echo ~a >> ~a" (timestamp)
	  (detection-job-counter-pathname video-name model))
  (format #f "cd ~a/darpa-collaboration/bin" (getenv "HOME"))
  (format #f "mkdir -p ~a/video-datasets/C-D1a/SINGLE_VERB/~a" (getenv "HOME") video-name)
  (format #f "rsync --exclude frame.ppm -avz istihbarat.ecn.purdue.edu:/aux/abarbu/steele/~a ~a/video-datasets/C-D1a/SINGLE_VERB/"
	  video-name (getenv "HOME"))
  (format #f "find ~a/video-datasets/C-D1a/SINGLE_VERB/~a -name \\*.gz -exec gunzip {} \\;" (getenv "HOME") video-name)
  (format #f "./fetch-unpack-detect-and-track.sh ~a ~a ~a ~a ~a ~a &> ~a/logs/~a-~a~%"
	  video-name model threshold-difference nms-threshold method look-ahead
	  (getenv "HOME") video-name model)
  (format #f "find ~a/video-datasets/C-D1a/SINGLE_VERB/~a -name \\*.ssv -exec gzip {} \\;" (getenv "HOME") video-name)
  (format #f "rsync --exclude frame.ppm -avz ~a/video-datasets/C-D1a/SINGLE_VERB/~a istihbarat.ecn.purdue.edu:/aux/abarbu/steele/"
	  (getenv "HOME") video-name)
  (format #f "rm -rf ~a/video-datasets/C-D1a/SINGLE_VERB/~a" (getenv "HOME") video-name)))

(define (detection-job-tries video-name model)
 (if (file-exists?  (detection-job-counter-pathname video-name model))
     (length (read-file (detection-job-counter-pathname video-name model)))
     0))

(define (detection-job-name video-name model)
 (list->string
  (take 14
	(string->list
	 (first
	  (system-output (format #f "md5sum <<< ~a-~a" video-name model)))))))

(define (detection-job-running? video-name model)
 (find-if
  (lambda (j)
   (and
    (equal? (field-ref j 4) "R")
    (equal? (field-ref j 1) (detection-job-name video-name model))))
  (cddr (system-output "qstat rcac-a"))))

(define (detection-job-queued? video-name model)
 (find-if
  (lambda (j)
   (and
    (equal? (field-ref j 4) "Q")
    (equal? (field-ref j 1) (detection-job-name video-name model))))
  (cddr (system-output "qstat rcac-a"))))

(define (detection-job-completed? video-name model)
 (and (> (detection-job-tries video-name model) 0)
      (equal?
       "Tracking Done."
       (last
	(read-file
	 (format #f "~a/logs/~a-~a" (getenv "HOME") video-name model))))))

(define (detection-job-new? video-name model)
 (and (not (detection-job-queued? video-name model))
      (not (detection-job-running? video-name model))
      (> (detection-job-tries video-name model) 0)))

(define (detection-job-aborted? video-name model)
 (and (not (detection-job-queued? video-name model))
      (not (detection-job-running? video-name model))
      (not (detection-job-completed? video-name model))
      (> (detection-job-tries video-name model) 0)))

(define (detection-job-fataled? video-name model)
 (and (not (detection-job-queued? video-name model))
      (not (detection-job-running? video-name model))
      (not (detection-job-completed? video-name model))
      (> (detection-job-tries video-name model) 2)))

(define (detection-job-pathname video-name model)
 (format #f "~a/new-jobs/~a" (getenv "HOME") (detection-job-name video-name model)))

(define (submit-detection-job video-name model threshold-difference nms-threshold method look-ahead)
 (write-file (generate-detection-job video-name model threshold-difference nms-threshold method look-ahead)
	     (detection-job-pathname video-name model))
 (format #f
	 "qsub -q rcac-a -N ~a -k oe -l select=1:ncpus=1,walltime=30:00:00 ~a"
	 (detection-job-name video-name model)
	 (detection-job-pathname video-name model)))

(define (darpa-verb name)
 (car (pregexp-split "\\|" (pregexp-replace* "\(\[B0-9\]+_\)+|_" name "|\\0|"))))

(define verb-objects
 (read-object-from-file "~/darpa-collaboration/documentation/verb-objects.sc"))

(define (generate-all-jobs)
 (write-file
  (reduce
   append
   (map
    (lambda (s)
     (map (lambda (v) (submit-detection-job s (string-downcase (symbol->string v)) -1 0.8 1 5))
	  (cons 'person (cdr (assoc (darpa-verb s) verb-objects)))))
    (read-file (format #f "~a/darpa-collaboration/documentation/single-verb-videos.text" (getenv "HOME"))))
   '())
  (format #f "~a/all-jobs" (getenv "HOME"))))

(define (generate-all-people)
 (write-file
  (reduce
   append
   (map
    (lambda (s)
     (map (lambda (v) (submit-detection-job s (string-downcase (symbol->string v)) -1 0.8 1 5))
      	  '(person)))
    (read-file (format #f "~a/darpa-collaboration/documentation/single-verb-videos.text" (getenv "HOME"))))
   '())
  (format #f "~a/all-jobs" (getenv "HOME"))))

(define (generate-broken)
 (write-file
  (map (lambda (a) (submit-detection-job (field-ref a 0) (field-ref a 1) -1 0.8 1 5))
       (read-file (format #f "~a/broken" (getenv "HOME"))))
  (format #f "~a/broken-jobs" (getenv "HOME"))))

(define (link-people)
 (for-each
  (lambda (s)
   (for-each 
    (lambda (v)
     (system
      (format #f "ln -s ~~/logs/~a-~a ~~/mapping/~a"
	      s (string-downcase (symbol->string v)) 
	      (detection-job-name s (string-downcase (symbol->string v))))))
    '(person)))
  (read-file (format #f "~a/darpa-collaboration/documentation/single-verb-videos.text" (getenv "HOME")))))
