#!./eracs -b -s
!#
;; exp1.scm
(use-modules
 (emacsy emacsy)
 (logging)
 (ice-9 match)
 )

;(set-program-arguments '("exp1.scm" "plot-path" "run1/exp1-trial1/individual-1.bin" "test.pdf"))
(mylog "exp1" pri-debug "command line: ~a" (program-arguments))

(define (help)
  (format (current-error-port) "usage: exp1 run-trial <directory>~%")
  (format (current-error-port) "       exp1 plot-path <file.bin> <output.pdf>~%")
  (quit))

(define args (cdr (program-arguments)))
(when (null? args)
  (help))

(define (for-each-i proc list)
  (for-each proc
            list
            (range 1 (length list))))

(define-fitness
  '((maximize "dummy value")
    (minimize "dummy value"))
  (dummy-fitness genes)
  ;(make-sim)
  ;(make-quadruped-robot)
  (init-scene (make-sim))
  (vector 1.0 2.0))

(define* (plot-path input #:optional (output #f))
  (let ((weights (read-brain input))
        (output-file (or output
                         (format #f "~a/~a.pdf" (dirname input) (basename input ".bin")))))
    (plot-robot-path weights output-file)))

(define
  (plot-fitness-time-series time-series output)
  (mathematica (apply format #f "Export[~a,plotFitnessTimeSeries[~a], ImageSize -> pdfImageSize]"
                       (map sexp->mathematica (list output time-series)))))

(define (run-trial dir)
  (define (file filename)
    (format #f "~a/~a" dir filename))
  (define results #f)
  (define fitness-time-series '())
 (with-blockable
  (mylog "exp1" pri-trace "BEGIN exp1!")
 
  (unless (access? dir F_OK)
    (mkdir dir))
  (set! results (open-output-file (file "results.m")))
  ;; (set! eval-robot-time 2.)
  (set! eval-robot-time 30.)
  (set! init-scene init-robot-scene)
  ;;(set! init-scene init-robot-scene-anemic)
  (set! population-count 
        ;4
        10
        ;;20
        )
  (format results "(* results.m *)~%")
  (set! generation-tick 
        (lambda (generation ind-gene-obj)
          (map (match-lambda 
                ((ind gene obj)
                 (cons! (list generation ind obj) fitness-time-series))) ind-gene-obj)))
  (optimize 
   ;;dummy-fitness
   average-distance 
   ;;30
   50
   ;;100
   ;;2
   ;;3
   )
  (format results "~a~%" (sexp->mathematica fitness-time-series))
  (close-port results)
  (plot-fitness-time-series fitness-time-series (file "fitness-time-series.pdf"))
 (plot-front (file "front.pdf"))
 (for-each-i (lambda (ind-weights number)
               (let ((filename (format #f "~a/individual-~d.bin" dir number)))
                 (write-brain ind-weights filename)
                 (plot-path filename)))
             (map car (first last-results)))
 (call-with-output-file (file "results.txt")
   (lambda (port)
     (format port "individual) fitness~%")
     (for-each-i (lambda (fitness number)
                   (format port "~d) (~{~f~^, ~})~%" number (vector->list fitness)))
                 (map cdr (first last-results)))))
 
 (mylog "exp1" pri-trace "END exp1!")
 (quit-application)))

;; Just eval the command line.
(let* ((func-name (car args))
       (func-sym (string->symbol func-name)))
  (pp (debug-options 'help))
  (eval (cons func-sym (cdr args)) (interaction-environment))
  ;;(quit)
  )
