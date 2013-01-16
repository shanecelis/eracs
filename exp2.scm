(use-modules
 (emacsy emacsy)
 (logging))

 (mylog "exp2" pri-debug "command line: ~a" (program-arguments))

(define args (cdr (program-arguments)))
(when (not (= 1 (length args)))
  (format (current-error-port) "usage: exp2 <directory>")
  (quit))

(define dir (car args))

(define (for-each-i proc list)
  (for-each proc
            list
            (range 1 (length list))))

(define (file filename)
  (format #f "~a/~a" dir filename)
  )

(with-blockable
 (mylog "exp2" pri-trace "BEGIN exp2!")
 
 (unless (access? dir F_OK)
     (mkdir dir))
; (set! eval-robot-time 2.)
 (set! eval-robot-time 60.)
 (set! init-scene init-robot-obstacle-scene)
 (set! population-count 
       ;; 4
       ;;10
       20
       )
 (optimize high-level-waypoint-fitness
           200
           ;;3
           )
 (plot-front (file "front.pdf"))
 (for-each-i (lambda (ind-weights number)
               (write-brain ind-weights (format #f "~a/individual-~d.bin" dir number)))
             (map car (first last-results)))
 (call-with-output-file (file "results.txt")
   (lambda (port)
     (format "individual) fitness~%")
     (for-each-i (lambda (fitness number)
                   (format port "~d) (~{~f~^, ~})~%" number (vector->list fitness)))
                 (map cdr (first last-results)))))
 
 (mylog "exp2" pri-trace "END exp2!")
 (quit-application))

