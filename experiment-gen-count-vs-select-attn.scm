(define-module (experiment-gen-count-vs-select-attn)
  #:use-module (guile-user)
  #:use-module (oop goops)
  #:use-module (experiment)
  #:use-module (emacsy util)
  #:use-module (srfi srfi-26) ; cut cute
  #:use-module (srfi srfi-1)  ; take
  #:export (<gen-count-vs-select-attn-trial>
            <gen-count-vs-select-attn>

            <gen-count-vs-select-attn-dummy>
            exp:task-count))

(eval-when (compile load eval)
           ;; Some trickery so we can access private procedures.
           (module-use! (resolve-module '(experiment-gen-count-vs-select-attn)) (resolve-module '(guile-user))))

(define-class <gen-count-vs-select-attn-trial> (<experiment>)
  (task-count #:getter exp:task-count #:init-keyword #:task-count)
  (max-generations #:getter exp:max-gen #:init-keyword #:max-gen #:init-value 1)
  (max-speed #:getter exp:max-speed #:init-keyword #:max-speed #:init-value 1.0)
  (task-done-at-gen #:accessor exp:task-done-at-gen #:init-form #f)
  (task-done-at-time #:accessor exp:task-done-at-time #:init-form #f)
  (physics-class #:accessor exp:physics-class #:init-keyword #:physics-class #:init-value #f)
  )

(define-class <gen-count-vs-select-attn-dummy> (<gen-count-vs-select-attn-trial>))

(define-method (initialize (exp <gen-count-vs-select-attn-trial>) initargs)
  (next-method)
  (set! (exp:task-done-at-gen exp)  (make-vector (exp:task-count exp) #f))
  (set! (exp:task-done-at-time exp) (make-vector (exp:task-count exp) #f)))

(define-method (generate-parameters! (exp <gen-count-vs-select-attn-trial>))
  (set! (exp:parameters exp)
        (map (lambda (i) 
               (generate-catchable-IC body-count (exp:max-speed exp)))
             (iota (exp:task-count exp)))))

(define-method (run-experiment! (exp <gen-count-vs-select-attn-dummy>))
  (format #t "Running dummy~%")
  (set! (exp:data exp) (list (random 5) (random 5) (random 5)))
  (for-each (lambda (i)
              (vector-set! (exp:task-done-at-gen exp) i (random 10.)))
            (iota (exp:task-count exp)))
  (format #t "Stop running dummy~%")
  )

(define-method (run-experiment! (exp <gen-count-vs-select-attn-trial>))
  (format #t "Running trial~%")
  (if (exp:physics-class exp)
      (set! (@@ (guile-user) physics-class) (exp:physics-class exp)))
  (let* ((start-time (emacsy-time))
         (task-counter 1)
         (ICs (map make-apply-IC (take (exp:parameters exp) task-counter))))
    (define (add-IC! generation)
      (vector-set! (exp:task-done-at-gen exp) (1- task-counter) generation)
      (vector-set! (exp:task-done-at-time exp) (1- task-counter) (- (emacsy-time) start-time))
      (incr! task-counter)
      (when (<= (length (exp:parameters exp)) task-counter)
       (set! ICs (map make-apply-IC (take (exp:parameters exp) task-counter))))
      ICs)
    (set! (exp:data exp)
          (generation-count-to-do2 
           (lambda () ICs) 
           (exp:max-gen exp)
           '()
           (compose not (make-scaffold-any-individual-succeeded? 
                         (1- (exp:task-count exp))
                         add-IC!))))))

(define-method (analyze-data! (exp <gen-count-vs-select-attn-trial>))
  (set! (exp:results exp) (cadr (exp:data exp))))

;;
(define-class <gen-count-vs-select-attn> (<parent-experiment>))

(define-method (run-experiment! (exp <gen-count-vs-select-attn>))
  (next-method)
  ;; What's the data for the aggregate? Don't know. #f?
  (set! (exp:data exp) #f))

(define-method (analyze-data! (exp <gen-count-vs-select-attn>))
  (define (exp->points exp)
    (let ((l (vector->list (exp:task-done-at-gen exp))))
      (map list (iota (length l)) l)))
  (define (exp-time->points exp)
    (let ((l (vector->list (exp:task-done-at-time exp))))
      (map list (iota (length l)) l)))
  (next-method)
  (let* ((exps (exp:child-experiments exp))
         (results (map exp:results exps))
         (m (mean results))
         (s (std results)))
    (set! (exp:results exp) (list m s))
    #;(format #t "points ~a~%" (append-map! exp->points exps))
    (line-plot (map exp->points exps) #:joined #t #:axes-label '("task number" "mean generation"))
    (line-plot (map exp-time->points exps) #:joined #t #:axes-label '("task number" "mean time"))
    
    ))




