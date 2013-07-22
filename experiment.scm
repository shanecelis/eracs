;; experiment.scm
;; I need to simplify this experiment stuff.

(define-module (experiment)
  #:use-module (oop goops)
  #:export (<experiment>
            exp:parameters
            exp:data
            exp:results
            generate-parameters!
            run-experiment!
            analyze-data!
            clear-experiment!
            
            <parent-experiment>
            exp:child-experiments
            ))

(define-class <experiment> ()
  (parameters #:accessor exp:parameters #:init-value #f)
  (data #:accessor exp:data #:init-value #f)
  (results #:accessor exp:results #:init-value #f))

(define-generic generate-parameters!) 
(define-generic run-experiment!)
(define-generic analyze-data!)

(define-method (generate-parameters! (exp <experiment>))
  #f)

(define-method (run-experiment! (exp <experiment>))
  #f)

(define-method (analyze-data! (exp <experiment>))
  #f)


(define-method (clear-experiment! (exp <experiment>))
  (set! (exp:parameters exp) #f)
  (set! (exp:data exp) #f)
  (set! (exp:results exp) #f))

;; Let's make it so we can have a hierarchy of experiments.
(define-class <parent-experiment> (<experiment>)
  (child-experiments #:accessor exp:child-experiments #:init-keyword #:child-experiments #:init-form '())
  (aggregate-proc #:accessor exp:aggregate-proc #:init-keyword #:aggregate-proc #:init-value #f)
  )

(define-method (generate-parameters! (exp <parent-experiment>))
  (for-each generate-parameters! (exp:child-experiments exp))
  (next-method))

(define-method (run-experiment! (exp <parent-experiment>))
  (for-each run-experiment! (exp:child-experiments exp))
  (next-method))

(define-method (analyze-data! (exp <parent-experiment>))
  (for-each analyze-data! (exp:child-experiments exp))
  (next-method)
  (if (exp:aggregate-proc exp)
      ((exp:aggregate-proc exp) (exp:child-experiments exp))))

(define-method (clear-experiment! (exp <parent-experiment>))
  (next-method)
  (for-each clear-experiment! (exp:child-experiments exp)))
