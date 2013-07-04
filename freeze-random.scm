;; freeze-random.scm

(define-module (freeze-random)
  #:use-module (emacsy util)
  #:export (make-freeze-random))

(define (make-freeze-random f)
  "Captures the random state when first called.  Then it will use the
same random seed for every subsequent call. It does change the
*random-state* by calling (random 1) to preserve expectations of
changes to the *random-state* when calling random-using procedures."
  (let ((random-state #f))
   (lambda args
     (if (not random-state)
         (set! random-state (copy-random-state)))
     (let ((previous-random-state *random-state*))
       (random 1)
       (in-out 
        (set! *random-state* (copy-random-state random-state))
        (apply f args)
        (set! *random-state* previous-random-state))))))
