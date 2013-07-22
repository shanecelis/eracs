(define-module (fitness)
  #:use-module (emacsy util)
  #:use-module (emacsy emacsy)
  #:use-module (vector-math)
  #:use-module (ice-9 optargs) ;; define*, define*-public
  )

(define-public fitness-functions '())
(define-public fitness-functions-alist '())

#;"The macro define-fitness defines a procedure that contains information about what its results are, e.g. (define-fitness ((minimize \"x^2 - 1\")) (f genes) (: (genes @ 0) * (genes @ 0) - 1)) defines the procedure f which returns one result that which is to be minimized."
(define-syntax-public define-fitness
  (syntax-rules ()
    ((define-fitness the-objectives (name . args) . body)
     (begin (define-interactive (name . args)
              . body)
            (set-procedure-property! name 'objectives 'the-objectives)
            (set! fitness-functions-alist (assq-set! fitness-functions-alist 'name name))
            (set! fitness-functions (map cdr fitness-functions-alist))))))

(define-public (fitness? func)
  (and (procedure? func) (objectives func) #t))

(define*-public (fitness-desc func #:optional (port #f))
  (format port "~a ~{~d) ~as ~a~^, ~}."
          (procedure-name func)
          (apply append! (map (lambda (number objective)
                  (list number 
                        (symbol->string (car objective))
                        (cadr objective)))
                (range 1 (length (objectives func)))
                (objectives func)))))

(define-public (objectives fitness-func)
  (procedure-property fitness-func 'objectives))


