#line 29 "util.nw"
(use-modules (system repl error-handling))
#line 34 "util.nw"
(define-public (my-load-file filename)
  #;(with-backtrace* (lambda () (load filename)) '(quit))
  (call-with-error-handling (lambda () 
    (format #t "~a" (current-module))
    (load filename)))
)
#line 43 "util.nw"
(define (for-each-i proc lst . args)
  (apply 
   for-each proc
   lst
   (append args (list (range 1 (length lst))))))
#line 51 "util.nw"
(define (for-each-i0 proc lst . args)
  (apply 
   for-each proc
   lst
   (append args (list (range 0 (1- (length lst)))))))
#line 60 "util.nw"
(define (map-i proc lst . args)
  (apply 
   map proc
   lst
   (append args (list (range 1 (length lst))))))
#line 68 "util.nw"
(define (map-i0 proc lst . args)
  (apply 
   map proc
   lst
   (append args (list (range 0 (1- (length lst)))))))
