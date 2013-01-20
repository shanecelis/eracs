#line 33 "util.nw"
(define-public (my-load-file filename)
  (with-backtrace* (lambda () (load filename)) '(quit)))
#line 38 "util.nw"
(define (for-each-i proc lst . args)
  (apply 
   for-each proc
   lst
   (append args (list (range 1 (length lst))))))
#line 46 "util.nw"
(define (for-each-i0 proc lst . args)
  (apply 
   for-each proc
   lst
   (append args (list (range 0 (1- (length lst)))))))
#line 55 "util.nw"
(define (map-i proc lst . args)
  (apply 
   map proc
   lst
   (append args (list (range 1 (length lst))))))
#line 63 "util.nw"
(define (map-i0 proc lst . args)
  (apply 
   map proc
   lst
   (append args (list (range 0 (1- (length lst)))))))
