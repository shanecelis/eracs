;; template.test.scm
(use-modules (check) 
             (ice-9 pretty-print)
             (vector-math))

(define test-errors '())  

(load "active-preferences.scm")  
(define given-points '(#(-0.2 0.3) #(-0.5 -0.2) #(0.3 0.8) #(0.6 -1)))
(define points (given-points->points sin given-points)) 
(check 1 (=> =?) 1.)
(check '(1) (=> =?) '(1.)) 
(check #(1) (=> =?) #(1.))
(check '(#(1)) (=> =?) '(#(1.)))

(check points (=> =?) '(#(-0.2 0.498669330795061) #(-0.5 0.279425538604203) #(0.3 0.50447979333866) #(0.6 -1.56464247339504))) 

(check ((compose-triangles '()) 0) => 0)
(define c (compose-triangles points))
(check (c 0) (=> =?) 0.299201598477037) 

(define e (compute-error points sin (lambda (t) (sin (* 2 t)))))
(check (e 0) (=> =?) 0.179520959086222)
(check (e 1) (=> =?) 0.0761509873393584)
   
(define indexed-given-points '((1 . #(.1 .2))))
;(define indexed-given-points '())
(define f (lambda (t) (make-vector 8 (sin t))))
(define indexed-points (given-indexed-points->indexed-points
                        f indexed-given-points 8))  
(check indexed-points (=> =?) '((1 . #(.1 0.100016)))) 

(check (map-i list '(a b c d)) => '((a 1) (b 2) (c 3) (d 4))) 

(check (map-i0 list '(a b c d)) => '((a 0) (b 1) (c 2) (d 3))) 

(check (given-indexed-points->indexed-points* f '((1 . #(.2 .2))) 8) 
       (=> =?) '(() ((1 . #(0.2 0.00133066920493879))) () () () () () ()))

(define g (lambda (t) (make-vector 8 (sin (* 2 t)))))

(define ci (compose-triangles-indexed '((1 . #(.2 .2))) 8))
(check (ci 0) => #(0 0.12 0 0 0 0 0 0))

(define ei (compute-error-indexed '((1 . #(.2 .2))) f g 8))
(check (ei 1) (=> =?) 0) 
(check (ei 0.1) (=> =?) 0.048931)

(check-report)
(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
