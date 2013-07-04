;; freeze-random-test.scm

(use-modules (freeze-random)
             (check))

(define (f)
  (map random (cdr (iota 19))))

(define g (make-freeze-random f))
(define h (make-freeze-random f))

(check (g) => '(0 1 1 2 2 2 1 2 6 7 10 0 5 3 12 5 5 12))
(check (h) (=> (compose not equal?)) '(0 1 1 2 2 2 1 2 6 7 10 0 5 3 12 5 5 12))
(check (f) => '(0 1 1 2 2 2 1 2 6 7 10 0 5 3 12 5 5 12))
(check (f) (=> (compose not equal?)) '(0 1 1 2 2 2 1 2 6 7 10 0 5 3 12 5 5 12))

(check (g) => '(0 1 1 2 2 2 1 2 6 7 10 0 5 3 12 5 5 12))
(check (g) => '(0 1 1 2 2 2 1 2 6 7 10 0 5 3 12 5 5 12))


