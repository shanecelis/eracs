;; https://gist.github.com/valvallow/413146
(define-syntax dotimes
  (syntax-rules ()
    ((_ n body ...)
     (do ((i n (- i 1)))
         ((not (< 0 i)))
       body ...))))

(define trials 1)
(define g1 '())
(dotimes trials (cons! (generation-count-to-do (list case-1-IC)) g1))

(define g2 '())
(dotimes trials (cons! (generation-count-to-do (list case-2-IC)) g2))

(define g23 '())
(dotimes trials (cons! (generation-count-to-do (list case-2-IC case-3-IC)) g23))

(define g123 '())
(dotimes trials (cons! (generation-count-to-do (list case-1-IC case-2-IC case-3-IC)) g123))

(format #t "Task 1 requires ~1,1f (~1,1f) generations.~%" (mean g1) (std g1))

(format #t "Task 2 requires ~1,1f (~1,1f) generations.~%" (mean g2) (std g2))

(format #t "Task 2 and 3 requires ~1,1f (~1,1f) generations.~%" (mean g23) (std g23))

(format #t "Task 1, 2 and 3 requires ~1,1f (~1,1f) generations.~%" (mean g123) (std g123))

(exit 0)
