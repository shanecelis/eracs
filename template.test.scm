;; template.test.scm
(use-modules (check)) 
(use-modules (ice-9 pretty-print))
 
(define test-errors '())  
(check #t => #t)
(check #f => #t)

(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
