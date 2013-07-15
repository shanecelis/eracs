
(define (main)
 (define trials 5)
 (define max-generations 30)
 (define g1 '())
 (define g2 '())
 (define g23 '())
 (define g123 '())
 (define t1 0.)
 (define t2 0.)
 (define t23 0.)
 (define t123 0.)

 (define (report i lst time)
   (when (not (null? lst))
       (let ((gen-counts (map car lst))
             (eval-counts (map cadr lst)))
        (format #t "Task ~a requires ~1,1f (~1,1f) generations (~1,1f gen/min).~%" i (mean gen-counts) (std gen-counts) (/ time (mean gen-counts) trials 60.))
        (format #t "    Total time ~as; total generations ~a; total evaluations ~a.~%" time (apply + gen-counts) (apply + eval-counts))
        )))

 (define (report-everything)
   (format #t "REPORT~%")
   (format #t "trials = ~a~%" trials)
   (format #t "max-generations = ~a~%" max-generations)
   (report "1" g1 t1)
   (report "2" g2 t2)
   (report "2 and 3" g23 t23)
   (report "1, 2 and 3" g123 t123)
   (format #t "END REPORT~%"))
 
 (define pos-diff '(0.  40. 80. 120. 160. ))
 (define ICs (map make-parametric-IC pos-diff))
 
(define (gen-count-for-IC IC)
  (let ((gens '()))
   (dotimes trials (cons! 
                    (generation-count-to-do (list IC) max-generations '()) 
                    gens))
   (mean (map car gens))))

(define gens (map gen-count-for-IC ICs))

(line-plot (map list pos-diff gens))

 ;; (record-time t1 (dotimes trials (cons! (generation-count-to-do (list case-1-IC) max-generations) g1)))
 ;; (report-everything)

 ;; (record-time t2 (dotimes trials (cons! (generation-count-to-do (list case-2-IC) max-generations) g2)))
 ;; (report-everything)

 ;; (record-time t23 (dotimes trials (cons! (generation-count-to-do (list case-2-IC case-3-IC) max-generations) g23)))
 ;; (report-everything)

 ;(record-time t123 (dotimes trials (cons! (generation-count-to-do (list case-1-IC case-2-IC case-3-IC) max-generations) g123)))
 ;(report-everything)
 )

(main)
(exit 0)
