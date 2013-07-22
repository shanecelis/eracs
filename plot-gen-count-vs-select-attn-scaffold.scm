

(define (main)
  (define trials 3)
  (define max-generations 40)

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
  
  (define task-counts '(1 2 3 4))

  (define IC (generate-catchable-IC))
  (define (gen-count-for-task-count task-count)
    (let ((gens '())
          (ICs (list IC)))
      (define (add-IC!) 
        (cons! (generate-catchable-IC)
               ICs))
      (dotimes 
       trials 
       (cons! 
        (begin
          (generation-count-to-do 
           (lambda () (map make-apply-IC ICs)) 
           max-generations 
           '()
           (compose not (make-scaffold-any-individual-succeeded? 
                         task-count
                         add-IC!)))) 
        gens))
      (mean (map car gens))))

  (define gens (map gen-count-for-task-count task-counts))

  (line-plot (map list task-counts gens)))

;(set! physics-class <fode-physics>)
(set! physics-class <bullet-physics-car>)
(main)
(exit 0)
