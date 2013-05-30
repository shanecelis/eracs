;; https://gist.github.com/valvallow/413146
(define-syntax dotimes
  (syntax-rules ()
    ((_ n body ...)
     (do ((i n (- i 1)))
         ((not (< 0 i)))
       body ...))))

(define-syntax record-time
  (syntax-rules ()
    ((_ var body)
     (begin 
       (let ((start (current-time)))
         body
         (set! var (- (current-time) start)))))))

(define trials 1)
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
  (if (not (null? lst))
      (format #t "Task ~a requires ~1,1f (~1,1f) generations (~1,1f gen/min).~%" i (mean lst) (std lst) (/ time (mean lst) trials 60.))))

(define (report-everything)
  (format #t "REPORT~%")
  (report "1" g1 t1)
  (report "2" g2 t2)
  (report "2 and 3" g23 t23)
  (report "1, 2 and 3" g123 t123)
  (format #t "END REPORT~%"))

(record-time t1 (dotimes trials (cons! (generation-count-to-do (list case-1-IC) max-generations) g1)))
(report-everything)

(record-time t2 (dotimes trials (cons! (generation-count-to-do (list case-2-IC) max-generations) g2)))
(report-everything)

(record-time t23 (dotimes trials (cons! (generation-count-to-do (list case-2-IC case-3-IC) max-generations) g23)))
(report-everything)

;(record-time t123 (dotimes trials (cons! (generation-count-to-do (list case-1-IC case-2-IC case-3-IC) max-generations) g123)))
(report-everything)

(exit 0)