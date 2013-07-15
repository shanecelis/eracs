;; determine-scheme-time.scm
(use-modules (mathematica))
(set! physics-class <bullet-physics-car>)

(define trials 10)
(define sim (current-sim))
(format #t "sim ~a~%" sim)

;; (format #t "original time ~a~% " (sim-time sim))

;; (sim-tick sim 0.1 1)
;; (format #t "after one step ~a~% " (sim-time sim))
;; (sim-tick sim 0.1 2)
;; (format #t "after two steps ~a~% " (sim-time sim))
;; (sim-tick sim 0.1 3)
;; (format #t "after three steps ~a~% " (sim-time sim))
(define (time-for-count count)
  (let ((t #f))
    
    (record-time* t (dotimes trials (eval-beer-robot current-genome 
                                                #:begin-fn 
                                                (lambda (fode-state)
                                                  (set-time! fode-state 0.)
                                                  (set! (step-count fode-state) count)
                                                  )
                                                #:end-fn
                                                (lambda (fode-state)
                                                  #f
                                                  (format #t "time ~a with step-count ~a~%" (get-time fode-state) (step-count fode-state))
                                                  )
                                                #:max-tick-count 2000
                                                )))
    t))

;(define counts '(1 5 10 15 20 30))
(define counts '(5 10 15 20 30))
(define bullet-time (map time-for-count counts))


(set! physics-class <fode-physics>)
;(set! physics-class <bullet-physics-car>)
;(set! make-effector-func make-c-effector-func)

(define fode-time (map time-for-count counts))




(line-plot (list (map list counts fode-time) 
                 (map list counts bullet-time)) 
           #:axes-label '("step-count" "duration")
           #:plot-legends '("FODE" "Bullet"))
(exit 0)
