;; hill-climber.scm
;; requires the following procedures 
;; neuron-count
;; set-nn-weights!
;; (get-robot-position)
;; (restart-physics)
;; mid-level-nn-brain

(use-modules ((rnrs) #:select (vector-map vector-for-each mod))
             (emacsy emacsy))

(define (neuron-count->matrix-size node-counts)
  "Adds a bias weight to each layer except the output layer."
  (let ((counts (reverse node-counts)))
    (cons (car counts) (map 1+ (cdr counts)))))

(define gene-count (apply * (neuron-count->matrix-size neuron-count)))

(define first-time #t)

(define (random-weight)
  (- 1 (random 2.)))

(define (random-brain)
  (let ((w (make-vector gene-count 0.)))
    (vector-map (lambda (x) 
                  (random-weight)) w)))

(define (prob p)
  (<= (random 1.) p))

(define (mutate-gene weight) 
  (if (prob 0.05)
      (random-weight)
      weight))

(define (mutate weights)
  (vector-map mutate-gene weights))

(define-interactive (randomize-brain)
  (set-nn-weights! (random-brain)))

(define-interactive (clear-brain)
  (set-nn-weights! (make-vector gene-count 0.)))

(define (evaluate-robot weights)
  (set-nn-weights! weights)
  (eval-robot))

(define (robot-time)
  (sim-time (sim eracs-buffer)))


(define eval-robot-time 10.) ;; 10 simulated seconds

(define-interactive (eval-robot)
  ;; One problem was the robot's brain updates at 10 Hz which depends
  ;; on the tick-count.  Resetting the tick-count so that evaluations
  ;; are deterministic.  Doesn't fix it entirely, but does help.
  (set! tick-count 0)
  (let ((original-brain current-brain)
        (start-position #f)
        (start-time (robot-time))
        (start-tick tick-count))
    
    (set! current-brain nn-brain)
    (vector-fill! touch-sensors #f)
    (set! direction 'right)
    (restart-physics)
    (sim-time-set! (sim eracs-buffer) 0.0)
    (set! start-time 0.0)
    (set! start-position (get-robot-position))
    (block-until (lambda () 
                  (> (- (robot-time) start-time) eval-robot-time))) 
    (let* ((pos (get-robot-position))
           (fitness (- (vector-ref start-position 0) (vector-ref pos 0))))
      (set! current-brain original-brain)
      (message "Fitness ~a tick-count ~a sim-time ~a." fitness (- tick-count start-tick) (- (robot-time) start-time))
      fitness)))

(define-interactive (osc-noop)
 #f
  )
(define-key eracs-mode-map (kbd "osc-ping") 'osc-noop)

(define-interactive
  (hill-climber #:optional (max-generations (read-from-string (read-from-minibuffer "max-evaluations: "))))
  (message "optimizer: starting")
  ;(message "hill-climber: starting")
  (let* ((parent (if (and #f first-time)
                     (begin 
                       (set! first-time #f) 
                       (random-brain))
                     (get-nn-weights)))
        (parent-fitness (evaluate-robot parent))
        (child #f)
        (child-fitness 0))
   (let loop ((generation 0))
     (set! child (mutate parent))
     (set! child-fitness (evaluate-robot child))
     ;;(message "gen ~a parent-fitness ~6f child-fitness ~6f" generation parent-fitness child-fitness)
     (message "eval ~a best-objective ~6f current-objective ~6f" generation parent-fitness child-fitness)
     (if (> child-fitness parent-fitness)
         (begin (set! parent child)
                (set! parent-fitness child-fitness)))
     (if (< generation max-generations)
      (loop (1+ generation))
      (begin (evaluate-robot parent))))))

(define-interactive (optimize)
  (hill-climber))
