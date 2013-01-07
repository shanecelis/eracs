;; hill-climber.scm
;; requires the following procedures 
;; neuron-count
;; set-nn-weights!
;; (get-robot-position)
;; (restart-physics)
;; mid-level-nn-brain

(use-modules ((rnrs) #:select (vector-map vector-for-each mod))
             (emacsy emacsy)
             (nsga2)
             (vector-math)
             (infix)
             (srfi srfi-11) ;; let-values
             (srfi srfi-26) ;; cut cute
             (mathematica plot))

(define (neuron-count->matrix-size node-counts)
  "Adds a bias weight to each layer except the output layer."
  (let ((counts (reverse node-counts)))
    (cons (car counts) (map 1+ (cdr counts)))))

(define (neuron-count->matrix-sizes node-counts)
  "Adds a bias weight to each input layer."
  (if (< (length node-counts) 2)
      '()
      (cons (list (1+ (car node-counts)) (cadr node-counts)) (neuron-count->matrix-sizes (cdr node-counts)))))

(define (neuron-count->weight-count neuron-count)
  (apply + (map (lambda (size) (apply * size)) 
                (neuron-count->matrix-sizes neuron-count))))

(define gene-count (neuron-count->weight-count neuron-count))

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
  (set-nn-weights! robot (random-brain)))

(define-interactive (clear-brain)
  (set-nn-weights! robot (make-vector gene-count 0.)))

(define (evaluate-robot weights)
  (set-nn-weights! robot weights)
  (eval-robot))

(define eval-robot-time 20.) ;; 20 simulated seconds

(define* (eval-robot-headless weights 
                              #:key 
                              (step-fn identity)
                              (begin-fn identity)
                              (end-fn identity))
  "Evaluates a robot with the given NN weights and optionally calls a thunk f every time step."
  (let* ((sim (make-sim))
         (robot (init-scene sim))
         (start-position #f)
         (start-time #f)
         (start-tick tick-count))
    (set-nn-weights! robot weights)
    (set! direction 'right)
    (set! (controller robot) run-nn-brain)
    (begin-fn robot)
    (while (< (sim-time sim) eval-robot-time)
      ;(message "Simulating ~a" (sim-time sim))
      (sim-tick sim)
      (robot-tick robot)
      (step-fn robot))
    (let ((fitness (end-fn robot)))
      (sim-remove-robot sim robot)
      fitness)
    #;(let* ((pos (robot-position robot))
           (xz-proj (vector 1. 0. 1.)) ;; Project only in the xz-plane (height displacement doesn't count.
           (fitness (vector-norm (vector* xz-proj (vector- start-position pos)))))
     (message "Fitness ~a tick-count ~a sim-time ~a." 
              fitness (tick-count robot) (sim-time sim))
     (sim-remove-robot sim robot)
     (- fitness))))

(define (eval-robot-to-target-headless weights)
  (let* ((sim (make-sim))
         (robot (init-scene sim))
         (start-position #f)
         (start-time #f)
         (start-tick tick-count)
         (target-position #(0 1 -10)))
    (set-nn-weights! robot weights)
    (set! direction 'right)
    (set! (controller robot) run-nn-brain)
    (set! start-position (robot-position robot))
    (while (< (sim-time sim) eval-robot-time)
      (sim-tick sim)
      (robot-tick robot))
    (let* ((pos (robot-position robot))
           (xz-proj (vector 1. 0. 1.)) ;; Project only in the xz-plane (height displacement doesn't count.
           (fitness (vector-norm (vector* xz-proj (vector- target-position pos)))))
     (message "Fitness ~a tick-count ~a sim-time ~a." 
              fitness (tick-count robot) (sim-time sim))
     (sim-remove-robot sim robot)
     fitness)))

(define-interactive (eval-robot)
  ;; One problem was the robot's brain updates at 10 Hz which depends
  ;; on the tick-count.  Resetting the tick-count so that evaluations
  ;; are deterministic.  Doesn't fix it entirely, but does help.
  ;(set! (tick-count robot) 0)
  (let ((original-brain (controller robot))
        (start-position #f)
        (start-time (robot-time robot))
        (start-tick (tick-count robot)))
    (set! direction 'right)
    (restart-physics)
    (set! (controller robot) nn-brain)
    (sim-time-set! (sim eracs-buffer) 0.0)
    (set! start-time 0.0)
    (set! start-position (robot-position))
    (block-until (lambda () 
                  (> (- (robot-time robot) start-time) eval-robot-time))) 
    (let* ((pos (robot-position))
           (fitness (- (vector-ref start-position 0) (vector-ref pos 0))))
      (set! (controller robot) original-brain)
      (message "Fitness ~a tick-count ~a sim-time ~a." fitness (- (tick-count robot) start-tick) (- (robot-time robot) start-time))
      fitness)))

;; no op
(define-interactive (osc-noop)
 #f)

(define-key eracs-mode-map (kbd "osc-ping") 'osc-noop)

(define-interactive
  (hill-climber 
   #:optional 
   (max-generations 
    (read-from-string (read-from-minibuffer "max-evaluations: "))))
  (message "optimizer: starting")
  ;(message "hill-climber: starting")
  (let* ((parent (if (and #f first-time)
                     (begin 
                       (set! first-time #f) 
                       (random-brain))
                     (get-nn-weights robot)))
        (parent-fitness (evaluate-robot parent))
        (child #f)
        (child-fitness 0))
   (let loop ((generation 0))
     (set! child (mutate parent))
     (set! child-fitness (evaluate-robot child))
     (message "eval ~a best-objective ~6f current-objective ~6f" generation parent-fitness child-fitness)
     (if (> child-fitness parent-fitness)
         (begin (set! parent child)
                (set! parent-fitness child-fitness)))
     (if (< generation max-generations)
      (loop (1+ generation))
      (begin (evaluate-robot parent))))))

(define (active-pref-error weights active-pref-training)
  (let* ((nn (vector->nn weights neuron-count))
         (error-squared-elements 
          (map 
           (lambda (ap-entry)
             (match ap-entry
               ((index joint-value t-offset joint-offset spread)
                (let* ((output (nn-run nn (vector t-offset 1 1 1 1)))
                       (diff (: (joint-value + joint-offset) - (output @ index)))
                       (diffsq (* diff diff)))
                  diffsq))
               (_ #f)))
         active-pref-training)))
    ;; Sum them up.
    (apply + error-squared-elements)))

(define (two-obj-fitness weights) 
  "Fitness objectives: 1) maximize distance from origin, and 2)
minimize active preference error."
  (let ((start-position #f)
        (xz-proj (vector 1. 0. 1.)))
    (define (get-start robot)
      (set! start-position (robot-position robot)))
    (define (distance-from-origin robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- start-position pos)))))
        (message "Distance ~a tick-count ~a sim-time ~a." 
                 distance (tick-count robot) (sim-time (in-sim robot)))
        distance))
    (vector (- (eval-robot-headless 
                weights 
                #:begin-fn get-start
                #:end-fn distance-from-origin))
            (active-pref-error weights active-preferences-training))))

(define (target-object-fitness weights) 
  "Fitness objectives: 1) minimize distance to target, and 2) minimize
active preference error."
  (let ((target-position #(0 1 -10))
        (xz-proj (vector 1. 0. 1.)))
    (define (distance-to-target robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- target-position pos)))))
        (message "Distance to target ~a tick-count ~a sim-time ~a." 
                 distance (tick-count robot) (sim-time (in-sim robot)))
        distance))
    (vector (eval-robot-headless weights #:end-fn distance-to-target)
            (active-pref-error weights active-preferences-training))))

(define (target-object-fitness-averaging weights) 
  "Fitness objectives: 1) minimize distance to target, and 2) minimize
active preference error."
  (let ((target-position #(0 1 -10))
        (xz-proj (vector 1. 0. 1.)))
    (define (distance-to-target robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- target-position pos)))))
        distance))
    (let-values (((accum report) (make-averaging-fns distance-to-target)))
      (define (report-and-message robot)
        (let ((distance-avg (report)))
                (message "Distance to target ~a tick-count ~a sim-time ~a." 
                         distance-avg (tick-count robot) (sim-time (in-sim robot)))
                distance-avg))
      (vector (eval-robot-headless weights 
                                   #:step-fn accum
                                   #:end-fn report-and-message)
              (active-pref-error weights active-preferences-training)))))

(define (make-averaging-fns fn)
  "Accepts a fn that when evaluated will produce a real number.  Returns two values: 1) a consuming function, and 2) a reporting function"
  (let ((n 0)
        (accum 0.))
   (values (lambda args
             (incr! accum (apply fn args))
             (incr! n 1))
           (lambda args
             (/ accum n)))))

;; I should try something like this:
;;
;; Fitness objectives: 1) minimize distance to target, and 2) minimize
;; distance to waypoint.

(define (normalized-distance start target current)
  (/ (vector-norm (vector- current target))
     (vector-norm (vector- start target))))

(define (high-level-waypoint-fitness weights)
  (let ((start-position #f)
        (target-position #(0 1 -10))
        (waypoint-position #(7 1 -5))
        (xz-proj (vector 1. 0. 1.))
        (approach-waypoint? #t)
        (leave-waypoint-position #f)
        (alpha .3))
    (define (capture-start robot)
      (set! start-position (robot-position robot)))
    (define (norm-distance-to-target robot)
      (let* 
          ((pos (robot-position robot))
           (norm-distance 
            (apply normalized-distance 
                   ;; Project only in the xz-plane (height displacement
                   ;; doesn't count).
                   (map (cut vector* xz-proj <>) 
                        (if approach-waypoint? 
                            (list start-position waypoint-position pos)
                            (list leave-waypoint-position target-position pos))))))
        (when (and approach-waypoint? (: norm-distance < alpha))
          (message "Reached waypoint")
          (set! approach-waypoint? #f)
          (set! leave-waypoint-position pos))
        norm-distance))
    (let-values (((accum report) (make-averaging-fns norm-distance-to-target)))
      (define (report-and-message robot)
        (let ((distance-avg (report)))
                (message "Distance to target ~a tick-count ~a sim-time ~a." 
                         distance-avg (tick-count robot) (sim-time (in-sim robot)))
                distance-avg))
      (vector (eval-robot-headless weights 
                                   #:begin-fn capture-start
                                   #:step-fn accum
                                   #:end-fn report-and-message)
              ;(active-pref-error weights active-preferences-training)
              ))))

(define last-pareto-front (vector))
(define last-pareto-front-index 0)
(define last-seed-fitness #f)
(define-interactive
  (optimize #:optional 
            (max-generations 
             (read-from-string (read-from-minibuffer "max-generations: "))))

  (message "nsga-ii: starting")
  (let* ((results #f)
         (fitness-fn 
          ;two-obj-fitness
          ;target-object-fitness
          ;target-object-fitness-averaging
          high-level-waypoint-fitness)
         (seed-weights (get-nn-weights robot))
         (original-fitness (fitness-fn seed-weights)))
    (set! results (nsga-ii-search fitness-fn
                                  #:objective-count (vector-length original-fitness)
                                  #:gene-count (neuron-count->weight-count neuron-count) ;504
                                  #:population-count 4
                                  #:generation-count max-generations
                                  #:seed-individual seed-weights))
    (set! last-seed-fitness original-fitness)
    (set! last-pareto-front (list->vector (sort results (lambda (a b)
                                             (: (cdr a) @ 0 > (cdr b) @ 0)))))
    ;(emacsy-event (make <key-event> #:command-char #\a))
    (message "Feasible fitnesses ~a" (map cdr results))
    ;(gnuplot-newplot )
    (set-pareto-front-index 0)
    (plot-front)
    (set! (controller robot) run-nn-brain)))

(define-interactive (plot-front)
    (define (raw-fitness->display-fitness fitness-vector)
      (vector (- (: fitness-vector @ 0))
              (: fitness-vector @ 1)))
    (gnuplot "reset")
    (gnuplot "set xlabel 'distance'")
    (gnuplot "set ylabel 'active pref error'")
    (let* ((results (vector->list last-pareto-front))
           (points (map (compose vector->list cdr) results))
           (sorted-points (sort points (lambda (a b)
                                          (< (car a) (car b)))))
           (flip-distance (map (lambda (point)
                                 (cons (- (car point)) (cdr point))) 
                               sorted-points)))
      (format #t "~a" (list (vector->list (raw-fitness->display-fitness
                                           last-seed-fitness))))
      (mathematica (format #f "Show@@Block[{$DisplayFunction = Identity},{~{ListPlot[~a, Joined->True]~^,~}}]"
                          (map list->mathematica-list* (list
                                                        flip-distance
                                                        (list (vector->list (raw-fitness->display-fitness
                                                                             last-seed-fitness)))
                                                        (list (vector->list 
                                                               (raw-fitness->display-fitness 
                                                                (cdr 
                                                                 (: last-pareto-front @ last-pareto-front-index)))))))))
      (gnuplot-plot-points 
       flip-distance
       (list (vector->list (raw-fitness->display-fitness
                            last-seed-fitness)))
       (list (vector->list 
              (raw-fitness->display-fitness 
               (cdr 
                (: last-pareto-front @ last-pareto-front-index))))))))

(define-interactive (set-pareto-front-index #:optional (index (read-from-string (read-from-minibuffer "index: "))))
  (set! last-pareto-front-index (mod index (vector-length last-pareto-front)))
  (restart-physics)
  (set! (controller robot) run-nn-brain)
  (set-nn-weights! robot (car (: last-pareto-front @ last-pareto-front-index)))
  (plot-front))

(define-interactive (next-individual)
  (set-pareto-front-index (1+ last-pareto-front-index)))

(define-interactive (prev-individual)
  (set-pareto-front-index (1- last-pareto-front-index)))

(define-key eracs-mode-map (kbd ".") 'next-individual)
(define-key eracs-mode-map (kbd ",") 'prev-individual)
