;; fitness-functions.scm
(use-modules ((rnrs) #:select (vector-map vector-for-each mod))
             (emacsy emacsy)
             (nsga-ii)
             (vector-math)
             (infix)
             (ice-9 q)
             (srfi srfi-1)  ;; take
             (srfi srfi-11) ;; let-values
             (srfi srfi-26) ;; cut cute
             (srfi srfi-4)  ;; uniform vectors
             (srfi srfi-4 gnu)  ;; uniform vectors
             (srfi srfi-69) ;; hash-table
             (rnrs io ports)
             (fitness)
             )

(define* (eval-robot-headless weights 
                              #:key 
                              (step-fn identity)
                              (begin-fn identity)
                              (end-fn identity)
                              (time-step eval-robot-time-step))
  "Evaluates a robot with the given NN weights. Calls hook functions
if supplied."
  (in-out 
   (mylog "hill-climber" pri-trace "BEGIN eval-robot-headless")
   (let* ((sim (make-sim))
          (robot (init-scene sim))
          (start-time #f))
     (set-nn-weights! robot weights)
     (set! direction 'right)
     (set! (controller robot) run-nn-brain)
     (begin-fn robot)
     (while (< (sim-time sim) eval-robot-time)
       ;;(message "Simulating ~a" (sim-time sim))
       (if time-step
           (sim-tick sim time-step)
           (sim-tick sim))
       (robot-tick robot)
       (step-fn robot))
     (let ((fitness (end-fn robot)))
       ;(sim-remove-robot sim robot)
       fitness))
    (mylog "hill-climber" pri-trace "END eval-robot-headless")))

(define-interactive (eval-robot-render
                     #:optional 
                     (weights (get-nn-weights robot)) 
                     #:key 
                     (begin-fn identity)
                     (step-fn identity)
                     (end-fn identity))
  "Evaluates a robot with the given NN weights. Calls hook functions if
supplied.  Renders this in a new buffer."
  (in-out (mylog "hill-climber" pri-trace "BEGIN eval-robot-render")
   (let* ((buffer (switch-to-buffer "*eval-robot*" <physics-buffer>))
          (scene (scene buffer))
          (step-count 0))
     (define (set-sim robot)
       (set! (sim buffer) (in-sim robot))
       (set! (buffer-robot buffer) robot)
       (scene-clear-physics scene)
       (scene-add-physics scene (current-sim))
       ;(last-rendered (emacsy-time))
       (begin-fn robot))
     (define (draw robot)
       (step-fn robot)
       (incr! step-count)
       (when (>= step-count eval-robot-render-speed)
         (set! step-count 0)
         (scene-update-physics scene (current-sim))
         ;; Yield so that everything can be re-rendered.
                                        ;(block-yield)
         ;; Instead of yielding why not run primitive-command-tick?
         ;; If there aren't any events, just keep running.
         (if (q-empty? event-queue)
             (block-yield)
             (primitive-command-tick))))
     (define (end robot)
       (set! (paused? buffer) #t)
       (end-fn robot))
     (set! (paused? buffer) #t)
     (eval-robot-headless weights 
                          #:begin-fn set-sim 
                          #:step-fn draw 
                          #:end-fn end))
   (mylog "hill-climber" pri-trace "END eval-robot-render")))

(define-interactive (toggle-rendering)
  (if (eq? (fluid-ref eval-robot-fluid) eval-robot-headless)
      (begin (fluid-set! eval-robot-fluid eval-robot-render)
             (message "Rendering turned on."))
      (begin (fluid-set! eval-robot-fluid eval-robot-headless)
             (message "Rendering turned off"))))

(define eval-robot-fluid (make-fluid 
                          eval-robot-headless
                         ;eval-robot-render
                          ))

(define (eval-robot . args)
  (apply (fluid-ref eval-robot-fluid) args))


;; XXX use case-lambda instead?
(define (make-averaging-fns fn)
  "Accepts a fn that when evaluated will produce a real number.  Returns two values: 1) a consuming function, and 2) a reporting function"
  (let ((n 0)
        (accum 0.))
   (values (lambda args
             (incr! accum (apply fn args))
             (incr! n 1))
           (lambda args
             (/ accum n)))))

(define-fitness
  ((maximize "distance from origin"))
  (simple-distance #:optional (weights (get-nn-weights (current-robot)))) 
  (let ((start-position #f)
        (xz-proj (vector 1. 0. 1.)))
    (define (get-start robot)
      (set! start-position (robot-position robot)))
    (define (distance-from-origin robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- start-position pos)))))
        (message "Distance ~1,2f." distance)
        distance))
    (vector (- (eval-robot 
                weights 
                #:begin-fn get-start
                #:end-fn distance-from-origin)))))

(define-fitness
  ((maximize "average distance from origin"))
  (average-distance #:optional (weights (get-nn-weights (current-robot)))) 
  (let ((start-position #f)
        (xz-proj (vector 1. 0. 1.)))
    (define (get-start robot)
      (set! start-position (robot-position robot)))
    (define (distance-from-origin robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- start-position pos)))))
        distance))
    (let-values (((accum report) (make-averaging-fns distance-from-origin)))
      (define (report-and-message robot)
        (let ((distance-avg (report)))
                (message "Distance from origin ~a tick-count ~a sim-time ~a." 
                         distance-avg (tick-count robot) (sim-time (in-sim robot)))
                distance-avg))
      (vector (- (eval-robot weights 
                             #:begin-fn get-start
                             #:step-fn accum
                             #:end-fn report-and-message))))))

(define-fitness
  ((maximize "distance from origin")
   (minimize "active preference error"))
  (ap-distance #:optional (weights (get-nn-weights (current-robot)))) 
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
                                             (vector- start-position pos))))
             (ap-err (ap-error ap-old-weights ap-given-indexed-points weights)
                     #;(active-pref-error weights active-preferences-training)
                     ))
        (message "Distance ~1,2f AP error ~1,2f." 
                 distance ap-err)
        (vector (- distance) ap-err)))
    (eval-robot 
     weights 
     #:begin-fn get-start
     #:end-fn distance-from-origin)))

(define-fitness 
  ((minimize "distance to target")
   (minimize "active preference error"))
  (ap-target #:optional (weights (get-nn-weights (current-robot)))) 
  "Fitness objectives: 1) minimize distance to target, and 2) minimize
active preference error."
  (let ((target-position #(0 1 -10))
        (xz-proj (vector 1. 0. 1.)))
    (define (distance-to-target robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- target-position pos))))
             (ap-err (ap-error ap-old-weights ap-given-indexed-points weights)))
        (message "Distance ~1,2f AP error ~1,2f." distance ap-err)
        (vector distance ap-err)))
    (eval-robot weights #:end-fn distance-to-target)))

(define-fitness 
  ((minimize "distance to target"))
  (target-distance #:optional (weights (get-nn-weights (current-robot)))) 
  "Fitness objectives: 1) minimize distance to target."
  (let ((target-position #(0 1 -10))
        (xz-proj (vector 1. 0. 1.)))
    (define (distance-to-target robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- target-position pos)))))
        (message "Distance to target ~1,2f." distance)
        (vector distance)))
    (eval-robot weights #:end-fn distance-to-target)))

(define-fitness 
  ((minimize "distance to target"))
  (average-target-distance-jump #:optional (weights (get-nn-weights (current-robot)))) 
  "Fitness objectives: 1) minimize average distance to target."
  (let ((zy-proj (vector 0. 1. 1.)))
    (define (distance-to-target robot)
      (let* ((pos (robot-position robot))
             (distance (vector-norm (vector* zy-proj 
                                             (vector- *target-position* pos)))))
        (vector distance)))
    (let-values (((accum report) (make-averaging-fns distance-to-target)))
      (define (report-and-message robot)
        (let ((distance-avg (report)))
          (message "Average distance to target ~1,2f." distance-avg)
          
          (vector distance-avg)))
      (eval-robot weights 
                  #:step-fn accum
                  #:end-fn report-and-message))))

(define-fitness 
  ((minimize "distance to target")
   (minimize "active preference error"))
  (ap-average-target-distance-jump 
   #:optional (weights (get-nn-weights (current-robot)))) 
  "Fitness objectives: 1) minimize average distance to target."
  (let ((zy-proj #(0. 1. 1.)))
    (define (distance-to-target robot)
      (let* ((pos (robot-position robot))
             (distance (vector-norm (vector* zy-proj 
                                             (vector- *target-position* pos)))))
        distance))
    (let-values (((accum report) (make-averaging-fns distance-to-target)))
      (define (report-and-message robot)
        (let ((distance-avg (report))
              (ap-err (ap-error ap-old-weights ap-given-indexed-points weights)))
          (message "Average distance to target ~1,2f and AP ~1,1f." distance-avg ap-err)
          (vector distance-avg ap-err)))
      (eval-robot weights 
                  #:step-fn accum
                  #:end-fn report-and-message))))

(define-fitness 
  ((minimize "distance to target"))
  (average-target-distance-jump 
   #:optional (weights (get-nn-weights (current-robot)))) 
  "Fitness objectives: 1) minimize average distance to target."
  (let ((zy-proj #(0. 1. 1.)))
    (define (distance-to-target robot)
      (let* ((pos (robot-position robot))
             (distance (vector-norm (vector* zy-proj 
                                             (vector- *target-position* pos)))))
        distance))
    (let-values (((accum report) (make-averaging-fns distance-to-target)))
      (define (report-and-message robot)
        (let ((distance-avg (report)))
          (message "Average distance to target ~1,2f." distance-avg)
          (vector distance-avg)))
      (eval-robot weights 
                  #:step-fn accum
                  #:end-fn report-and-message))))



(define-fitness
  ((minimize "distance to target")
   (minimize "active preference error"))
  (ap-target-averaging #:optional (weights (get-nn-weights (current-robot)))) 
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
      (vector (eval-robot weights 
                                   #:step-fn accum
                                   #:end-fn report-and-message)
              (active-pref-error weights active-preferences-training)))))

(define-fitness
  ((minimize "distance to target")
   (minimize "distance to waypoint"))
  (target-waypoint
   #:optional (weights (get-nn-weights (current-robot)))) 
  "Fitness objectives: 1) minimize distance to target, and 2) minimize
distance to waypoint."
  (let ((target-position #(0 1 -10))
        (waypoint-position #(7 1 -5))
        (xz-proj (vector 1. 0. 1.)))
    (define (distance-to-target robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- target-position pos)))))
        distance))
    (define (distance-to-waypoint robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- waypoint-position pos)))))
        distance))
    (let-values (((accum report) (make-averaging-fns distance-to-target))
                 ((accum* report*) (make-averaging-fns distance-to-waypoint)))
      (define (report-and-message robot)
        (let ((distance-avg (report))
              (distance*-avg (report*)))
                (message "Distance to target ~a; distance to waypoint ~a." 
                         distance-avg distance*-avg)
                (vector distance-avg distance*-avg)))
      (eval-robot weights 
                           #:step-fn (lambda (robot) 
                                       (accum robot)
                                       (accum* robot))
                           #:end-fn report-and-message))))

(define *waypoint-alpha* 0.3)

(define-fitness
  ((minimize "distance to target/waypoint"))
  (high-level-waypoint-fitness #:optional (weights (get-nn-weights (current-robot))))
  (define (normalized-distance start target current)
    (/ (vector-norm (vector- current target))
       (vector-norm (vector- start target))))
  (let ((start-position #f)
        (target-position #(0 1 -10))
        (waypoint-position #(7 1 -5))
        (xz-proj (vector 1. 0. 1.))
        (approach-waypoint? #t)
        (leave-waypoint-position #f)
        (alpha *waypoint-alpha*))
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
        (* (if approach-waypoint?
               1
               alpha) norm-distance)))
    (let-values (((accum report) (make-averaging-fns norm-distance-to-target)))
      (define (report-and-message robot)
        (let ((distance-avg (report)))
                (message "Average distance to target ~1,2f." distance-avg)
                (vector distance-avg)))
      (eval-robot weights 
                  #:begin-fn capture-start
                  #:step-fn accum
                  #:end-fn report-and-message))))

(define-fitness
  ((minimize "distance to target/waypoint"))
  (hlwp-jump #:optional (weights (get-nn-weights (current-robot))))
  (define (normalized-distance start target current)
    (/ (vector-norm (vector- current target))
       (vector-norm (vector- start target))))
  (let ((start-position #f)
        (waypoint-position #(0 .1 0))
        (yz-proj (vector 0. 1. 1.))
        (approach-waypoint? #t)
        (leave-waypoint-position #f)
        (alpha *waypoint-alpha*))
    (define (capture-start robot)
      (set! start-position (robot-position robot)))
    (define (norm-distance-to-target robot)
      (let* 
          ((pos (robot-position robot))
           (norm-distance 
            (apply normalized-distance 
                   (map (cut vector* yz-proj <>) 
                        (if approach-waypoint? 
                            (list start-position waypoint-position pos)
                            (list leave-waypoint-position *target-position* pos))))))
        (when (and approach-waypoint? (: norm-distance < alpha))
          (message "Reached waypoint")
          (set! approach-waypoint? #f)
          (set! leave-waypoint-position pos))
        (* (if approach-waypoint?
               1
               alpha) norm-distance)))
    (let-values (((accum report) (make-averaging-fns norm-distance-to-target)))
      (define (report-and-message robot)
        (let ((distance-avg (report)))
                (message "Average distance to target ~1,2f." distance-avg)
                (vector distance-avg)))
      (eval-robot weights 
                  #:begin-fn capture-start
                  #:step-fn accum
                  #:end-fn report-and-message))))

(define-interactive
  (robot-avoids-obstacle? #:optional (weights (get-nn-weights (current-robot)))) 
  "Return true if the robot gets within 4.5 units of the target at any point."
  (let ((target-position #(0 1 -10))
        (xz-proj (vector 1. 0. 1.)))
    (define (check-distance-to-target robot)
      (let* ((pos (robot-position robot))
             ;; Project only in the xz-plane (height displacement doesn't count).
             (distance (vector-norm (vector* xz-proj 
                                             (vector- target-position pos)))))
        (if (< distance 4.5)
            (throw 'close-enough #t distance))))
    (catch 'close-enough
      (lambda () 
        (eval-robot weights #:step-fn check-distance-to-target)
        (message "Robot did not get close enough to target.")
        #f)
      (lambda (key result distance)
        (message "Robot got close enough to target; distance ~1,2f." distance)
        result))))

(define-interactive
  (robot-crosses-gap? #:optional (weights (get-nn-weights (current-robot)))) 
  (define (on-other-side? robot)
    (let ((pos (robot-position robot))
          (ditch-end (- (+ (/ *robot-width* 2.) *ditch-width*))))
      (format #t "ditch-end ~1,1f pos ~{~1,1f, ~}~%" ditch-end (vector->list pos))
      (values (and (: pos @ 2 < ditch-end)
                   (: pos @ 1 > 0.)) 
              pos)))
  (receive (result pos) (eval-robot weights #:end-fn on-other-side?)
    (if result
        (message "Robot crossed the gap; end position ~{~1,1f~^, ~}." (vector->list pos))
        (message "Robot did not cross the gap."))
    result))

