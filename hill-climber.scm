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
             (ice-9 q)
             (srfi srfi-1)  ;; take
             (srfi srfi-11) ;; let-values
             (srfi srfi-26) ;; cut cute
             (srfi srfi-4)  ;; uniform vectors
             (srfi srfi-4 gnu)  ;; uniform vectors
             (rnrs io ports)
             (mathematica plot))

(add-hook! emacsy-terminate-hook (lambda ()
                                   (mathematica-quit)))

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
  (set-nn-weights! (current-robot) (random-brain)))

(define-interactive 
  (write-brain #:optional
               (weights (get-nn-weights (current-robot)))
               (filename (read-from-minibuffer "Write brain to file: ")))
  (call-with-output-file filename 
    (lambda (port)
      ;(uniform-vector-write (any->f64vector weights) port)
      (put-bytevector port (u32vector (vector-length weights)))
      (put-bytevector port (any->f64vector weights)))))

(define-interactive 
  (read-brain #:optional
              (filename (read-from-minibuffer "Read brain from file: ")))
  (let ((count (make-u32vector 1))
        (weights #f))
   (call-with-input-file filename 
     (lambda (port)
       (get-bytevector-n! port count 0 (* 4 1))
       ;(get-bytevector-all port)
       (set! weights (make-f64vector (uniform-vector-ref count 0)))
       (get-bytevector-n! port weights 0 (* 8 (uniform-vector-ref count 0)))
       ;(uniform-vector-read! weights port)
       (when (called-interactively?)
        (set-nn-weights! (current-robot) weights))
       weights))))

(define-interactive (clear-brain)
  (set-nn-weights! (current-robot) (make-vector gene-count 0.)))

(define (evaluate-robot weights)
  (set-nn-weights! robot weights)
  (eval-robot))

(define eval-robot-time 
  30.
  ;;1.
  
  ) ;; simulated seconds

(define eval-robot-time-step
  (/ 1. 60.)
  ;;0.05
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


;; (define-interactive (eval-robot)
;;   ;; One problem was the robot's brain updates at 10 Hz which depends
;;   ;; on the tick-count.  Resetting the tick-count so that evaluations
;;   ;; are deterministic.  Doesn't fix it entirely, but does help.
;;   ;(set! (tick-count robot) 0)
;;   (let ((original-brain (controller robot))
;;         (start-position #f)
;;         (start-time (robot-time robot))
;;         (start-tick (tick-count robot)))
;;     (set! direction 'right)
;;     (restart-physics)
;;     (set! (controller robot) run-nn-brain)
;;     (sim-time-set! (sim eracs-buffer) 0.0)
;;     (set! start-time 0.0)
;;     (set! start-position (robot-position))
;;     (block-until (lambda () 
;;                   (> (- (robot-time robot) start-time) eval-robot-time))) 
;;     (let* ((pos (robot-position))
;;            (fitness (- (vector-ref start-position 0) (vector-ref pos 0))))
;;       (set! (controller robot) original-brain)
;;       (message "Fitness ~a tick-count ~a sim-time ~a." fitness (- (tick-count robot) start-tick) (- (robot-time robot) start-time))
;;       fitness)))

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
                     (get-nn-weights (current-robot))))
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



(define fitness-functions '())

#;"The macro define-fitness defines a procedure that contains information about what its results are, e.g. (define-fitness ((minimize \"x^2 - 1\")) (f genes) (: (genes @ 0) * (genes @ 0) - 1)) defines the procedure f which returns one result that which is to be minimized."
(define-syntax-public define-fitness
  (syntax-rules ()
    ((define-fitness the-objectives (name . args) . body)
     (begin (define-interactive (name . args)
              . body)
            (set-procedure-property! name 'objectives 'the-objectives)
            (cons! name fitness-functions)))))

(define (fitness? func)
  (and (procedure? func) (objectives func) #t))

(define* (fitness-desc func #:optional (port #f))
  (format port "~a ~{~d) ~as ~a~^, ~}."
          (procedure-name func)
          (apply append! (map (lambda (number objective)
                  (list number 
                        (symbol->string (car objective))
                        (cadr objective)))
                (range 1 (length (objectives func)))
                (objectives func)))))

(define (objectives fitness-func)
  (procedure-property fitness-func 'objectives))

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
        (message "Distance ~a tick-count ~a sim-time ~a." 
                 distance (tick-count robot) (sim-time (in-sim robot)))
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
                                             (vector- start-position pos)))))
        (message "Distance ~a tick-count ~a sim-time ~a." 
                 distance (tick-count robot) (sim-time (in-sim robot)))
        distance))
    (vector (- (eval-robot 
                weights 
                #:begin-fn get-start
                #:end-fn distance-from-origin))
            (active-pref-error weights active-preferences-training))))

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
                                             (vector- target-position pos)))))
        (message "Distance to target ~a tick-count ~a sim-time ~a." 
                 distance (tick-count robot) (sim-time (in-sim robot)))
        distance))
    (vector (eval-robot weights #:end-fn distance-to-target)
            (active-pref-error weights active-preferences-training))))

(define path '())

(load "colors.scm")

(define (map-sleazy f . args)
  (let ((l (length (car args))))
   (apply map f (car args) (map (cut take <> l) (cdr args)))))

(define-interactive (draw-all-robot-paths)
  (if last-pareto-front
   (map (lambda (color weights) 
          (draw-robot-path weights color))
        (vector->list colors)
        (append! (map car (vector->list last-pareto-front)) 
                 (list last-seed-weights)))
   (message "No pareto front available.")))

(define (calc-robot-path weights)
  (let ((points '())
        (capture-frequency 10)) ;; ticks
    (define (capture-position robot)
      (if (= (mod (robot-tick robot) capture-frequency) 0)
          (cons! (robot-position robot) points)))
    (eval-robot weights #:step-fn capture-position)
    points))

(define-interactive (draw-robot-path #:optional 
                         (weights (get-nn-weights (current-robot)))
                         (color #(1. 1. 1. 1.)))
  (let ((points (calc-robot-path weights)))
    (cons! (add-line (current-scene) points color) path)))

(define-interactive 
  (plot-robot-path #:optional 
                   (weights (get-nn-weights (current-robot)))
                   (filename "path-plot.pdf"))
  (let ((points (calc-robot-path weights)))
    ;; How do I get the obstacles?
    (mathematica 
     (apply format #f "Export[~a, plotRobotPathAndObstacles[~a, ~a], ImageSize -> {5, 5} inches]" (map sexp->mathematica (list filename points obstacles))))
    (when (called-interactively?)
      (preview filename))))



(define-interactive (reload-mathematica)
  (mathematica "<<\"plot-front.m\""))


(define-interactive (clear-path)
  (for-each (cut remove-actor (current-scene) <>) path)
  (set! path '()))

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

;; I should try something like this:
;;
;; Fitness objectives: 1) minimize distance to target, and 2) minimize
;; distance to waypoint.

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
        (* (if approach-waypoint?
               1
               alpha) norm-distance)))
    (let-values (((accum report) (make-averaging-fns norm-distance-to-target)))
      (define (report-and-message robot)
        (let ((distance-avg (report)))
                (message "Fitness ~a tick-count ~a sim-time ~a." 
                         distance-avg (tick-count robot) (sim-time (in-sim robot)))
                (vector distance-avg)))
      (eval-robot weights 
                  #:begin-fn capture-start
                  #:step-fn accum
                  #:end-fn report-and-message))))

(define last-fitness-func #f)
(define last-pareto-front (vector))
(define last-pareto-front-index 0)
(define last-seed-fitness #f)
(define last-seed-weights #f)

;; each element is of the form ((#<genes ...> . #<objective values...>) ...)
(define last-results '())

(define population-count 
  ;;4
  10
  )
(define generation-tick #f)

(define-interactive
  (optimize 
   #:optional 
   (fitness-fn
    (let*-values
        (((to-string from-string) (object-tracker
                                   (compose symbol->string procedure-name))))
      (from-string (completing-read
                    "Fitness function: " 
                    (map to-string fitness-functions)
                    ;:history* 'fitness-function
                    #:initial-input 
                    (and last-fitness-func (to-string last-fitness-func))))))
   (max-generations 
    (read-from-string (read-from-minibuffer "Generation count: "
                                            ;:history* 'generation-count
                                            ))))
  
  (message "nsga-ii optimizing ~a" (fitness-desc fitness-fn))
  (block-yield)
  ;; Let's the message be displayed before going into the big
  ;; optimization procedure.
  (let* ((seed-weights (get-nn-weights (current-robot)))
         (fitness-fn* (lambda (weights)
                        (with-fluids ((eval-robot-fluid eval-robot-headless))
                             (fitness-fn weights))))
         (original-fitness (fitness-fn* seed-weights))
         (objective-count (vector-length original-fitness))
         ;; Had to use with-dynamic-state to make fluids work when crossing
         ;; into C code that called Scheme code.
         (results (apply nsga-ii-search 
                   fitness-fn*
                   #:objective-count objective-count
                   #:gene-count (neuron-count->weight-count neuron-count) ;504
                   #:population-count population-count
                   #:generation-count max-generations
                   #:generation-tick-func generation-tick
                   (if (called-interactively?) 
                       (if (null? last-results)
                        (list #:seed-individual seed-weights)
                        (list #:seed-population (map car (first last-results))))
                       '()))))
    (set! last-fitness-func fitness-fn)
    (set! last-seed-fitness original-fitness)
    (set! last-seed-weights seed-weights)
    (set! results (sort! results (lambda (a b)
                                        (: (cdr a) @ 0 > (cdr b) @ 0))))
    (cons! results last-results)
    (set! last-pareto-front (list->vector results))
    (message "Feasible fitnesses ~a" (map cdr results))
    (when (called-interactively?) 
        (call-interactively 'set-pareto-front-index 0)
        (call-interactively 'plot-front))
    (set! (controller (current-robot)) run-nn-brain)))

(define ql-pid #f)
(define (ql-show filename)
  (when ql-pid
      (kill ql-pid SIGINT)
      (set! ql-pid #f))
  (let ((pid (primitive-fork)))
    (if (= pid 0)
        ;; child
        (begin
          (display "CHILD!")
          (execlp "ql" filename))
        ;; parent
        (set! ql-pid pid))))

(define (my-sleep seconds)
  (usleep (inexact->exact (* seconds 1000))))

(define (preview filename)
  "Show the file using Preview.app.  When called on a file that's already open, it will update what's shown.  Nice."
  (catch 'system-error
    (lambda ()
      (system* "myexec" "open" "-ga" "Preview.app" filename)
      (system* "myexec" "appswitch" "-fa" "preview")
      (my-sleep 0.2)
      (system* "myexec" "appswitch" "-a" "eracs"))
    (lambda (key . args)
      (mylog "hill-climber" pri-warn "Got system error"))))

(define-method (sexp->mathematica (sexp <vector>))
  (sexp->mathematica (vector->list sexp)))

(define-method (sexp->mathematica (sexp <string>))
  (format #f "\"~a\"" sexp))

(define-method (sexp->mathematica (sexp <integer>))
  (format #f "~d" sexp))

(define-method (sexp->mathematica (sexp <real>))
  (format #f "~f" sexp))

;; (define-method (sexp->mathematica (sexp <symbol>))
;;   (format #f "~a" (symbol->string sexp)))

(define-method (sexp->mathematica (sexp <list>))
  (format #f "{~{~a~^,~}}" (map sexp->mathematica sexp)))

;; XXX need to handle display of fitness better.
(define-interactive (plot-front #:optional (filename "tmp-plot.pdf"))
    (let* ((results (vector->list last-pareto-front))
           (points (map cdr results))
           ;; Sort by the first element.
           (sorted-points (sort points (lambda (a b)
                                          (< (: a @ 0) (: b @ 0)))))
           (objectives (or (objectives last-fitness-func)
                            '((minimize "objective 1")
                              (minimize "objective 2")))))
      
          (define (raw-fitness->display-fitness fitness-vector)
            (let ((adjustment (map (lambda (obj) 
                                     (case (car obj)
                                       ((maximize) -1)
                                       ((minimize) 1))) 
                                   objectives)))
              (vector* (list->vector adjustment) fitness-vector)))
          
      (mathematica 
       (apply format #f "exportPDF[~a, Show[plotFrontAndPoint[~a, ~a, ~a], AxesLabel -> ~a, AxesOrigin -> {0, 0}, PlotRange -> {{0,12},{0,12}}]];"
              (map sexp->mathematica
                   (list
                    filename
                    (map raw-fitness->display-fitness
                         sorted-points)
                    (1+ last-pareto-front-index)
                    (raw-fitness->display-fitness
                     last-seed-fitness)
                    (map cadr objectives)))))
      ;; Show the exported image.
      (when (called-interactively?)
        (call-interactively 'draw-all-robot-paths)
        (preview filename))))

(define-interactive (am-i-int)
  (message "this-command ~a, this-interactive-command ~a" this-command (fluid-ref (@@ (emacsy command) this-interactive-command)))
  (if (called-interactively?)
      (message "Yes, I was called interactively.")
      (message "No, I was not called interactively.")))

(define-interactive (am-not)
  (am-i-int))

(define-interactive 
  (set-pareto-front-index 
   #:optional 
   (index 
    (read-from-string (read-from-minibuffer "index: "))))
  (set! last-pareto-front-index (mod index (vector-length last-pareto-front)))
  (restart-physics)
  (set! (controller (current-robot)) run-nn-brain)
  (set-nn-weights! (current-robot) (car (: last-pareto-front @ last-pareto-front-index)))
  (when (called-interactively?)
   (draw-robot-path (car (: last-pareto-front @ last-pareto-front-index))
                    (: colors @ last-pareto-front-index))
   
   (plot-front)))

(define-interactive (goto-individual
                     #:optional
                     (number
                      (read-from-string (read-from-minibuffer "Individual number: "))))
  (if (or (not (number? number)) (< number 1) (> number (vector-length last-pareto-front)))
      (message "Invalid individual")
      (call-interactively 'set-pareto-front-index (1- number))))

(define-interactive (next-individual)
  (call-interactively 'set-pareto-front-index (1+ last-pareto-front-index)))

(define-interactive (prev-individual)
  (call-interactively 'set-pareto-front-index (1- last-pareto-front-index)))

(define-key eracs-mode-map (kbd ".") 'next-individual)
(define-key eracs-mode-map (kbd ",") 'prev-individual)

(define-interactive (test-read-brain)
  ;;(call-interactively 'read-brain "run3/exp-hlwp-trial1/individual-2.bin")
  (call-interactively 'read-brain "run4/individual-1.bin"))
