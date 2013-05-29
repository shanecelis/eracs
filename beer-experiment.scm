;; beer-experiment.scm
;;
;; replication of the selecive attention experiment
(use-modules 
 (minimal-cognition ctrnn)
 (minimal-cognition fode)
 (minimal-cognition vision)
 (nsga2) 
 (fitness)
 (infix)
 (srfi srfi-11) ;; let-values
)

(define (scale-by a list)
  (map (lambda (x) (* a x)) list))

(define sensor-count 9)
(define internode-count 10)
(define effector-count 2)
(define node-count (+ sensor-count internode-count effector-count))
;(define body-count 3) ;; 1 agent, 2 objects
(define body-count 2)
(define agent-diameter 30)
(define object-diameter 26)
(define motor-constant 5)
(define max-sight-distance 205)
(define visual-angle (/ pi 6))
(define max-sight-output 10)
(define max-height 180.)
(define horizontal-position (scale-by 80 '(-1. 1.)))
;; This is what the paper says, but it then will take max-height
;; seconds for object 2 to actually fall, which seems pretty lengthly.
(define horizontal-velocity (scale-by 2 '(-2. 2.)))
(define vertical-velocity-1 (scale-by -3 '(3. 4.)))
(define vertical-velocity-2 (scale-by -3 '(1. 2.)))

(define horizontal-velocity (scale-by 1 '(-2. 2.)))
(define vertical-velocity-1 (scale-by -1 '(3. 4.)))
(define vertical-velocity-2 (scale-by -1 '(1. 2.)))
(define alpha 0.7)

(define scene-actors '())

(define h 0.1) ;; time step

(define population-count 12)

(define ctrnn (make-n-ctrnn node-count))
(define ctrnn-state (make-ctrnn-state ctrnn))

(define (go-right t i)
  (if (= i 1)
      1.
      0.))

(define (make-effector-func ctrnn-state)
  (lambda (t i)
    (let ((first-effector-index (1+ sensor-count)))
      (: ctrnn-state @ (first-effector-index + (i - 1))))))

(define (random-range low high)
  (if (> low high)
      (random-range high low)
      (+ (random (- high low)) low)))

(define fode (make-fode body-count (make-effector-func ctrnn-state)))

(define (make-fode-state* fode-params)
  "Make an FODE state and initialize it to some fixed values."
  (let ((ty (make-fode-state fode-params)))
    ;; agent
    (set! (object-x ty 0) 0.)
    (set! (object-y ty 0) 0.)
    ;; object 1 position
    (for-each (lambda (i)
                (set! (object-x ty i) (+ i -3.))
                (set! (object-y ty 1) max-height))
              (range 1 (1- body-count)))
    ty))

(let ((orig emacsy-mode-line))
  (set! emacsy-mode-line 
        (lambda ()
        (with-buffer (recent-buffer)
          (format #f "~a sim-time ~1,1f agent (~1,1f, ~1,1f)~{ object (~1,1f, ~1,1f)~}"; " object2 (~1,1f, ~1,1f)" 
                  (orig)
                  (fode-time fode-state)
                  (object-x fode-state 0)
                  (object-y fode-state 0)
                  (append-map (lambda (i)
                                (list (object-x fode-state i)
                                      (object-y fode-state i)))
                       (range 1 (1- body-count))))))))

(define (in-range? x list)
  (and (>= x (car list))
       (<= x (cadr list))))


(define (beer-choose-initial-conditions fode-params fode-state)
  "This is how beer chooses his initial conditions for 1 or 2 objects."
  ;; Pick the initial x1 and v1
  (let ((ty fode-state)
        (n (car fode-params))
        (k (cadr fode-params)))
      ;; Set the heights to the same thing.
    ;; object 1 yi
    (set! (object-y ty 1) max-height)
    ;; object 1 position
    (set! (object-x ty 1) (apply random-range horizontal-position))
    ;; object 1 velocity
    (set! (object-vx k 1) (apply random-range horizontal-velocity))
    (set! (object-vy k 1) (apply random-range vertical-velocity-1))
    
    (unless (= n 2)
      ;; object 2 yi
      (set! (object-y ty 2) max-height)
      ;; Compute x1
      (let* ((t1 (/ (: ty @ 4) (: k @ 3)))
             (x1 (+ (: ty @ 3) (* (: k @ 4) t1)))
             (v2y (apply random-range vertical-velocity-2))
             (t2 (/ (: ty @ 6) v2y))
             (beta (* motor-constant alpha (abs (- t1 t2))))
             (x2 (random-range (- x1 beta) (+ beta x1)))
             (v2x (apply random-range horizontal-velocity))
             (x2i (- x2 (* v2x t2)))
             )
        ;; object 2 position
        (: ty @ 5 := x2i)
        ;; object 2 velocity
        (: k @ 4 := v2x) ;; vx
        (: k @ 5 := v2y) ;; vy
        (if (in-range? x2i horizontal-position)
            (message "Initial conditions: object 1 r (~1,1f, ~1,1f) v (~1,1f, ~1,1f) object 2 r (~1,1f, ~1,1f) v (~1,1f, ~1,1f)" (: ty @ 3) (: ty @ 4) (: k @ 2) (: k @ 3) (: ty @ 5) (: ty @ 6) (: k @ 4) (: k @ 5))
            (beer-choose-initial-conditions fode-params fode-state))))))

(define (case-1-IC fode-params fode-state)
  (let ((n (car fode-params))
        (k (cadr fode-params)))
    (if (> n 2)
        (throw 'invalid-object-count n))
    (set! (object-x fode-state 1) 0.)
    (set! (object-y fode-state 1) max-height)
    (set! (object-vx k 1) 0.)
    (set! (object-vy k 1) (car vertical-velocity-1))))

(define (case-2-IC fode-params fode-state)
  (let ((n (car fode-params))
        (k (cadr fode-params)))
    (if (> n 2)
        (throw 'invalid-object-count n))
    (set! (object-x fode-state 1) 0.)
    (set! (object-y fode-state 1) max-height)
    (set! (object-vx k 1) (car horizontal-velocity))
    (set! (object-vy k 1) (car vertical-velocity-1))))

(define (case-3-IC fode-params fode-state)
  (let ((n (car fode-params))
        (k (cadr fode-params)))
    (if (> n 2)
        (throw 'invalid-object-count n))
    (set! (object-x fode-state 1) 0.)
    (set! (object-y fode-state 1) max-height)
    (set! (object-vx k 1) (cadr horizontal-velocity))
    (set! (object-vy k 1) (car vertical-velocity-1))))

(define (case-4-IC fode-params fode-state)
  (let ((n (car fode-params))
        (k (cadr fode-params)))
    (for-each (lambda (i)
                (set! (object-x fode-state i) (apply random-range horizontal-position))
                (set! (object-y fode-state i) max-height)

                (set! (object-vx k i) (apply random-range horizontal-velocity))
                (set! (object-vy k i) (apply random-range vertical-velocity-1)))
              (range 1 (1- body-count)))))


;(define choose-initial-conditions beer-choose-initial-conditions)
(define choose-initial-conditions case-4-IC)

(define fode-state (make-fode-state* fode))

(define vision-line-actors #f)
(define vision-line-actors-index 0)
(define (draw-vision-lines agent-position end-point)
  #;(format #t "draw-vision-lines ~a ~a~%" agent-position end-point)
  (let ((scene (current-scene)))
    (when (or (not vision-line-actors) 
              (not (= (vector-length vision-line-actors) sensor-count)))
      ;; initialize the actors
      (set! vision-line-actors (make-vector sensor-count #f)))
    (when scene
      #;(format #t "draw-vision-lines add-line ~a ~a~%" agent-position end-point)
      (if (not (: vision-line-actors @ vision-line-actors-index))
          (vector-set! vision-line-actors vision-line-actors-index 
                       (add-line scene (list (vector-append agent-position #(0.))
                                             (vector-append end-point #(0.)))))
          (update-line (: vision-line-actors @ vision-line-actors-index)
                       (list (vector-append agent-position #(0.))
                             (vector-append end-point #(0.)))))
      (set! vision-line-actors-index 
            (mod (1+ vision-line-actors-index) sensor-count)))))

(define current-genome (make-genome-for-n-ctrnn node-count))
(define gene-count (generalized-vector-length current-genome))

(add-hook! physics-tick-hook #.\ (fode-physics-tick))
(define tick-count 0)

(define update-ctrnn-freq 1)

;; Instead of just having local variables, shouldn't we just have
;; buffer variables?

(define draw-display? #t)
(define pause-fode? #f)

(define-interactive (toggle-draw-display)
  (set! draw-display? (not draw-display?)))

(define-key eracs-mode-map (kbd "d")   'toggle-draw-display)

(define-interactive (toggle-pause-fode)
  (set! pause-fode? (not pause-fode?)))

(define-key eracs-mode-map (kbd "p")   'toggle-pause-fode)

(define (draw-fode scene fode-state)
  ;; Draw the agent.
  (cons! (add-sphere scene (vector 
                            (object-x fode-state 0) 
                            (object-y fode-state 0) 
                            0)
                     (/ agent-diameter 2)) scene-actors)
  ;; Draw the objects.
  (for-each 
   (lambda (i)
     (let ((position (vector (object-x fode-state i) 
                             (object-y fode-state i) 
                             0)))
       (if draw-display?
           (cons! (add-sphere scene 
                              position
                              (/ object-diameter 2)
                              #;object-diameter
                              #;(* .9 object-diameter)
                              #(0 0 0 1)
                              ) scene-actors)))) 
   (range 1 (1- body-count))))

(define (fode-physics-tick)
  (let* ((scene (current-scene)) ;; This should be attached to the buffer.
         (restart? #t))
    (when scene
      (if draw-display?
       (for-each (lambda (actor) (remove-actor scene actor)) scene-actors))
      (set! scene-actors '())
      (unless pause-fode?
        (step-fode fode-state h fode)
        (when (= 0 (mod tick-count update-ctrnn-freq))
          #;(if draw-display?
              (for-each (lambda (actor) (remove-actor scene actor)) vision-line-actors))
          (step-ctrnn ctrnn-state h ctrnn)))
      (if draw-display?
          (draw-fode scene fode-state))
      
      ;; Check if we should restart the simulation.
      (for-each 
       (lambda (i)
         (if (> (object-y fode-state i) 0)
             (set! restart? #f))) 
       (range 1 (1- body-count)))

      (when restart?
        ;; restart
        (reset-fode)))
    (incr! tick-count)))

(define (make-current-vision-input)
  #;(make-vision-input fode-state
                                        (1- body-count) ;; object count
                                        sensor-count
                                        ;; We're going to treat the
                                        ;; agent-diameter as though it
                                        ;; is zero for vision
                                        ;; purposes.
                                        0 
                                        ;agent-diameter
                                        (/ object-diameter 2)
                                        max-sight-distance
                                        visual-angle
                                        max-sight-output
                                        draw-vision-lines
                                        )
  ;; Here's how to use the C implementation of vision.
   
    (list 'c-vision-input fode-state
                                        (1- body-count) ;; object count
                                        sensor-count
                                        ;; We're going to treat the
                                        ;; agent-diameter as though it
                                        ;; is zero for vision
                                        ;; purposes.
                                        0 
                                        ;agent-diameter
                                        (/ object-diameter 2)
                                        max-sight-distance
                                        visual-angle
                                        max-sight-output
                                        draw-vision-lines
                                        )
  )

(define-interactive (reset-fode)
  (genome->ctrnn current-genome ctrnn)
  (randomize-ctrnn-state! ctrnn-state)
  (set! fode (adjust-fode (make-fode body-count (make-effector-func ctrnn-state))))
  (set! fode-state (make-fode-state* fode))
  (choose-initial-conditions fode fode-state)
  (set! (input-func ctrnn) (make-current-vision-input))
  ;(randomize-genome! current-genome)
  ;(genome->ctrnn current-genome ctrnn)
  ;(set! (input-func ctrnn) vision-input)
  ;(set! ctrnn-state (make-ctrnn-state ctrnn))
  ;(set! effector-func (make-effector-func ctrnn-state))
  )


(define-interactive (randomize-brain)
  (randomize-genome! current-genome)
  (genome->ctrnn current-genome ctrnn)
  (set! (input-func ctrnn) (make-current-vision-input))
  (randomize-ctrnn-state! ctrnn-state)
  ;;(list-set! fode 2 (make-effector-func ctrnn-state))
  )

(add-hook! post-window-open-hook 
           (lambda ()
             (set-parameter! 'camera-position (vector 0 (/ max-height 2) 300))
             (set-parameter! 'camera-target (vector 0 (/ max-height 2) 0))
             (set-parameter! 'camera-up #(0 1 0))
             (randomize-genome! current-genome)
             (reset-fode)) 
           #t)

(define-fitness
  ((minimize "Distance to objects"))
  (beer-selective-attention #:optional (genome current-genome))
  (let ((d (make-vector body-count #f))
        (fitness #f))
    ;; Set the agent body to a distance of zero.
    (define (step-func fode-state)
      (for-each 
       (lambda (i)
         (when (and (not (vector-ref d i)) 
                    (< (object-y fode-state i) 0))
           (vector-set! d i (- (object-x fode-state i) 
                               (object-x fode-state 0)))))
       (range 1 (1- body-count)))
      
      ;; Return true if we have distances set for all objects.
      (not (vector-every identity d)))
    (define (end-func fode-state)
      #;(format #t "d = ~a~%" d)
      (if (vector-every identity d)
          (vector (vector-sum (vector-map abs d)))
          #;(throw 'invalid-fitness)
          (vector 10000) ;; Big number for terrible fitness.
          ))
    ;; We set the distance for the agent (object 0) to 0.
    (vector-set! d 0 0.)
        
    (set! fitness (eval-beer-robot genome 
                                   #:step-fn step-func 
                                   #:end-fn end-func))
    #;(message "Fitness ~a." fitness)
    fitness))

(define initial-conditions (list case-1-IC case-2-IC case-3-IC))

(define-fitness
  ((minimize "Average distance to objects for different initial conditions."))
  (beer-selective-attention-n #:optional (genome current-genome))
  (let ((trials (length initial-conditions))
        (last-body-count body-count)
        (last-IC choose-initial-conditions)
        (sum 0.0))
    (set! body-count 2)
    (do ((i 1 (1+ i)))
        ((> i trials))
      (set! choose-initial-conditions (list-ref initial-conditions (1- i)))
      (incr! sum (vector-ref (beer-selective-attention genome) 0)))
   (set! choose-initial-conditions last-IC)
   (set! body-count last-body-count)
   (vector (/ sum trials))))

(define init-ctrnn-state (make-ctrnn-state ctrnn))
(randomize-ctrnn-state! init-ctrnn-state)

(define*
  (eval-beer-robot genome
                   #:key 
                   (step-fn identity)
                   (begin-fn identity)
                   (end-fn identity)
                   (max-tick-count 2000)
                   )
  (let* ((ctrnn (make-n-ctrnn node-count))
         (ctrnn-state (make-ctrnn-state ctrnn))
         (effector-func (make-effector-func ctrnn-state))
         (fode (make-fode body-count effector-func))
         (fode-state (make-fode-state* fode))
         (vision-input (list 'c-vision-input fode-state
                                          (1- body-count) ;; object count
                                          sensor-count
                                          ;; We're going to treat the
                                          ;; agent-diameter as though it
                                          ;; is zero for vision
                                          ;; purposes.
                                          0 
                                          ;agent-diameter
                                          (/ object-diameter 2)
                                          max-sight-distance
                                          visual-angle
                                          max-sight-output))
         (tick-count 0))
    (choose-initial-conditions fode fode-state)
    ;(randomize-ctrnn-state! ctrnn-state)
    ;(vector-move-left! init-ctrnn-state 0 (vector-length init-ctrnn-state) ctrnn-state 0)
    (array-copy! init-ctrnn-state ctrnn-state)
    (genome->ctrnn genome ctrnn)
    (set! (input-func ctrnn) vision-input)
    #;(format #t "begin state ~a~%~%" (vector-sum ctrnn-state))
    (begin-fn fode-state)
    (while (and 
            (< tick-count max-tick-count) 
            (step-fn fode-state))
      (if (= 0 (mod tick-count update-ctrnn-freq))
          (if (not (step-ctrnn ctrnn-state h ctrnn))
              (throw 'step-ctrnn-error)))
      (if (not (step-fode fode-state h fode))
          (throw 'step-fode-error))
      (step-fn fode-state)
      (incr! tick-count)
      #;(format #t "Tick ~a~%" tick-count))
    #;(format #t "after state ~a~%" (vector-sum ctrnn-state))
    (end-fn fode-state)))

(define last-fitness-func #f) 
(define last-results #f)
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
                                            )))
   (seed-population (list current-genome)))
  "Optimizes the given fitness function for a certain number of
generations with a given seed population.  The results are a list
of ((genome . fitness) ...) sorted in ascending order of the first
objective. Genome and fitness are #f64 arrays."
  (message "nsga-ii optimizing ~a" (fitness-desc fitness-fn))
  (if (called-interactively?) 
      ;; Let's the message be displayed before going into the big
      ;; optimization procedure.
      (block-yield))

  (let* (#;
         (fitness-fn* 
          (lambda (weights)
            (with-fluids ((eval-robot-fluid eval-robot-headless))
              (fitness-fn weights))))
         (objective-count (length (objectives fitness-fn)))
         ;; Had to use with-dynamic-state to make fluids work when crossing
         ;; into C code that called Scheme code.
         (results (nsga-ii-search 
                   fitness-fn
                   #:objective-count objective-count
                   #:gene-count gene-count
                   #:population-count population-count
                   #:generation-count max-generations
                   #:seed-population seed-population)))
    ;; Get rid of any duplicate individuals.
    #;(set! results (uniq results))
    (set! last-fitness-func fitness-fn)
    (set! results (sort! results (lambda (a b)
                                   ;; XXX The < or > needs to be used
                                   ;; in reference to whether this is
                                   ;; being minimized or maximized.
                                   (: (cdr a) @ 0 < (cdr b) @ 0))))
    (set! last-results results)

    (set! current-genome (caar results))
    (genome->ctrnn current-genome ctrnn)
    (reset-fode)
    #;(message "Feasible fitnesses ~a" (map cdr results))
    results
    #;
    (when (called-interactively?) 
      (call-interactively 'set-pareto-front-index 0)
      (call-interactively 'plot-front))
    #;(set! (controller (current-robot)) run-nn-brain)
    ))

(define (generation-count-to-do my-initial-conditions)
  "Determine the number of generations required to succeed at the
given tasks."
  (let ((fitness-fn beer-selective-attention-n)
        (best-fitness 1000.)
        (results '())
        (gen-count 0)
        (last initial-conditions)
        (successful-distance (+ (/ object-diameter 2)
                                (/ agent-diameter 2))))
    (set! initial-conditions my-initial-conditions)
    (while (> best-fitness successful-distance)
      (set! results (optimize fitness-fn 1 (map car results)))
      (set! best-fitness (generalized-vector-ref (cdar results) 0))
      (incr! gen-count))
    (set! initial-conditions last)
    gen-count))

(define (mean lst)
  (exact->inexact (/ (apply + lst) (length lst))))

(define (std lst)
  (define (square x) (* x x))
  (exact->inexact (sqrt (- (mean (map square lst)) (square (mean lst))))))

;(optimize beer-selective-attention 1)

