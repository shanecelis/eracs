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
(define body-count 3) ;; 1 agent, 2 objects
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

(define h 0.5) ;; time step

(define population-count 10)

(define ctrnn (make-n-ctrnn node-count))

(define ctrnn-state (make-ctrnn-state ctrnn))

(define (go-right t i)
  (if (= i 1)
      1.
      0.))

(define (make-effector-func ctrnn-state)
  (lambda (t i)
    (let ((first-effector-index (1+ sensor-count)))
      (: ctrnn-state @ (first-effector-index + (i - 1)))
      ;0.                                ; No motors
      )))

(define effector-func (make-effector-func ctrnn-state))

(define (random-range low high)
  (if (> low high)
      (random-range high low)
      (+ (random (- high low)) low)))

(define (adjust-fode fode-params)
  (let ((n (car fode-params))
        (k (cadr fode-params)))
    ;; agent velocity
    (: k @ 0 := motor-constant)
    (: k @ 1 := 0)
    ;; object 1 velocity
    (: k @ 2 := (begin (apply random-range horizontal-velocity)))
    (: k @ 3 := (begin (apply random-range vertical-velocity-1)))
    ;; object 2 velocity
    (: k @ 4 := (begin (apply random-range horizontal-velocity)))
    (: k @ 5 := (begin (apply random-range vertical-velocity-2)))
    )
  fode-params)

(define fode (adjust-fode (make-fode body-count effector-func)))

(define (make-fode-state* fode-params)
  (let ((ty (make-fode-state fode-params)))
    ;; agent
    (: ty @ 1 := 0.)
    (: ty @ 2 := 0.)
    ;; object 1 position
    (: ty @ 3 := -2.)
    ;(: ty @ 4 := max-height)
    (generalized-vector-set! ty 4 max-height)
    ;; object 2 position
    (: ty @ 5 := 2.)
    ;(: ty @ 6 := max-height)
    (generalized-vector-set! ty 6 max-height)
    ty
    ))

(let ((orig emacsy-mode-line))
  (set! emacsy-mode-line 
        (lambda ()
        (with-buffer (recent-buffer)
          (format #f "~a sim-time ~1,1f agent (~1,1f, ~1,1f) object1 (~1,1f, ~1,1f) object2 (~1,1f, ~1,1f)" 
                  (orig) 
                  (: fode-state @ 0)
                  (: fode-state @ 1)
                  (: fode-state @ 2)
                  (: fode-state @ 3)
                  (: fode-state @ 4)
                  (: fode-state @ 5)
                  (: fode-state @ 6)
                                    
                  )))))

(define (in-range? x list)
  (and (>= x (car list))
       (<= x (cadr list))))

(define (choose-initial-conditions fode-params fode-state)
  ;; Pick the initial x1 and v1
  (let ((ty fode-state)
        (n (car fode-params))
        (k (cadr fode-params)))
      ;; Set the heights to the same thing.
    ;; object 1 yi
    (generalized-vector-set! ty 4 max-height)
    ;; object 2 yi
    (generalized-vector-set! ty 6 max-height)
    ;; object 1 position
    (: ty @ 3 := (begin (apply random-range horizontal-position)))
    ;; object 1 velocity
    (: k @ 2 := (begin (apply random-range horizontal-velocity)))
    (: k @ 3 := (begin (apply random-range vertical-velocity-1)))
    
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
          (choose-initial-conditions fode-params fode-state)))))

(define fode-state (make-fode-state* fode))

(define vision-line-actors '())
(define (draw-vision-lines agent-position end-point)
  #;(format #t "draw-vision-lines ~a ~a~%" agent-position end-point)
  (let ((scene (current-scene)))
    (when scene
      #;(format #t "draw-vision-lines add-line ~a ~a~%" agent-position end-point)
      (cons! (add-line scene (list (vector-append agent-position #(0.))
                                   (vector-append end-point #(0.))))
             vision-line-actors))))

(define current-genome (make-genome-for-n-ctrnn node-count))
(define gene-count (generalized-vector-length current-genome))

(add-hook! physics-tick-hook #.\ (my-physics-tick))
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

(define (my-physics-tick)
  (let* ((scene (current-scene)) ;; This should be attached to the buffer.
         (restart? #t))
    (when scene
      (if draw-display?
       (for-each (lambda (actor) (remove-actor scene actor)) scene-actors))
      (set! scene-actors '())
      (unless pause-fode?
        (step-fode fode-state h fode)
        (when (= 0 (mod tick-count update-ctrnn-freq))
          (if draw-display?
              (for-each (lambda (actor) (remove-actor scene actor)) vision-line-actors))
          (step-ctrnn ctrnn-state h ctrnn)))
                                        ;(format #t "~a\n" fode-state)
      
      (if draw-display?
       (cons! (add-sphere scene (vector 
                                 (object-x fode-state 0) 
                                 (object-y fode-state 0) 
                                 0)
                          (/ agent-diameter 2)
                          ) scene-actors))
      
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
                             ) scene-actors))
         #;(cons! (add-line scene 
                          (list (vector+ #(0 50 0) position)
                                (vector+ (vector (/ object-diameter 2) 50 0) position)
                                )
                          ) scene-actors)
        (if (> (object-y fode-state i) 0)
            (set! restart? #f))
        #;(format #t "drawing object ~a at position ~a~%" i position))
       ) 
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
  (message "Calling fitness.")
  (let ((d1 #f)
        (d2 #f)
        (fitness #f))
    (define (step-func fode-state)
      (let ((y0 (object-y fode-state 0))
            (y1 (object-y fode-state 1))
            (y2 (object-y fode-state 2)))
       (when (and (not d1) (: y1 < 0))
         (set! d1 (- (object-x fode-state 1) (object-x fode-state 0))))
       (when (and (not d2) (: y2 < 0))
         (set! d2 (- (object-x fode-state 2) (object-x fode-state 0))))
       ;; Stop when d1 and d2 are set.
       (not (and d1 d2))))
    (define (end-func fode-state)
      (if (and d1 d2)
          (vector (+ (abs d1) (abs d2)))
          #;(throw 'invalid-fitness)
          (vector 1000) ;; Big number for terrible fitness.
          ))
    (set! fitness (eval-beer-robot genome 
                                   #:step-fn step-func 
                                   #:end-fn end-func))
    (message "Fitness ~a." fitness)
    fitness))

(define-fitness
  ((minimize "Average distance to objects"))
  (beer-selective-attention-n #:optional (genome current-genome))
  (let ((trials 10)
        (sum 0.0))
   (do ((i 1 (1+ i)))
       ((> i trials))
     (incr! sum (vector-ref (beer-selective-attention genome) 0)))
   (vector (/ sum trials))))

(define*
  (eval-beer-robot genome
                   #:key 
                   (step-fn identity)
                   (begin-fn identity)
                   (end-fn identity)
                   (max-tick-count 500)
                   )
  (let* ((ctrnn (make-n-ctrnn node-count))
         (ctrnn-state (make-ctrnn-state ctrnn))
         (effector-func (make-effector-func ctrnn-state))
         (fode (adjust-fode (make-fode body-count effector-func)))
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
    (genome->ctrnn genome ctrnn)
    (set! (input-func ctrnn) vision-input)
    (begin-fn fode-state)
    (while (and 
            (< tick-count max-tick-count) 
            (step-fn fode-state))
      (if (= 0 (mod tick-count update-ctrnn-freq))
          (step-ctrnn ctrnn-state h ctrnn))
      (step-fode fode-state h fode)
      (step-fn fode-state)
      (incr! tick-count)
      #;(format #t "Tick ~a~%" tick-count))
    (end-fn fode-state)))

(define last-fitness-func #f) 

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
                                        (: (cdr a) @ 0 > (cdr b) @ 0))))
    (set! current-genome (caar results))
    (genome->ctrnn current-genome ctrnn)
    (reset-fode)
    (message "Feasible fitnesses ~a" (map cdr results))
    #;
    (when (called-interactively?) 
      (call-interactively 'set-pareto-front-index 0)
      (call-interactively 'plot-front))
    #;(set! (controller (current-robot)) run-nn-brain)
    ))

;(optimize beer-selective-attention 1)

