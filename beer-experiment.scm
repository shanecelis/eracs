;; beer-experiment.scm
;;
;; replication of the selecive attention experiment
(use-modules 
 (minimal-cognition ctrnn)
 ;(minimal-cognition fode)
 ((minimal-cognition fode)
             #:renamer (symbol-prefix-proc 'fode:))
 (minimal-cognition vision)
 (minimal-cognition unified-procedure)
 (system foreign)
 (nsga-ii) 
 (fitness)
 (infix)
 (srfi srfi-19)
 (srfi srfi-8) ;; receive
 (srfi srfi-11) ;; let-values
 (mathematica-plot plot)
 (freeze-random)
 (mathematica)
 (ice-9 q)
 (ice-9 match)
 (emacsy emacsy)
 (emacsy core)
 (oop goops)
 )

(define physics-class <fode-physics>)
;(define physics-class <bullet-physics>)
;(define physics-class <bullet-physics-car>)


(define-interactive (toggle-physics)
  (if (eq? physics-class <fode-physics>)
      (set! physics-class <bullet-physics-car>)
      (set! physics-class <fode-physics>))
  (reset-fode))

(define (make-cycle list func)
  (let ((i 0)
        (v (list->vector list)))
    (lambda* (#:optional (incr 1))
             (format #t "BEFORE i = ~a~%" i)
             (set! i (modulo (+ i incr) (vector-length v)))
             (format #t "i = ~a~%" i)
             (func (vector-ref v i)))))

(define temp (make-cycle
  (list <fode-physics> <bullet-physics-car>)
  (lambda (x)
    (set! physics-class x)
    (reset-fode))))

;; XXX There is a bug here because I can't get rid of temp.  The macro
;; won't behave.  Adding new define-*s is tricky, maybe not worth the
;; effort.
(define-interactive cycle-physics
  temp)


(define ctrnn (make-n-ctrnn node-count))
(define ctrnn-state (make-ctrnn-state ctrnn))

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

(define-syntax record-time*
  (syntax-rules ()
    ((_ var body)
     (begin 
       (let ((start (current-time time-monotonic)))
         body
         (set! var (let* ((diff (time-difference! (current-time time-monotonic) start))
                          (seconds (time-second diff))
                          (nanoseconds (time-nanosecond diff)))
                     (time->float diff)
                     )))))))

(define (time->float time)
  (string->number (format #f "~a.~a" (time-second time) (time-nanosecond time))))

(define (make-effector-func-proc ctrnn-state)
  (lambda (t i . rest)
    (let ((first-effector-index (1+ sensor-count)))
      (: ctrnn-state @ (first-effector-index + (i - 1))))))

(define (make-effector-func-unified ctrnn-state)
  (make-unified-procedure double 
                          (lambda (t i . rest)
                            (let ((first-effector-index (1+ sensor-count)))
                              (: ctrnn-state @ (first-effector-index + (i - 1)))))
                          (list double int '*)))

(define (make-c-effector-func ctrnn-state)
  (make-unified-procedure double 
                          fode:c-ctrnn-effector
                          (list double int (list '* (bytevector->pointer ctrnn-state)))))

(define* (time-to-arrival pos vel #:optional (y1 0.))
  "Return the amount of time until an object at #(x0 y0) with velocity #(vx vy) arrives at y1."
  (let* ((y0 (: pos @ 1))
         (vy (: vel @ 1))
         (t (if (= vy 0.)
                0.
                (/ (- y1 y0) vy)))
         (x0 (: pos @ 0))
         (vx (: vel @ 0))
         (x1 (+ x0 (* vx t))))
    (list t x1)))


;(define make-effector-func make-effector-func-unified)
(define make-effector-func make-c-effector-func)

(define (random-range low high)
  (if (> low high)
      (random-range high low)
      (+ (random (- high low)) low)))

(define fode #f)

(define ;(make-fode-state* fode-params)
  (fix-physics physics)
  "Make an FODE state and initialize it to some fixed values."
  (let ((ty physics))
    ;; agent
    (set! (object-x ty 0) 0.)
    (set! (object-y ty 0) 0.)
    ; NEVER set the initial velocity, it's actually a parameter for the input.
    (set! (object-vx ty 0) 0.)
    (set! (object-vy ty 0) 0.)
    (agent-motor-constant-set! ty motor-constant)

    ;; object positions
    (for-each (lambda (i)
                (set! (object-x ty i) (+ i -3.))
                (set! (object-y ty 1) max-height))
              (range 1 (1- (object-count physics))))
    physics))

;; XXX huh, have to be careful about when define-method-public is
;; called; here using it instead of define-method break it.
(define-method (emacsy-mode-line (buffer <physics-buffer>))
  (if fode-state
      (format #f "~a sim-time ~1,1f agent (~1,1f, ~1,1f)~{ object (~1,1f, ~1,1f)~}"
              (next-method)
              (get-time fode-state)
              (object-x fode-state 0)
              (object-y fode-state 0)
              (append-map (lambda (i)
                            (list (object-x fode-state i)
                                  (object-y fode-state i)))
                          (range 1 (1- body-count))))
      (next-method)))

#;(let ((orig emacsy-mode-line))
  (set! emacsy-mode-line 
        (lambda ()
        (with-buffer (recent-buffer)
                     (if fode-state
                      (format #f "~a sim-time ~1,1f agent (~1,1f, ~1,1f)~{ object (~1,1f, ~1,1f)~}" ; " object2 (~1,1f, ~1,1f)" 
                              (orig)
                              (get-time fode-state)
                              (object-x fode-state 0)
                              (object-y fode-state 0)
                              (append-map (lambda (i)
                                            (list (object-x fode-state i)
                                                  (object-y fode-state i)))
                                          (range 1 (1- body-count))))
                      (orig))))))

(define (in-range? x list)
  (and (>= x (car list))
       (<= x (cadr list))))

;; the initial conditions might be defined like this:
;; generate-IC :: int -> alist
;; gather-IC :: fode-params -> alist
;; apply-IC! :: (alist, fode-params) -> undefined


(define* (generate-IC #:optional (object-count body-count))
  (let ((rs (map
             (lambda (i)
               (vector (apply random-range horizontal-position)
                       max-height)) 
             (range 0 (- object-count 2))))
        (vs (map
             (lambda (i)
               (vector 
                (apply random-range horizontal-velocity)
                (apply random-range (if (odd? i) 
                                        vertical-velocity-1
                                        vertical-velocity-2))))
             (range 0 (- object-count 2)))))
    `((position . ,(cons #(0 0) rs))
      (velocity . ,(cons #(0 0) vs)))))

(define* (generate-catchable-IC #:optional 
                                (object-count body-count)
                                (max-speed motor-constant)
                                (max-tries 10000))
  (let ((params (generate-IC object-count))
        (tries 0))
    (while (not (can-catch-objects? params max-speed))
      (if (> tries max-tries)
          (throw 'too-many-tries-for-generate-catchable-IC object-count max-speed max-tries))
      (set! params (generate-IC object-count))
      (incr! tries))
    params))

(define (speeds-to-catch-objects params)
  "Return the speeds required to catch all the objects."
  (let* ((rs (assq-ref params 'position))
         (vs (assq-ref params 'velocity))
         
         (t-x1s (map time-to-arrival rs vs))
         (speeds #f))
    
    (set! t-x1s (sort! t-x1s (lambda (a b) (< (car a) (car b)))))
    (set! speeds (map
                  (match-lambda*
                   (((t1 x1) (t2 x2))
                    (/ (- x2 x1) (- t2 t1))))
                  t-x1s (cdr t-x1s)))
    (map abs speeds)))

(define* (can-catch-objects? params #:optional (max-speed (- motor-constant 1.)))
  (< (apply max (speeds-to-catch-objects params)) max-speed))

(define (gather-IC fode-params)
  (let* ((n (object-count fode-params))
         (rs (map (lambda (i)
                (vector (object-x fode-params i)
                        (object-y fode-params i))) (iota n)))
         (vs (map (lambda (i)
                (vector (object-vx fode-params i)
                        (object-vy fode-params i))) (iota n))))
    `((position . ,rs)
      (velocity . ,vs))))

(define (make-apply-IC alist)
  (lambda (fode-params . rest)
   (let ((n (object-count fode-params))
         (rs (assq-ref alist 'position))
         (vs (assq-ref alist 'velocity)))
     (if (not (= n (length rs)))
         (throw 'wrong-object-count fode-params alist))
     (map (lambda (i r v)
            (unless (= i 0)
              (set! (object-x fode-params i) (vector-ref r 0))
              (set! (object-y fode-params i) (vector-ref r 1))
              (set! (object-vx fode-params i) (vector-ref v 0))
              (set! (object-vy fode-params i) (vector-ref v 1)))) 
          (iota n) rs vs))))

(define (beer-choose-initial-conditions fode-params fode-state)
  "This is how beer chooses his initial conditions for 1 or 2 objects."
  ;; Pick the initial x1 and v1
  (let ((ty (fp:state fode-state))
        (n (object-count fode-params))
        #;(k  (fp:k-params fode-params)))
      ;; Set the heights to the same thing.
    ;;(format #t "choosing ICs~%")
    ;; object 1 yi
    (set! (object-y fode-params 1) max-height)
    ;; object 1 position
    (set! (object-x fode-params 1) (apply random-range horizontal-position))
    ;; object 1 velocity
    (set! (object-vx fode-params 1) (apply random-range horizontal-velocity))
    (set! (object-vy fode-params 1) (apply random-range vertical-velocity-1))
    
    ;(format #t "N is ~a~%" n )
    (when (= n 3)
      ;; object 2 yi
      (set! (object-y fode-state 2) max-height)
      ;; Compute x1
      (let* ((t1 (/ 
                  #;(: ty @ 4) 
                  (object-y fode-state 1)
                  #;(: k @ 3)
                  (object-vy fode-state 1)))
             (x1 (+ #;(: ty @ 3) 
                  (object-x fode-state 1)
                  (* #;(: k @ 4) (object-vx fode-state 2)
                                  t1)))
             (v2y (apply random-range vertical-velocity-2))
             (t2 (/ #;(: ty @ 6) (object-y fode-state 2) v2y))
             (beta (* motor-constant alpha (abs (- t1 t2))))
             (x2 (random-range (- x1 beta) (+ beta x1)))
             (v2x (apply random-range horizontal-velocity))
             (x2i (- x2 (* v2x t2)))
             )
        ;; object 2 position
        (set! (object-x fode-state 2) x2i)
        ;; (: ty @ 5 := x2i)
        ;; object 2 velocity
        (set! (object-vx fode-state 2) v2x)
        (set! (object-vy fode-state 2) v2y)
        ;; (: k @ 4 := v2x) ;; vx
        ;; (: k @ 5 := v2y) ;; vy
        (if (in-range? x2i horizontal-position)
            (message "Initial conditions: object 1 r (~1,1f, ~1,1f) v (~1,1f, ~1,1f) object 2 r (~1,1f, ~1,1f) v (~1,1f, ~1,1f)" 
                     ;(: ty @ 3) (: ty @ 4) 
                     (object-x fode-state 1) (object-y fode-state 1) 
                     
                     ;(: k @ 2) (: k @ 3) 
                     (object-vx fode-state 1)  (object-vy fode-state 1)
                     ;(: ty @ 5) (: ty @ 6) 
                     (object-x fode-state 2) (object-y fode-state 2) 
                     ;(: k @ 4) (: k @ 5)
                     (object-vx fode-state 2)  (object-vy fode-state 2)
                     )
            (beer-choose-initial-conditions fode-params fode-state))))))

(define (case-1-IC fode-params fode-state)
  (let ((n (object-count fode-params))
        (k fode-params))
    (if (> n 2)
        (throw 'invalid-object-count n))
    (set! (object-x fode-state 1) 0.)
    (set! (object-y fode-state 1) max-height)
    (set! (object-vx k 1) 0.)
    (set! (object-vy k 1) (car vertical-velocity-1))))

(define (case-2-IC fode-params fode-state)
  (let ((n (object-count fode-params))
        (k fode-params))
    (if (> n 2)
        (throw 'invalid-object-count n))
    (set! (object-x fode-state 1) 0.)
    (set! (object-y fode-state 1) max-height)
    (set! (object-vx k 1) (car horizontal-velocity))
    (set! (object-vy k 1) (car vertical-velocity-1))))

(define (case-3-IC fode-params fode-state)
  (let ((n (object-count fode-params))
        (k fode-params))
    (if (> n 2)
        (throw 'invalid-object-count n))
    (set! (object-x fode-state 1) 0.)
    (set! (object-y fode-state 1) max-height)
    (set! (object-vx k 1) (cadr horizontal-velocity))
    (set! (object-vy k 1) (car vertical-velocity-1))))

(define (case-4-IC fode-params fode-state)
  (let ((n (object-count fode-params))
        (k fode-params))
    (set! (object-x fode-state 0) 0)
    (set! (object-y fode-state 0) 0)
    (set! (object-vx fode-state 0) 0)
    (set! (object-vy fode-state 0) 0)
    (for-each (lambda (i)
                (set! (object-x fode-state i) 
                      (apply random-range horizontal-position))
                (set! (object-y fode-state i) max-height)

                (set! (object-vx k i) (apply random-range horizontal-velocity))
                (set! (object-vy k i) (apply random-range vertical-velocity-1)))
              (range 1 (1- n)))))

(define (make-parametric-IC position-difference)
  (lambda (fode-params fode-state)
   (let ((n (object-count fode-params))
         (k fode-params))
     (for-each (lambda (i)
                 (set! (object-x fode-state i) (* (1- i) position-difference))
                 (set! (object-y fode-state i) max-height)
                 (set! (object-vx k i) 0.)
                 (set! (object-vy k i) (min -1. (+ (* (1- i) 2.) (cadr vertical-velocity-1)))))
               (range 1 (1- n))))))

(define* (make-parametric-IC* position-difference #:optional (object-count body-count))
  (let ((rs (map (lambda (i)
                   (vector (* i position-difference)
                           max-height))
                 (iota (1- object-count))))
        (vs (map (lambda (i)
                (vector 0. 
                        (min -1. (+ (* i 2.) (cadr vertical-velocity-1)))))
              (iota (1- object-count)))))
    `((position . ,(cons #(0 0) rs))
      (velocity . ,(cons #(0 0) vs)))))



;(define choose-initial-conditions (make-freeze-random beer-choose-initial-conditions))
(define choose-initial-conditions (make-parametric-IC 50.))

(define-interactive (change-IC)
  (set! choose-initial-conditions (make-freeze-random beer-choose-initial-conditions)))

(define fode-state #f)

(define vision-line-actors #f)
(define vision-line-actors-index 0)
(define (draw-vision-lines agent-position end-point)
  #;(format #t "draw-vision-lines ~a ~a~%" agent-position end-point)
  (let ((scene (current-scene)))
    (when (or (not vision-line-actors) 
              (not (= (vector-length vision-line-actors) sensor-count)))
      ;; initialize the actors
      (set! vision-line-actors (make-vector sensor-count #f))
      (set! vision-line-actors-index 0))
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

(define (undraw-vision-lines)
  (let ((scene (current-scene)))
   (when (and scene vision-line-actors)
     (vector-for-each (lambda (actor) (remove-actor scene actor)) vision-line-actors)
     (set! vision-line-actors #f) 
     (set! vision-line-actors-index 0))))

(define current-genome (make-genome-for-n-ctrnn node-count))
(define gene-count (generalized-vector-length current-genome))

(add-hook! physics-tick-hook #.\ (fode-physics-tick))
(define tick-count 0)


;; Instead of just having local variables, shouldn't we just have
;; buffer variables?

(define draw-display? #t)
(define pause-fode? #f)

(define-interactive (toggle-draw-display)
  (set! draw-display? (not draw-display?)))

(define-key eracs-mode-map (kbd "d") 'toggle-draw-display)

(define-interactive (toggle-pause-fode)
  (set! pause-fode? (not pause-fode?)))

(define-key eracs-mode-map (kbd "p") 'toggle-pause-fode)

(define (fode-physics-tick)
  (let* ((scene (current-scene)) ;; This should be attached to the buffer.
         (restart? #t))
    (when  (and scene fode-state)
      (unless pause-fode?
        (if (not (step-physics fode-state h))
            (throw 'step-physics-error))
        (when #t #;(= 0 (mod tick-count update-ctrnn-freq))
          #;(if draw-display?
              (for-each (lambda (actor) (remove-actor scene actor)) vision-line-actors))
          (step-ctrnn ctrnn-state h ctrnn)))
      (if draw-display?
          (draw-physics scene fode-state))
      
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

(define last-vision-values '())

(define (make-collect-vision-values vision-input)
  (set! last-vision-values '())
  (let ((last-time #f))
   (lambda (t i)
     (let ((output (vision-input t i)))
       (when (< i sensor-count)
           (if (or #t (not (= output 10.)))
               (format #t "output not 10 but ~a at time ~a for sensor ~a~%" output t i))
           (when (not (equal? last-time t))
             #;(format #t "Adding another vector for time ~a~%" t)
             (cons! (cons t (make-vector sensor-count 0.)) last-vision-values)
             (set! last-time t))
           (vector-set! (cdar last-vision-values) i output))
      output))))

(define* (line-plot points 
                    #:key 
                    (axes-label #f)
                    (plot-label #f)
                    (joined #t)
                    (plot-legends #f)
                    (plot-range #f))
  
  (define (other-options)
    (let ((strings '()))
     (if axes-label
         (cons! (format #f "AxesLabel -> ~a" (sexp->mathematica axes-label)) strings))
     (if plot-label
         (cons! (format #f "PlotLabel -> ~a" (sexp->mathematica plot-label)) strings))
     (if plot-legends
         (cons! (format #f "PlotLegends -> ~a" (sexp->mathematica plot-legends)) strings))
     
     (if plot-range
         (cons! (format #f "PlotRange -> ~a" (sexp->mathematica plot-range)) strings))
     (cons! (format #f "Joined -> ~a" (sexp->mathematica joined)) strings)
     (string-join strings ", ")))
  (let* ((filename (format #f "~a.pdf" (tmpnam)))
         (expr (format #f "Export[\"~a\", ListPlot[~a, ~a]];" 
                        filename
                        (sexp->mathematica points)
                        (other-options))))
    (mylog "mathematica.input" pri-debug "~a" expr)
   (mathematica-eval expr)
   (system* "open" "-a" "Preview" filename)))

(define* (vision-values->points vision-values #:optional (sensor-index #f))
    ;; We have a list of ((t1 . #(v11 v12 ...)) (t2 . #(v21 v22
    ;; ... ))) that we want to transform into '((t1 v11) (t2 v21) ...)
    ;; for sensor 1 or a list of all lists if no sensor is given.
    (if sensor-index
        (map (lambda (entry)
               (list (car entry) (+ (* 10 sensor-index) (vector-ref (cdr entry) sensor-index))))
             vision-values)
        (map (lambda (index)
               (vision-values->points vision-values index))
             (range 0 (1- sensor-count)))))
  
(define-interactive (show-vision-values #:optional (values last-vision-values))
  (line-plot (vision-values->points values)
             #:axes-label '("time" "sensor value")
             #:plot-legends (map number->string (range 1 sensor-count))
             #:plot-range 'Full
             ))

(define-interactive (show-input-difference #:optional (genome current-genome))
  
  (let ((prev-record record-vision-values?)
        (prev-class physics-class)
        (prev-values last-vision-values))
    
    (in-out
     (begin
       (set! record-vision-values? #t)
       (set! physics-class <fode-physics>)
       (set! last-vision-values '()))
     (let ((fode-values #f)
           (bullet-values #f)
           (diff #f)
           (points #f)
           (tick-count 600))

       (eval-beer-robot genome #:max-tick-count tick-count)
       (set! fode-values (reverse last-vision-values))
       (line-plot (vision-values->points fode-values)
                  #:axes-label '("time" "sensor value")
                  #:plot-label "FODE"
                  #:plot-legends (map number->string (range 1 sensor-count))
                  #:plot-range '(Automatic '(-.5 10.5)) #;'((0 10) (-.2 12))
                  )
       (set! physics-class <bullet-physics-car>)
       (eval-beer-robot genome #:max-tick-count tick-count)
       (set! bullet-values (reverse last-vision-values))
       (line-plot (vision-values->points bullet-values)
                  #:axes-label '("time" "sensor value")
                  #:plot-label "Bullet"
                  #:plot-legends (map number->string (range 1 sensor-count))
                  )

       (set! diff (map vector-sum (map vector-abs (map vector- 
                                                       (map cdr fode-values)
                                                       (map cdr bullet-values)))))
       (set! points (map list (map car fode-values) diff))
       (line-plot points #:axes-label '("time" "sum of abs difference")
                  #:plot-label "Sensor differences"
                  ))
     (begin
       (set! record-vision-values? prev-record)
       (set! physics-class prev-class)
       (set! last-vision-values prev-values)))))

(define* (make-current-vision-input #:optional (draw? #t) (my-fode-state fode-state))
  (if (and use-c-vision? (not record-vision-values?))
            ;; Here's how to use the C implementation of vision.
      (make-unified-procedure 
       int 
       vision-input-pointer
       (list double 
             int 
             (list '*
                   (make-c-vision-context
                    (fp:state my-fode-state)
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
                    (and draw? draw-vision-lines))) 
             (list '* %null-pointer)))
      
      
      (let ((scheme-vision (make-vision-input (fp:state my-fode-state)
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
                                                         (and draw? draw-vision-lines))))
        (if record-vision-values?
            ;; Let's use the old lisp version, because it's easier to collect the
            ;; input.
            (make-collect-vision-values scheme-vision)
            scheme-vision))))

(define-interactive (reset-fode)
  (genome->ctrnn current-genome ctrnn)
  ;(randomize-ctrnn-state! ctrnn-state)
  (array-copy! init-ctrnn-state ctrnn-state)
  (undraw-vision-lines)
  (when fode
    (undraw-physics (current-scene) fode)
    (reset-physics fode))
  (set! fode (make physics-class
               #:object-count body-count 
               #:effector-func 
               (make-effector-func ctrnn-state)
;               go-nowhere
;               go-left
;                go-right
               
               ))
  (set! fode-state (fix-physics fode))
  (choose-initial-conditions fode fode-state)
  (set! (input-func ctrnn) (make-current-vision-input))
  (set! vision-line-actors #f))


(define-interactive (randomize-brain)
  (randomize-genome! current-genome)
  (genome->ctrnn current-genome ctrnn)
  (set! (input-func ctrnn) (make-current-vision-input))
  (randomize-ctrnn-state! ctrnn-state))

(define-interactive (reset-camera)
  (set-parameter! 'camera-position (vector 0 (/ max-height 2) 300))
  (set-parameter! 'camera-target (vector 0 (/ max-height 2) 0))
  (set-parameter! 'camera-up #(0 1 0)))

(add-hook! post-window-open-hook 
           (lambda ()
             (reset-camera)
             (randomize-genome! current-genome)
             (reset-fode)) 
           #t)

(define-interactive
  (graph-distance-over-time #:optional (genome current-genome))
    (let ((distances '()))
    ;; Set the agent body to a distance of zero.
    (define (step-func fode-state)
      (cons! (cons (- (get-time fode-state)) 
                   (map 
                    (lambda (i)
                      (- (object-x fode-state i) 
                         (object-x fode-state 0)))
                    (range 1 (1- body-count)))) 
             distances)
      
      ;; Return true if we have distances set for all objects.
      (not (every (lambda (i) (< (object-y fode-state i) 0)) (range 1 (1- body-count)))))
        
    (eval-beer-robot genome 
                     #:step-fn step-func)
    (mathematica (format #f "ListPlot[~a, AxesLabel -> {\"delta d\", \"t\"}, PlotRange -> {{-100, 100}, Automatic}, Epilog -> {{Dashed, Line@{{~a, 0}, {~:*~a, -200}}}, {Dashed, Line@{{-~:*~a, 0}, {-~:*~a, -200}}}}];" 
                         (sexp->mathematica (reverse! (map reverse distances))) 
                         successful-distance))))

(define-fitness
  ((minimize "Distance to objects"))
  (beer-selective-attention #:optional (genome current-genome)
                            (initial-condition choose-initial-conditions))
  (let ((d (make-vector body-count #f))
        (fitness #f))
    (define (begin-func fode-state)
      (if initial-condition
       (initial-condition fode-state fode-state)))
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
      ;(format #t "d = ~a~%" d)
      (if (vector-every identity d)
          (vector (max-of-vector (vector-map abs d)))
          #;(throw 'invalid-fitness)
          (vector 10000) ;; Big number for terrible fitness.
          ))
    ;; We set the distance for the agent (object 0) to 0.
    (vector-set! d 0 0.)
        
    (set! fitness (eval-beer-robot genome 
                                   #:begin-fn begin-func
                                   #:step-fn step-func 
                                   #:end-fn end-func))
    (message "Fitness ~a." fitness)
    fitness))

;(define initial-conditions (list case-1-IC case-2-IC case-3-IC))

(define initial-conditions (list choose-initial-conditions))

(define-interactive (add-IC)
  (cons! (make-freeze-random beer-choose-initial-conditions) initial-conditions))

(define-fitness
  ((minimize "the maximum distance to objects for a set of initial conditions."))
  (beer-selective-attention-n 
   #:optional 
   (genome current-genome)
   (my-initial-conditions initial-conditions))
  
  (let* ((initial-conditions (run-if-thunkable my-initial-conditions))
         (trials (length initial-conditions))
         (last-body-count body-count)
         (fitnesses '()))
    
    (do ((i 1 (1+ i)))
        ((> i trials))
      (cons! (vector-ref (beer-selective-attention 
                          genome
                          (list-ref initial-conditions (1- i))) 
                         0) fitnesses))
   (set! body-count last-body-count)
   (let ((total-fitness (vector (apply max fitnesses))))
     (message "Aggregate/Max fitness ~a." total-fitness)
     total-fitness
     )))

(define init-ctrnn-state (make-ctrnn-state ctrnn))
;; XXX Let's not randomize just yet.
;(randomize-ctrnn-state! init-ctrnn-state)

(define*
  (eval-beer-robot-render genome
                          #:key 
                          (step-fn identity)
                          (begin-fn identity)
                          (end-fn identity)
                          (max-tick-count 2000))
  (let* ((buffer (switch-to-buffer "*eval-robot*" <physics-buffer>))
         (scene (scene buffer)))
    (define (draw fode-state)
      (when draw-display?
          (draw-physics scene fode-state)
          (if (q-empty? event-queue)
             (block-yield)
             (primitive-command-tick))))
    (eval-beer-robot-headless genome
                              #:step-fn (lambda (fode-state) 
                                          (draw fode-state) 
                                          (step-fn fode-state))
                              #:begin-fn begin-fn
                              #:end-fn end-fn
                              #:max-tick-count max-tick-count)))

(define*
  (eval-beer-robot-headless genome
                   #:key 
                   (step-fn identity)
                   (begin-fn identity)
                   (end-fn identity)
                   (max-tick-count 2000))
  (let* ((ctrnn (make-n-ctrnn node-count))
         (ctrnn-state (make-ctrnn-state ctrnn))
         (effector-func (make-effector-func ctrnn-state))
         (fode (make physics-class
                 #:object-count body-count 
                 #:effector-func effector-func))
         (fode-state (fix-physics fode))
         (vision-input (make-current-vision-input #f ; don't draw.
                                                  fode-state
                                                  ))
         (tick-count 0))
    #;(choose-initial-conditions fode fode-state)
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
      (if #t #;(= 0 (mod tick-count update-ctrnn-freq))
          (if (not (step-ctrnn ctrnn-state h ctrnn))
              (throw 'step-ctrnn-error)))
      (if (not (step-physics fode-state h))
          (throw 'step-physics-error))
      (incr! tick-count (step-count fode-state))
      #;(format #t "Tick ~a~%" tick-count))
    #;(format #t "after state ~a~%" (vector-sum ctrnn-state))
    #;(format #t "step-count ~a~%"(step-count fode-state))
    (end-fn fode-state)))

(define eval-beer-robot eval-beer-robot-headless)

(define last-fitness-func #f) 
(define last-results #f)

(define successful-distance (+ (/ object-diameter 2)
                                (/ agent-diameter  2)))

(define (individual-succeeded? fitness)
  "Did the individual come in contact with the object."
  (format #t "checking fitness ~a~%" fitness)
  (let ((distance (generalized-vector-ref fitness 0)))
    (<= distance successful-distance)))

(define (any-individual-succeeded? generation results)
  "Returns true if we haven't found a successful candidate.  Input is (rank genome objective)."
  (format #t "Continue? ~a~%" generation)
  (any (compose individual-succeeded? caddr) results))

(define (scaffold-any-individual-succeeded? generation results)
  (if (any-individual-succeeded? generation results)
      (if (< (length initial-conditions) 4)
          (begin
            (add-IC)
            (format #t "Succeeded. Adding next task. ~a~%" initial-conditions)
            #f)
          #t)
      #f))

(define (make-scaffold-any-individual-succeeded? tasks-count add-IC!)
  (let ((added-count 0))
   (lambda (generation results)
     (if (any-individual-succeeded? generation results)
         (if (< added-count tasks-count)
             (begin
               (add-IC! generation)
               (incr! added-count)
               (format #t "Succeeded. Adding task ~a/~a.~%" added-count tasks-count)
               #f ;; Continue searching.
               )
             #t ;; Stop searching.
             )
         #f ;; Continue searching.
         ))))

(define (make-scaffold-any-individual-not-succeeded? tasks-count add-IC!)
  (let ((added-count 0))
   (lambda (generation results)
     (if (any-individual-succeeded? generation results)
         (if (< added-count tasks-count)
             (begin
               (add-IC! generation)
               (incr! added-count)
               (format #t "Succeeded. Adding task ~a/~a.~%" added-count tasks-count)
               'fitness-changed ;; Continue searching.
               )
             #f ;; Stop searching.
             )
         #t ;; Continue searching.
         ))))

(define-interactive (beer-optimize)
  (optimize beer-selective-attention-n 30 '() (compose not scaffold-any-individual-succeeded?)))

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
   (seed-population (list current-genome))
   (continue-search? (compose not any-individual-succeeded?)))
  
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
         (eval-count 0)
         (fitness-fn* 
          (lambda (weights)
            (incr! eval-count)
            (fitness-fn weights)))
         (generation-count 0)
         (objective-count (length (objectives fitness-fn)))
         ;; Had to use with-dynamic-state to make fluids work when crossing
         ;; into C code that called Scheme code.
         (results (nsga-ii-search 
                   fitness-fn*
                   #:objective-count objective-count
                   #:gene-count gene-count
                   #:population-count population-count
                   #:generation-count max-generations
                   #:seed-population seed-population
                   #:generation-tick-func (lambda args
                                            (incr! generation-count)
                                            (if continue-search?
                                             (apply continue-search? args)
                                             #t)))))
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
    (message "Feasible fitnesses ~a" (map cdr results))
    (values results generation-count eval-count)
    #;
    (when (called-interactively?) 
      (call-interactively 'set-pareto-front-index 0)
      (call-interactively 'plot-front))
    #;(set! (controller (current-robot)) run-nn-brain)
    ))

#;(define-interactive (secondary-optimize)
  (let ((results
         (nsga-ii-search
          
          )
         )))
  )

(define (run-if-thunkable x)
  (if (thunk? x)
      (x)
      x))

(define* (generation-count-to-do my-initial-conditions 
                                 #:optional 
                                 (max-generations #f) 
                                 (seed-population '()) 
                                 (continue-search? #f))
  "Determine the number of generations required to succeed at the
given tasks."
  (define-fitness
    ((minimize "distance from origin"))
    (fitness-fn genome)
    (beer-selective-attention-n genome my-initial-conditions))
  (let ()
    (receive (results gen-count eval-count) (optimize 
                                             fitness-fn
                                             max-generations
                                             seed-population
                                             continue-search?)
      (list gen-count eval-count))))

(define* (generation-count-to-do2 my-initial-conditions 
                                 #:optional 
                                 (max-generations #f) 
                                 (seed-population '()) 
                                 (continue-search? #f))
  "Determine the number of generations required to succeed at the
given tasks."
  (define-fitness
    ((minimize "distance from origin"))
    (fitness-fn genome)
    (beer-selective-attention-n genome my-initial-conditions))
  (let ()
    ;; results are ((genome . objective-value) ... )
    (receive (results gen-count eval-count) (optimize 
                                             fitness-fn
                                             max-generations
                                             seed-population
                                             continue-search?)
      (list results gen-count eval-count))))

(define (mean lst)
  (exact->inexact (/ (apply + lst) (length lst))))

(define (std lst)
  (define (square x) (* x x))
  (exact->inexact (sqrt (- (mean (map square lst)) (square (mean lst))))))

;(optimize beer-selective-attention 1)

