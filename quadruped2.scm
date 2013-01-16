;; quadruped.scm
(use-modules (ice-9 receive)
             (oop goops)
             (emacsy emacsy)
             (osc-registry)
             (vector-math)
             (infix)
             (linear-spline)
             (gnuplot plot)
             (logging)
             (srfi srfi-26) ;cut
             )
(define pi (acos -1))

(define (current-robot)
  (buffer-robot (current-buffer)))


;; Some deprecated functions
(define (physics-clear-scene)
  (and (current-scene) (scene-clear-physics (current-scene))))

(define (physics-update-scene sim)
  (and (current-scene) (scene-update-physics (current-scene) sim)))

(define (physics-add-scene sim)
  (and (current-scene) (scene-add-physics (current-scene) sim)))


;; (define* (robot-time #:optional (my-sim (current-sim)))
;;   (sim-time my-sim))
(cons! <physics-buffer> buffer-classes)

(define neuron-count '(5 4 8))

(define-class <quadruped> ()
  (bodies #:getter bodies #:init-keyword #:bodies)
  (joints #:getter joints #:init-keyword #:joints)
  (controller #:accessor controller #:init-keyword #:controller)
  (touch-sensors #:accessor touch-sensors #:init-form (make-vector 9 #f))
  (target-angles #:accessor target-angles #:init-form (make-vector 8 0.))
  (active-joints #:accessor active-joints #:init-form (make-vector 8 #t))
  (tick-count #:accessor tick-count #:init-value 0)
  (in-sim #:accessor in-sim #:init-value #f)
  (nn-brain #:accessor nn-brain #:init-form (make-nn neuron-count))
  )

(define-method (robot-time (robot <quadruped>))
  (and (in-sim robot) (sim-time (in-sim robot))))

(define-method (reset-robot (robot <quadruped>))
  (vector-fill! (touch-sensors robot) #f)
  (vector-fill! (target-angles robot) 0.)
  (vector-fill! (active-joints robot) #t))

(define* (make-boxy-cylinder pos radius length alignment #:optional (name #f))
  (let ((dims (make-vector 3 (* 2 radius))))
    (vector-set! dims (1- alignment) length)
    (apply make-box pos dims 1. (if name (list name) '()))))

(define (low-level-brain robot )
  (target-angles robot))

(define direction 'right)

(define (hand-coded-brain robot)
  (let* ((index (member-ref direction 
                            '(right up left down none)))
         (v     (make-vector 8 0.))
         ;(T .) ;; period: 1 cycle per second
         (f 4.) ;; frequency
         (t (* .01 (tick-count robot))) ;; time
         (omega (* 2. pi f)) ;; angular frequency
         (limit (* .8 (/ pi 4.))))
    (if (and index (<= index 3))
        (vector-set! v (* 2 index) (* limit (sin (* omega t)))))
    v))

(define (make-quadruped-robot)
  (define (make-contact-responder robot index)
    (let ((v (touch-sensors robot)))
     (lambda ()
       (vector-set! v index #t))))
  
  (let* ((nth list-ref)
         (body (make-box #(0 1 0) #(1 .2 1) 1. "root"))
         (legs (list (make-boxy-cylinder #(1    1  0  ) .1 1 1 "leg")
                     (make-boxy-cylinder #(1.5 .5  0  ) .1 1 2 "leg")
                     (make-boxy-cylinder #(0    1 -1  ) .1 1 3 "leg")
                     (make-boxy-cylinder #(0   .5 -1.5) .1 1 2 "leg") 
                     (make-boxy-cylinder #(-1   1  0  ) .1 1 1 "leg")
                     (make-boxy-cylinder #(-1.5 .5 0  ) .1 1 2 "leg")
                     (make-boxy-cylinder #(0    1  1  ) .1 1 3 "leg")
                     (make-boxy-cylinder #(0   .5  1.5) .1 1 2 "leg")))
         (joints (list (make-hinge body (nth legs 0)
                                   #(.5 0 0) #(-.5 0  0)
                                   #(0 0 -1) #(0   0 -1))
                       (make-hinge (nth legs 0) (nth legs 1)
                                   #(.5 0 0) #(0 0.5  0)
                                   #(0 0 -1) #(0   0 -1))
                       (make-hinge body (nth legs 2)
                                   #(0  0 -.5) #(0  0 .5)
                                   #(-1 0   0) #(-1 0  0))
                       (make-hinge (nth legs 2) (nth legs 3)
                                   #(0  0 -.5) #(0 0.5  0)
                                   #(-1 0   0) #(-1  0  0))
                       (make-hinge body (nth legs 4)
                                   #(-.5 0 0) #(.5 0  0)
                                   #(0   0 1) #(0  0  1))
                       (make-hinge (nth legs 4) (nth legs 5)
                                   #(-.5 0 0) #(0 0.5  0)
                                   #(0   0 1) #(0   0  1))
                       (make-hinge body (nth legs 6)
                                   #(0  0 .5) #(0  0 -.5)
                                   #(1 0   0) #(1 0  0))
                       (make-hinge (nth legs 6) (nth legs 7)
                                   #(0  0 .5) #(0 0.5  0)
                                   #(1 0   0) #(1  0  0))))
         ;(target-body (make-box #(0 1 -10) #(1  1 1)))
         ;(wall-body   (make-box #(0 1 -5)  #(10 1 1)))
         
         (robot (make <quadruped> 
                  #:bodies (append (cons body legs) ;(list target-body wall-body)
                                   
                                   ) 
                  #:joints joints
                  #:controller hand-coded-brain) ;low-level-brain
                ))
    
    ;; XXX - The contact function is a closure that contains the robot.
    ;; This seems to create a circular reference that will not allow
    ;; for garbage collection to happen.  Yuck!
    ;; FIXED it by having the closure not keep the robot but instead only
    ;; keep the reference to the touches vector that robot has.
    (for-each 
     (lambda (body index)
       (set-contact-func! body (make-contact-responder robot index)))
     (bodies robot)
     (range 0 8))
    robot))

(define (sim-add-robot sim robot)
  (map #.\ (sim-add-body sim %1) (bodies robot))
  (map #.\ (sim-add-constraint sim %1) (joints robot))
  (set! (in-sim robot) sim)
  )

(define (sim-remove-robot sim robot)
  (map #.\ (sim-remove-constraint sim %1) (joints robot))
  (map #.\ (sim-remove-body sim %1) (bodies robot))
  (set! (in-sim robot) #f))

(define (sim-add-ground-plane2 sim)
  (sim-add-fixed-box sim #(0. -10. 0.) #(400. 20. 400.)))

(define robot #f)

(define (init-robot-scene-anemic sim)
  (let ((robot (make-quadruped-robot)))
    (sim-add-robot sim robot)
    robot))

(define (init-robot-scene sim)
  ;; Add the robot to the physics simulation.
  (let ((robot (make-quadruped-robot)))
    
    (sim-add-robot sim robot)
    ;(mylog "quadruped2" pri-debug "added quadruped to sim ~a" sim)
    ;;(sim-add-ground-plane sim)
    (sim-add-ground-plane2 sim)
    robot))

(define obstacles '( 
                   ;;target-body 
                   (#(0 1 -10) #(1  1 1))
                   ;;wall-body
                   (#(0 1 -5)  #(10 1 1))
                   ))

(define (init-robot-obstacle-scene sim)
  (map (lambda (obs) (apply sim-add-fixed-box sim obs)) obstacles)
  (init-robot-scene sim))

(define init-scene init-robot-obstacle-scene)

(set! robot (init-scene (current-sim)))
(set! (buffer-robot eracs-buffer) robot)
(physics-add-scene (current-sim))

(define* (robot-position #:optional (my-robot (current-robot)))
  (get-position (car (bodies my-robot))))

(define-interactive (focus-on-robot)
  (set-parameter! 'camera-target (robot-position)))

(define osc-reg (make <osc-registry>))

;; Setup the OSC registry for target-angles.
(for-each 
  (lambda (index)
    (osc-register 
     osc-reg 
     ;; path
     (format #f "/2/target-angles/~a" (1+ index))
     ;; getter
     (lambda () (vector-ref (target-angles (current-robot)) index))
     ;; setter
     (lambda (value) (vector-set! (target-angles (current-robot)) index value)))) 
  (range 0 7))

;; Setup the active joints.
(for-each 
  (lambda (index)
    (osc-register 
     osc-reg 
     ;; path
     (format #f "/2/active/1/~a" (1+ index))
     ;; getter
     (lambda () (vector-ref (active-joints (current-robot)) index))
     ;; setter
     (lambda (value) (vector-set! (active-joints (current-robot)) index value)))) 
  (range 0 7))

;; Setup touch sensors for OSC.
(for-each 
 (lambda (index)
   (osc-register 
    osc-reg 
    ;; path
    (format #f "/2/touch~a" index)
    ;; getter
    (lambda () (vector-ref (touch-sensors (current-robot)) index))
    ;; setter
    (lambda (value) (vector-set! (touch-sensors (current-robot)) index value))))
 (range 0 8))

(define nn-time-period 2.) ;; seconds

(define time-loop-param #f)

(define (time-loop-value robot)
  (or time-loop-param 
      (let* ((time (/ (mod (robot-time robot) nn-time-period)
                      nn-time-period))) ;; [0,1] 
        (- (* 2. time) 1.)))) ;; [-1,1]

(define (time-loop-value-set! robot value)
  ;(osc-set! "/2/play" (if value 1. 0.))
  (set! time-loop-param value))

;; Setup the time loop
(osc-register
 osc-reg
 "/2/time-slider"
 (lambda () (time-loop-value robot))
 (lambda (value) (time-loop-value-set! robot value)))



(define (distal-touch-sensors robot)
  (list->vector 
   (map (lambda (i) (vector-ref (touch-sensors robot) (+ 2 (* i 2)))) 
        (range 0 3))))

(define (nn-input robot)
  (let* ((abs-time (/ (robot-time robot) eval-robot-time))
         (time (time-loop-value robot))  ;; [-1, 1]
         (inputs (vector-map (lambda (x)
                              (if x
                                   1.0
                                  -1.0))  (distal-touch-sensors robot))))
    ;(vector-append (vector time) inputs)
      ;; fake the input except for time component
    ;(vector abs-time time 1 1 1 1)
    (vector time 1 1 1 1)
    
    ))

(define (osc-value->angle x)
  (* x (/ 4. pi)))

(define (angle->osc-value a)
  (/ (* a pi ) 4.))

;; The neural net does output directly encoded angular values.
(define (nn-output robot)
  (vector-map osc-value->angle (target-angles robot)))

(define (set-nn-weights! robot w)
  (set! (nn-brain robot) (vector->nn w neuron-count)))

(define (get-nn-weights robot)
  (nn->vector (nn-brain robot)))

(define (run-nn-brain robot)
  (nn-run (nn-brain robot) (nn-input robot)))

(set! active-preferences-primary-controller run-nn-brain)



(define* (vector-denan v #:optional (nan-fill 0.))
  (vector-map (lambda (x) 
                (if (nan? x)
                    nan-fill
                    x)) 
              v))


(define (noop . args)
  #f)

(define-interactive (toggle-draw-path)
  (set! draw-path? (not draw-path?))
  ;; delete the current one.
  (draw-path)
  (if draw-path?
      (set! draw-path (make-line-drawing))
      (set! draw-path noop)))

(define draw-path noop)
(define draw-path? #f)
(define draw-path-frequency 10) ;ticks

(define (make-line-drawing)
  (let ((points '())
        (actor #f))
    (case-lambda 
      ((point)
       (cons! point points)
       (if actor
           (remove-actor (current-scene) actor))
       (set! actor (add-line (current-scene) points #(1. 0. 0. 1.))))
      (() 
       (when actor 
           (remove-actor (current-scene) actor)
           (set! actor #f))
       (set! points '())))))

(define eval-robot-render-speed 1)

(define-interactive (increase-render-speed #:optional (n 1))
  (incr! eval-robot-render-speed n)
  (if (< eval-robot-render-speed 1)
      (set! eval-robot-render-speed 1))
  (message "Render speed ~dX" eval-robot-render-speed))

(define-interactive (decrease-render-speed #:optional (n 1))
  (increase-render-speed (- n)))

(define-key eracs-mode-map (kbd "=") 'increase-render-speed)
(define-key eracs-mode-map (kbd "-") 'decrease-render-speed)




(define (my-physics-tick)
  (for-each 
   (lambda (physics-buffer)
     (with-buffer 
      physics-buffer
      (unless (paused? physics-buffer)            
        ;; Update the simulation.
        (repeat eval-robot-render-speed
                (sim-tick (current-sim))
                ;; Do we want multiple bodies shown?
                ;; (when (= 0 (mod (tick-count robot) 120))
                ;;   (physics-add-scene (current-sim)))
                (when (current-robot)
                  (let ((robot (current-robot)))
                    (robot-tick robot)
                    (if (and draw-path? 
                             (= 0 (mod (tick-count robot) draw-path-frequency)))
                        (draw-path (robot-position robot))))
                  (osc-push osc-reg)))

        ;; Update the visualization.
        (physics-update-scene (current-sim))

        )))
   (filter (cut is-a? <> <physics-buffer>) (buffer-list))))

(define controller-update-frequency 5) ; robot ticks

(define-method (robot-tick (robot <quadruped>))
  ;; Update brain.
  ;; (format #t "Update brain?")
  ;; (tick-count robot)
  ;; (format #t "tick count?")    
  
  (when (= 0 (mod (tick-count robot) controller-update-frequency))
    ;(format #t "Update brain.")
    ;; Run the brain.
    (let ((new-angles ((controller robot) robot)))
      (for-each (lambda (i) 
                  (vector-set! (target-angles robot) i
                               (vector-ref new-angles i)))
                (range 0 7))))
  ;(format #t "Actuate joints.")
  ;; Actuate joints with desired angles.
  (for-each 
   #.\ (actuate-joint %1 (if %3 %2 #f))
   (joints robot)
   (vector->list (target-angles robot))
   (vector->list (active-joints robot)))

  ;; Reset the touch sensors.
  (vector-fill! (touch-sensors robot) #f)

  (incr! (tick-count robot)))

(add-hook! physics-tick-hook #.\ (my-physics-tick))

(define-interactive 
  (osc-set-target-angles #:optional (event this-command-event))
  (let ((parsed-path (string-tokenize 
                      (osc-path event) 
                      (char-set-delete char-set:graphic #\/)))
         (values (osc-values event))
         (start-index 1)) ;; 1 for TouchOSC, 0 for Control
    (match `(,parsed-path ,@values)
      (((page "target-angles" i) value)
       (let ((index (- (string->number i) start-index)))
        (if ap-recording?
            (begin
              (cons! (list index
                           (vector-ref (target-angles robot) index)
                           (time-loop-value robot)
                           (- (osc-value->angle value) 
                              (vector-ref (target-angles robot) index))
                           2.)
                     active-preferences-training)
              (let ((spline (make-spline))
                    (value-offset (- (osc-value->angle value) 
                                          (vector-ref (target-angles robot) index)))
                    (time-offset (time-loop-value robot))
                    (spread 2.)
                    (new-v #f))
                (spline-fill! spline (lambda (t)
                                       (triangle-basis* t
                                                        time-offset
                                                        value-offset
                                                        spread)))
                (set! new-v (vector+ (spline-range-vector spline)
                                     (spline-range-vector 
                                      (: active-preferences-splines @ index))))
                (vector-move! (spline-range-vector 
                               (: active-preferences-splines @ index))
                 new-v))
              ;; What's a good way to call this function in a rate-limited way?
              ;; (call-with-limit 1 ;Hz
              ;;                  (lambda () (plot-ap-brain)))
              ;(plot-ap-brain)
              ))
        (vector-set! (target-angles robot) 
                     index
                     (osc-value->angle value)))))))

(for-each #.\ (define-key eracs-mode-map 
                (kbd (format #f "osc-2-target-angles-~a" %)) 
                'osc-set-target-angles) (range 1 8))

(define-interactive 
  (osc-set-active-joints #:optional (event this-command-event))
  (let ((parsed-path (string-tokenize (osc-path event) 
                                       (char-set-delete char-set:graphic #\/)))
         (values (osc-values event))
         (start-index 1)) ;; 1 for TouchOSC, 0 for Control
    (match `(,parsed-path ,@values)
      (((page "active" _ i) value)
       (vector-set! (active-joints robot) (- (string->number i) start-index) (> value 0))))))

(for-each #.\ (define-key eracs-mode-map 
                (kbd (format #f "osc-2-active-1-~a" %)) 
                'osc-set-active-joints) (range 1 8))

;(define-key eracs-mode-map (kbd "osc-2-target-angles-2") 'osc-set-target-angles)

(define brains (list (cons 'hand-coded-brain hand-coded-brain)
                     (cons 'low-level-brain low-level-brain)
                     (cons 'nn-brain run-nn-brain)
                     (cons 'ap-brain active-preferences-controller)))

(define-interactive 
  (switch-to-brain 
   #:optional 
   (brain (completing-read "Brain: "
                           (map (compose symbol->string car) brains))))
    (set! (controller (current-robot)) (assq-ref brains (string->symbol brain))))

(define-interactive (restart-physics)
  (let ((weights (and (current-robot) (get-nn-weights (current-robot)))))
    (physics-clear-scene)
    (set! (sim (current-buffer)) (make-sim))
    (set! robot (init-scene (current-sim)))
    (set! (buffer-robot (current-buffer)) robot)
    (and weights
     (set-nn-weights! robot weights))
    (set! (controller robot) run-nn-brain)
    (physics-add-scene (current-sim))
    (draw-path)))

(define lines '())

(define-interactive (make-lines #:optional (n 1))
  (define (rand)
    (1- (random 2.)))
  (define (crand)
    (random 1.))
  (repeat n
   (cons! 
    (add-line (current-scene) 
              (list #(0. 0. 0.) 
                    (vector (rand) (rand) (rand)))
              (vector (crand) (crand) (crand) (crand)))
          lines)))

(define-interactive (clear-lines)
  (for-each (cut remove-actor (current-scene) <>) lines)
  (set! lines '()))
