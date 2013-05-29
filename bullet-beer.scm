;; bullet-beer.scm
;;
;; We want to have a few things that work just like the FODE
;; that we've setup in beer-experiment.  They are:
;; (define fode-params (make-fode body-count effector))
;; (define fode-state (make-fode-state fode-params))
;; (object-x fode-state i) ;; x-position of object i
;; (object-y fode-state i) ;; y-position of object i
;; (object-vx fode-params i) ;; x-speed of object i
;; (object-vy fode-params i) ;; y-speed of object i
;; (step-fode fode-state h fode-params) ;; move the simulation forward
;; (draw-fode scene fode-state) ;; draw the scene
;;
;; We want the same interface but s/fode/bullet/.  

#;(define (make-bullet body-count effector)
  )

(use-modules 
 (oop goops))

(define max-height 180.)
(define h 0.1)
(define tick-count 0)
(define draw-display? #t)

(define-class <physics> ()
  ;; Number of effectors/muscles/outputs the agent has.
  (effector-count #:accessor effector-count #:init-keyword #:effector-count #:init-value 2)
  ;; Number of objects in the scene including agent.
  (object-count #:accessor object-count #:init-keyword #:object-count #:init-value 1)
  ;; Number of nodes that can be provided with input for the controller.
  (node-count #:accessor node-count #:init-keyword #:node-count #:init-value 1)
  ;; (effector-func t i) -> [-1, 1]
  ;; accepts a time t and an effector identifier i \in [1, effector-count]
  (effector-func #:accessor effector-func #:init-value #f)
  ;; (input-func t i) -> [-1, 1]
  ;; accepts a time t and a input number i \in [1, input-count]
  (input-func #:accessor input-func #:init-value #f)
  ;; All the parameters should probably be placed in here.
  (agent-diameter #:getter agent-diameter #:init-value 30)
  (object-diameter #:getter object-diameter #:init-value 26)
  (motor-constant #:getter motor-constant #:init-value 5)
  #;(max-sight-distance #:getter max-sight-distance #:init-value 205)
  )

(define-class <bullet-physics> (<physics>)
  (sim #:accessor bp:sim #:init-keyword #:sim #:init-form (current-sim))
  (objects #:accessor bp:objects #:init-value '())
  (force-constant #:accessor force-constant #:init-value 4.)
  )

(define-method (initialize (bp <bullet-physics>) initargs)
  (define (process-body body)
    (set-friction! body 0.001)
    (sim-add-body (bp:sim bp) body)
    body)
  (next-method)
  (sim-add-ground-plane2 (bp:sim bp))
  
  (let ((body (make-box #(0 0 0) 
                        (vector (agent-diameter bp) 
                                1 
                                (agent-diameter bp)) 1. "agent")))
    (set! 
     (bp:objects bp) 
     (cons body 
           (map 
            (lambda (i)
              (make-box (vector (- i 2.) 0 (- max-height)) 
                        (vector (object-diameter bp) 
                                1 
                                (object-diameter bp)) 
                        1. 
                        (format #f "object ~d" i)))
            (range 1 (1- (object-count bp)))))))
  (for-each process-body (bp:objects bp)))

(define-method (draw-physics scene (bp <bullet-physics>))
  "Draws the bullet physics simulation."
  (if (not (scene-update-physics scene (bp:sim bp)))
      (scene-add-physics scene (bp:sim bp))))

(define (go-right t i)
  (if (= i 1)
      1.0
      0.0))

(define (go-left t i)
  (if (= i 1)
      0.0
      1.0))

(define-method (get-time (bp <bullet-physics>))
  (sim-time (bp:sim bp)))

(define-method (step-physics (bp <bullet-physics>) h)
  "Apply the effectors and step the physics simulation forward by h
seconds."
  (if (effector-func bp)
      (let ((e1 ((effector-func bp) (get-time bp) 1))
            (e2 ((effector-func bp) (get-time bp) 2))
            (agent (car (bp:objects bp))))
        (apply-force agent 
                     (vector 
                      (* (force-constant bp) (motor-constant bp) (- e1 e2)) 
                      0. 
                      0.) 
                     #(0. 0. 0.))))
  (sim-tick (bp:sim bp) h))

(define-method (reset-physics (bp <bullet-physics>))
  (format #t "NYI - RESET!")
  )

(define-method (object-x-ref (bp <bullet-physics>) i)
  (let ((p (get-position (list-ref (bp:objects bp) i))))
    (vector-ref p 0)))

(define-method (object-y-ref (bp <bullet-physics>) i)
  (let ((p (get-position (list-ref (bp:objects bp) i))))
    (vector-ref p 1)))

(define-method (object-x-set! (bp <bullet-physics>) i v)
  (let* ((body (list-ref (bp:objects bp) i))
        (p (get-position body)))
    (vector-set! p 0 v)
    (set-position! body p)))

(define-method (object-y-set! (bp <bullet-physics>) i v)
  (let* ((body (list-ref (bp:objects bp) i))
        (p (get-position body)))
    (vector-set! p 1 v)
    (set-position! body p)))

(define-method (object-vx-ref (bp <bullet-physics>) i)
  (let ((p (get-velocity (list-ref (bp:objects bp) i))))
    (vector-ref p 0)))

(define-method (object-vy-ref (bp <bullet-physics>) i)
  (let ((p (get-velocity (list-ref (bp:objects bp) i))))
    (vector-ref p 1)))

(define-method (object-vx-set! (bp <bullet-physics>) i v)
  (let* ((body (list-ref (bp:objects bp) i))
        (p (get-velocity body)))
    (vector-set! p 0 v)
    (set-velocity! body p)))

(define-method (object-vy-set! (bp <bullet-physics>) i v)
  (let* ((body (list-ref (bp:objects bp) i))
        (p (get-velocity body)))
    (vector-set! p 1 v)
    (set-velocity! body p)))

(define object-x (make-procedure-with-setter object-x-ref object-x-set!))
(define object-y (make-procedure-with-setter object-y-ref object-y-set!))

(define object-vx (make-procedure-with-setter object-vx-ref object-vx-set!))
(define object-vy (make-procedure-with-setter object-vy-ref object-vy-set!))


(define eval-robot-render-speed 1)

(define (sim-add-ground-plane2 sim)
  (sim-add-fixed-box sim #(0. -10. 0.) #(400. 20. 400.)))

(define (init-scene sim)
  (sim-add-ground-plane2 sim)
  
  (let ((body (make-box #(0 1 0) #(1 .2 1) 1. "root")))
    body))

#;(define (my-physics-tick)
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
                )
        ;; Update the visualization.
        (scene-update-physics (current-scene) (current-sim)))))
   (filter (cut is-a? <> <physics-buffer>) (buffer-list))))

(define bp #f)
(define (my-physics-tick)
  (let* ((scene (current-scene)) ;; This should be attached to the buffer.
         (restart? #t))
    (when scene
      
      (step-physics bp h)
      #;(when (= 0 (mod tick-count update-ctrnn-freq))

        (step-ctrnn ctrnn-state h ctrnn))
      (if draw-display?
          (draw-physics scene bp))
      
      ;; Check if we should restart the simulation.
      (for-each 
       (lambda (i)
         (if (> (object-y bp i) 0)
             (set! restart? #f))) 
       (range 1 (1- (object-count bp))))

      (when restart?
        ;; restart
        (reset-physics)))
    (incr! tick-count)))

(define robot #f)
(add-hook! post-window-open-hook 
           (lambda ()
             (set! bp (make-instance <bullet-physics> #:object-count 2))
             (set! (effector-func bp) go-left)
             
             (let ((middle (/ max-height 2.)))
              (set-parameter! 'camera-position 
                              #;(vector 0. (* max-height 1.5) (- middle))
                              (vector 0.0 260.0 -82.0))
              
              (set-parameter! 'camera-target 
                              (vector 0.0 0.0   (- middle)))))
           #;(lambda ()
             (set! robot (init-scene (current-sim)))
             (sim-add-body (current-sim) robot)
             ;(set! (buffer-robot eracs-buffer) robot)
             (scene-add-physics (current-scene) (current-sim))
             (format #t "friction ~a~%" (get-friction robot))
             ;(set-friction! robot 0.001)
             (format #t "friction ~a~%" (get-friction robot))
             (apply-impulse robot #(1. 0. 0.) #(0. 0. 0.))
             ) 
           #t)

(add-hook! physics-tick-hook #.\ (my-physics-tick))
