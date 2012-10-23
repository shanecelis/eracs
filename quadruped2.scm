;; quadruped.scm
(use-modules (ice-9 receive)
             (oop goops)
             (emacsy emacsy)
             (osc-registry))
(define pi (acos -1))

(define (robot-time)
  (sim-time (sim eracs-buffer)))

(define-class <quadruped> ()
  (bodies #:getter bodies #:init-keyword #:bodies)
  (joints #:getter joints #:init-keyword #:joints)
  (controller #:accessor controller #:init-keyword #:controller)
  (touch-sensors #:accessor touch-sensors #:init-value (make-vector 9 #f))
  (target-angles #:accessor target-angles #:init-value (make-vector 8 0.))
  (active-joints #:accessor active-joints #:init-value (make-vector 8 #t)))

(define (make-boxy-cylinder pos radius length alignment)
  (let ((dims (make-vector 3 (* 2 radius))))
    (vector-set! dims (1- alignment) length)
    (make-box pos dims)))

(define (make-quadruped-robot)
  (let* ((nth list-ref)
         (body (make-box #(0 1 0) #(1 .2 1)))
         (legs (list (make-boxy-cylinder #(1    1  0  ) .1 1 1)
                     (make-boxy-cylinder #(1.5 .5  0  ) .1 1 2)
                     (make-boxy-cylinder #(0    1 -1  ) .1 1 3)
                     (make-boxy-cylinder #(0   .5 -1.5) .1 1 2) 
                     (make-boxy-cylinder #(-1   1  0  ) .1 1 1)
                     (make-boxy-cylinder #(-1.5 .5 0  ) .1 1 2)
                     (make-boxy-cylinder #(0    1  1  ) .1 1 3)
                     (make-boxy-cylinder #(0   .5  1.5) .1 1 2)))
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
                                   #(1 0   0) #(1  0  0)))))
    ;(cons (cons body legs) joints)
    (make <quadruped> #:bodies (cons body legs) #:joints joints)))

(define (sim-add-robot sim robot)
  (map #.\ (sim-add-body sim %1) (bodies robot))
  (map #.\ (sim-add-constraint sim %1) (joints robot)))

(define (sim-remove-robot sim robot)
  (map #.\ (sim-add-body sim %1) (bodies robot))
  (map #.\ (sim-add-constraint sim %1) (joints robot)))


;; Add the robot to the physics simulation.
(define robot (make-quadruped-robot))
(sim-add-robot (current-sim) robot)
(sim-add-ground-plane (current-sim))
(physics-add-scene (current-sim))


(define (robot-position)
  (get-position (car (bodies robot))))

(define-interactive (focus-on-robot)
  (set-parameter! 'camera-target (robot-position)))

(define (make-contact-responder robot index)
  (lambda ()
    (vector-set! (touch-sensors robot) index #t)))

(for-each 
 (lambda (body index)
   (set-contact-func! body (make-contact-responder robot index)))
 (bodies robot)
 (range 0 (1- (length (bodies robot)))))

(define osc-reg (make <osc-registry>))

;; Setup the OSC registry for target-angles.
(for-each 
  (lambda (index)
    (osc-register 
     osc-reg 
     ;; path
     (format #f "/2/target-angles/~a" (1+ index))
     ;; getter
     (lambda () (vector-ref (target-angles robot) index))
     ;; setter
     (lambda (value) (vector-set! (target-angles robot) index value)))) 
  (range 0 7))

;; Setup touch sensors for OSC.
(for-each 
 (lambda (index)
   (osc-register 
    osc-reg 
    ;; path
    (format #f "/2/touch~a" index)
    ;; getter
    (lambda () (vector-ref (touch-sensors robot) index))
    ;; setter
    (lambda (value) (vector-set! (touch-sensors robot) index value))))
 (range 0 8))

(define (active-joints-ref i)
  (vector-ref (active-joints robot) i))

(define* (active-joints-update #:optional (i #f))
  (if i
      (if (last-osc-client)
          (send-osc-message (last-osc-client) 9000 (format #f "/2/active/~a" (1+ i))
                            (if (active-joints-ref i) 1 0)))
      (for-each active-joints-update (range 0 7))))

(define (active-joints-set! i v)
  (if  (not (eq? (active-joints-ref i) v))
       (active-joints-update i))
  (vector-set! (active-joints robot) i v))

(define active-joints-acc 
  (make-procedure-with-setter active-joints-ref active-joints-set!))


(define direction 'right)

(define (hand-coded-brain tick-count)
  (let* ((index (member-ref direction 
                            '(right up left down none)))
         (v     (make-vector 8 0.))
         ;(T .) ;; period: 1 cycle per second
         (f 4.) ;; frequency
         (t (* .01 tick-count)) ;; time
         (omega (* 2. pi f)) ;; angular frequency
         (limit (* .8 (/ pi 4.))))
    (if (and index (<= index 3))
        (vector-set! v (* 2 index) (* limit (sin (* omega t)))))
    v))

(define (low-level-brain tick-count)
  (target-angles robot))

(define current-brain low-level-brain)
(set! current-brain hand-coded-brain)

(define tick-count 0)

(define (distal-touch-sensors)
  (list->vector 
   (map (lambda (i) (vector-ref (touch-sensors robot) (+ 2 (* i 2)))) 
        (range 0 3))))

(define nn-time-period 10.)


(define (nn-input)
  (let* ((time (/ (mod (robot-time) nn-time-period) 
                  nn-time-period))  ;; [0, 1]
         (time* (- (* 2. time) 1.)) ;; [-1, 1]
         (inputs (vector-map (lambda (x)
                              (if x
                                   1.0
                                  -1.0))  (distal-touch-sensors))))
    (vector-append (vector time*) inputs)
      ;; fake the input
    (vector time* 1 1 1 1)
    ))

(define (nn-output)
  (vector-map #.\ (* % (/ 4. pi)) (target-angles robot)))

(define neuron-count '(5 16 16 8))

(define fann-brain (make-nn neuron-count))

(define (set-nn-weights! w)
  (set! fann-brain (vector->nn w neuron-count)))

(define (get-nn-weights)
  (nn->vector fann-brain))

(define (nn-brain tick-count)
  (nn-run fann-brain (nn-input)))

(define (my-physics-tick)
  (if (not (paused? eracs-buffer))
      (with-buffer eracs-buffer
       ;; Update the simulation.
       (sim-tick (current-sim))
       ;; Update the visualization.
       (physics-update-scene (current-sim))
       ;; Update brain.
       ;; (when (= 0 (mod tick-count 120))
       ;;   (physics-add-scene (current-sim)))
       (when (= 0 (mod tick-count 10))
         ;; Run the brain.
         (let ((new-angles (current-brain tick-count)))
           (for-each (lambda (i) 
                       (vector-set! (target-angles robot) i
                                    (vector-ref new-angles i)))
                     (range 0 7)))
         ;(osc-push osc-reg)
         )
       ;; Actuate joints with desired angles.
       (for-each 
        #.\ (actuate-joint %1 (if %3 %2 #f))
        (joints robot)
        (vector->list (target-angles robot))
        (vector->list (active-joints robot)))

       ;(touch-sensors-update)
       (osc-push osc-reg)
       ;; Reset the touch sensors.
       (vector-fill! (touch-sensors robot) #f)

       (incr! tick-count))))

(define-method (robot-tick (robot <quadruped>))
  
  )

(add-hook! physics-tick-hook #.\ (my-physics-tick))

(define-interactive 
  (osc-set-target-angles #:optional (event this-command-event))
  (let ((parsed-path (string-tokenize (osc-path event) 
                                       (char-set-delete char-set:graphic #\/)))
         (values (osc-values event))
         (start-index 1)) ;; 1 for TouchOSC, 0 for Control
    (match `(,parsed-path ,@values)
      (((page "target-angles" i) value)
       (vector-set! (target-angles robot) 
                    (- (string->number i) start-index) 
                    (* (/ pi 4.) value))))))

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
                     (cons 'nn-brain nn-brain)))

(define-interactive 
  (switch-to-brain #:optional 
                   (brain (completing-read "Brain: "
                                           (map (compose symbol->string car) brains))))
    (set! current-brain (assq-ref brains (string->symbol brain))))

(define add-body #.\ (sim-add-body (current-sim) %))
(define remove-body #.\ (sim-remove-body (current-sim) %))
(define add-constraint #.\ (sim-add-constraint (current-sim) %))
(define remove-constraint #.\ (sim-remove-constraint (current-sim) %))

(define-interactive (restart-physics)
  (map remove-constraint (joints robot))
  (map remove-body (bodies robot))
  (set! robot (make-quadruped-robot))
  (map add-body (bodies robot))
  (map add-constraint (joints robot))
  (physics-clear-scene)
  (physics-add-scene (current-sim)))
