;; quadruped.scm
(use-modules (ice-9 receive)
             (oop goops)
             (emacsy emacsy))
(define pi (acos -1))

(define-class <quadruped> ()
  (bodies #:getter bodies #:init-keyword #:bodies)
  (joints #:getter joints #:init-keyword #:joints)
  (controller #:accessor controller #:init-keyword #:controller)
  (sensors #:accessor sensors)
  ;(target-angles #:accessor target-angles)
  ;(active-joints #:accessor active-joints)
  
  )



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

(define robot (make-quadruped-robot))
;; (define bodies car)
;; (define joints cdr)

(map #.\ (sim-add-body (current-sim) %1) (bodies robot))
(map #.\ (sim-add-constraint (current-sim) %1) (joints robot))

(sim-add-ground-plane (current-sim))

(physics-add-scene (current-sim))

(define-interactive (focus-on-robot)
  (set-parameter! 'camera-target (get-robot-position)))

(define (get-robot-position)
  (get-position (car (bodies robot))))

(define (make-contact-responder index)
  (lambda ()
    (set! (touch-sensors-acc index) #t)
    (vector-set! touch-sensors index #t)))

(for-each (lambda (body index)
            (set-contact-func! body (make-contact-responder index)))
          (bodies robot)
          (range 0 8))

(define target-angles (make-vector 8 0))

(define (target-angles-ref i)
  (vector-ref target-angles i))

(define* (target-angles-update #:optional (i #f))
  (if i
      (if (last-osc-client)
          (send-osc-message (last-osc-client) 9000 
                            (format #f "/2/target-angles/~a" (1+ i))
                            (/ (target-angles-ref i) (/ pi 4.))))
      (for-each target-angles-update (range 0 7))))


(define (target-angles-set! i v)
  (if (not (eq? (target-angles-ref i) v))
      (begin (vector-set! target-angles i v)
             (target-angles-update i))
      (vector-set! target-angles i v)))

(define target-angles-acc 
  (make-procedure-with-setter target-angles-ref target-angles-set!))

(define active-joints (make-vector 8 #t))

(define (active-joints-ref i)
  (vector-ref active-joints i))

(define* (active-joints-update #:optional (i #f))
  (if i
      (if (last-osc-client)
          (send-osc-message (last-osc-client) 9000 (format #f "/2/active/~a" (1+ i))
                            (if (active-joints-ref i) 1 0)))
      (for-each active-joints-update (range 0 7))))

(define (active-joints-set! i v)
  (if  (not (eq? (active-joints-ref i) v))
       (active-joints-update i))
  (vector-set! active-joints i v))

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
  target-angles)

(define current-brain low-level-brain)
(set! current-brain hand-coded-brain)

(define tick-count 0)

(define touch-sensors (make-vector 9 #f))

(define (touch-sensors-ref i)
  (vector-ref touch-sensors i))

(define (touch-sensors-set! i v)
  (if (not (eq? (touch-sensors-ref i) v))
      (begin
        (vector-set! touch-sensors i v)
        (touch-sensors-update i))
      (vector-set! touch-sensors i v)))

(define touch-sensors-acc 
  (make-procedure-with-setter touch-sensors-ref touch-sensors-set!))

(define* (touch-sensors-update #:optional (i #f))
  (if i
      (if (and (last-osc-client) (> i 0))
          (send-osc-message (last-osc-client) 
                            9000 
                            (format #f "/2/touch~a" i)
                            (if (touch-sensors-ref i)
                                1.0
                                0.0)))
      (for-each touch-sensors-update (range 0 8))))

(define (distal-touch-sensors)
  (list->vector 
   (map (lambda (i) (vector-ref touch-sensors (+ 2 (* i 2)))) (range 0 3))))

(define nn-time-period 10.)

(define (robot-time)
  (sim-time (sim eracs-buffer)))


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
  (vector-map #.\ (* % (/ 4. pi)) target-angles))

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
                       (set! (target-angles-acc i) 
                             (vector-ref new-angles i)))
                     (range 0 7))))
       ;; Actuate joints with desired angles.
       (for-each 
        #.\ (actuate-joint %1 (if %3 %2 #f))
        (joints robot)
        (vector->list target-angles)
        (vector->list active-joints))

       (touch-sensors-update)
       ;; Reset the touch sensors.
       (vector-fill! touch-sensors #f)

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
       (vector-set! target-angles 
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
       (vector-set! active-joints (- (string->number i) start-index) (> value 0))))))

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
