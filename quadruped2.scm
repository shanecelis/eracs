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
             )
(define pi (acos -1))

;; (define* (robot-time #:optional (my-sim (current-sim)))
;;   (sim-time my-sim))

(define neuron-count '(5 16 16 8))

;(define fann-brain (make-nn neuron-count))

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
  (vector-fill! (active-joints robot) #t)
  )

(define (make-boxy-cylinder pos radius length alignment)
  (let ((dims (make-vector 3 (* 2 radius))))
    (vector-set! dims (1- alignment) length)
    (make-box pos dims)))

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
    (lambda ()
      (vector-set! (touch-sensors robot) index #t)))
  
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
                                   #(1 0   0) #(1  0  0))))
         (target-body (make-box #(0 1 -10) #(1  1 1)))
         (wall-body   (make-box #(0 1 -5)  #(10 1 1)))
         
         (robot (make <quadruped> 
                  #:bodies (append (cons body legs) ;(list target-body wall-body)
                                   
                                   ) 
                  #:joints joints
                  #:controller hand-coded-brain ;low-level-brain
                  )))
    
    (for-each 
     (lambda (body index)
       (set-contact-func! body (make-contact-responder robot index)))
     (bodies robot)
     (range 0 8))
    robot))

(define (sim-add-robot sim robot)
  (map #.\ (sim-add-body sim %1) (bodies robot))
  (map #.\ (sim-add-constraint sim %1) (joints robot))
  (set! (in-sim robot) sim))

(define (sim-remove-robot sim robot)
  (map #.\ (sim-remove-constraint sim %1) (joints robot))
  (map #.\ (sim-remove-body sim %1) (bodies robot))
  (set! (in-sim robot) #f))

(define (sim-add-ground-plane2 sim)
  (sim-add-fixed-box sim #(0. -10. 0.) #(400. 20. 400.)))

;; Add the robot to the physics simulation.
(define robot (make-quadruped-robot))
(sim-add-robot (current-sim) robot)
(mylog "quadruped" pri-debug "current sim ~a" (current-sim))
;(sim-add-ground-plane (current-sim))
(sim-add-ground-plane2 (current-sim))
;target-body 
(sim-add-fixed-box (current-sim) #(0 1 -10) #(1  1 1))
;wall-body
(sim-add-fixed-box (current-sim) #(0 1 -5)  #(10 1 1))

(physics-add-scene (current-sim))

(define* (robot-position #:optional (my-robot robot))
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
     (lambda () (vector-ref (target-angles robot) index))
     ;; setter
     (lambda (value) (vector-set! (target-angles robot) index value)))) 
  (range 0 7))

;; Setup the active joints.
(for-each 
  (lambda (index)
    (osc-register 
     osc-reg 
     ;; path
     (format #f "/2/active/1/~a" (1+ index))
     ;; getter
     (lambda () (vector-ref (active-joints robot) index))
     ;; setter
     (lambda (value) (vector-set! (active-joints robot) index value)))) 
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

(define nn-time-period 10.) ;; seconds?

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
  (let* ((time (time-loop-value robot))  ;; [-1, 1]
         (inputs (vector-map (lambda (x)
                              (if x
                                   1.0
                                  -1.0))  (distal-touch-sensors robot))))
    (vector-append (vector time) inputs)
      ;; fake the input except for time component
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

(define (make-spline)
  (make <linear-spline> #:domain '(-1. 1.) #:step-size 0.1))

(define (make-splines)
  (list->vector 
           (map 
            (lambda (i) 
              (make-spline))
            (range 0 7))))


;; describe the triangle functions (joint-index origin height base).
(define active-preferences-training '())
(define active-preferences-splines (make-splines))

(define active-preferences-primary-controller run-nn-brain)

;; active preferences is a controller that can be augmented by the user.
(define (active-preferences-controller robot)
  ;; Construct the offset for time t for joint i.
  (let* ((t (time-loop-value robot))
         (offset (make-vector 8 0.)))
    (for-each (lambda (ap-entry)
                (vector-set! offset (car ap-entry) 
                             (+ (vector-ref offset (car ap-entry))
                                (apply triangle-basis* t (cddr ap-entry)))))
              active-preferences-training)
    (vector+ offset (active-preferences-primary-controller robot))))

;; ;; Convert the preferences into splines.
;; (define (ap-prefs->splines)
;;   (let ((splines 
;;          (list->vector 
;;           (map 
;;            (lambda (i) 
;;              (make <linear-spline> #:domain '(-1. 1.) #:step-size 0.1)) 
;;            (range 0 7)))))
;;     (for-each (lambda (ap-entry)
                
;;                 (vector-set! offset (car ap-entry) 
;;                              (+ (vector-ref offset (car ap-entry))
;;                                 (apply triangle-basis* t (cdr ap-entry)))))
;;               active-preferences-training)
;;     )
;;   )

(define (active-preferences-offset robot)
  ;; Construct the offset for time t for joint i.
  (let* ((t (time-loop-value robot))
         (offset (make-vector 8 0.)))
    (for-each (lambda (ap-entry)
                (vector-set! offset (car ap-entry) 
                             (+ (vector-ref offset (car ap-entry))
                                (apply triangle-basis* t (cddr ap-entry)))))
              active-preferences-training)
    offset))


(define (ap->linear-splines robot)
  (let ((orig-time-loop time-loop-param)
         (splines (make-splines)))
    
    (for-each (lambda (i)
                ;; This spline-set isn't working right;
                ;; I don't think.
                (spline-fill! (: splines @ i) (lambda (time)
                                (time-loop-value-set! robot time)
                                (let ((output (active-preferences-controller robot)))
                                  (: output @ i))))) 
              (range 0 7))
    (time-loop-value-set! robot orig-time-loop)
    splines))

(define* (vector-denan v #:optional (nan-fill 0.))
  (vector-map (lambda (x) 
                (if (nan? x)
                    nan-fill
                    x)) 
              v))

(define-interactive (plot-ap-brain)
  (plot-splines (ap->linear-splines robot)))

(define-interactive (plot-ap-prefs)
  (plot-splines active-preferences-splines))

(define (plot-splines splines-vector)
  (let* ((splines (vector->list splines-vector))
         (spline-count (length splines))
         (m (vector-length (spline-range-vector (car splines))))
         (xs (map spline-domain-vector splines))
         (ys (map (lambda (spline i) 
                    (vector+ 
                     (make-vector m (+ 1 (* 2. (- (1- spline-count) i))))
                     (vector-denan 
                      (vector-bound -1. 1. (spline-range-vector spline)) -1)))
                    splines
                    (index-range splines-vector))))
    (gnuplot "set xlabel 'time step'")
    (gnuplot (format #f "set yrange [0:~a]" (* 2 spline-count)))
    (gnuplot (format #f "set ytic 0,2,~a" (1+ (* 2 spline-count))))
    ;(gnuplot "set xtic 0,40")
    ;(gnuplot "set ytic 0,2,17")
    
    ;(format #t "xs ~a ys ~a~%" xs ys)
    (apply gnuplot-multiplot ys)
    (gnuplot "unset xtic")
    (gnuplot "unset ytic")
    (gnuplot "unset yrange")
    (gnuplot "unset xlabel")
    ))

(define (ap->nn-training-values robot)
  (let ((orig-time-loop time-loop-param)
        (result (map (lambda (time)
                       (time-loop-value-set! robot time)
                       (cons (nn-input robot) 
                             (active-preferences-controller robot)))
         
                     (range -1. 1. .05))))
    (time-loop-value-set! robot orig-time-loop)
    result))

(define-interactive (ap-train)
  (train-nn (ap->nn-training-values robot))
  (set! (controller robot) run-nn-brain))

(define (my-physics-tick)
  (if (not (paused? eracs-buffer))
      (with-buffer eracs-buffer
       ;; Update the simulation.
       (sim-tick (current-sim))
       ;; Update the visualization.
       (physics-update-scene (current-sim))
       ;; Do we want multiple bodies shown?
       ;; (when (= 0 (mod (tick-count robot) 120))
       ;;   (physics-add-scene (current-sim)))
       (robot-tick robot)
       (osc-push osc-reg))))

(define controller-update-frequency 10)

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
    (set! (controller robot) (assq-ref brains (string->symbol brain))))

(define-interactive (restart-physics)
  (sim-remove-robot (current-sim) robot)
  (set! robot (make-quadruped-robot))
  (sim-add-robot (current-sim) robot)
  (physics-clear-scene)
  (physics-add-scene (current-sim))
  ;(sim-time-set! (current-sim) 0.0)
  )
