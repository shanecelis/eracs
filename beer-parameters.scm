(define (scale-by a list)
  (map (lambda (x) (* a x)) list))

(define sensor-count 9)
(define internode-count 10)
(define effector-count 2)
(define node-count (+ sensor-count internode-count effector-count))
(define body-count 3) ;; 1 agent, 2 objects
;(define body-count 2)
(define agent-diameter 30)
(define object-diameter 26)
(define motor-constant 5.0)
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

(define h 0.1) ;; time step
(define population-count 12)
(define update-ctrnn-freq 1)
(define record-vision-values? #f)

(define use-c-vision? #t)

