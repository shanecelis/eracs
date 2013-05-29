;; fode-physics.scm

;(load "bullet-beer.scm")

(use-module ((minimal-cognition fode)
             #:renamer (symbol-prefix-proc 'fode:)))

(define-class <fode-physics> (<physics>)
  (params #:accessor fp:params #:init-value #f)
  (state #:accessor fp:state #:init-value #f)
  (scene-actors #:accessor fp:scene-actors #:init-value '())
  )

(define-method (initialize (fp <fode-physics>))
  (next-method)
  (set! (fp:params fp) (fode:make-fode (object-count fp) (effector-func fp)))
  (set! (fp:state fp) (fode:make-fode-state (fp:params fp))))

(define-method (object-x-ref (fp <fode-physics>) i)
  (fode:object-x (fp:state fp) i))

(define-method (object-y-ref (fp <fode-physics>) i)
  (fode:object-y (fp:state fp) i))

(define-method (object-x-set! (fp <fode-physics>) i v)
  (set! (fode:object-x (fp:state fp) i) v))

(define-method (object-y-set! (fp <fode-physics>) i v)
  (set! (fode:object-y (fp:state fp) i) v))


(define-method (object-vx-ref (fp <fode-physics>) i)
  (fode:object-vx (fp:params fp) i))

(define-method (object-vy-ref (fp <fode-physics>) i)
  (fode:object-vy (fp:params fp) i))

(define-method (object-vx-set! (fp <fode-physics>) i v)
  (set! (fode:object-vx (fp:params fp) i) v))

(define-method (object-y-set! (fp <fode-physics>) i v)
  (set! (fode:object-vy (fp:params fp) i) v))

(define-method (step-physics (fp <fode-physics>) h)
  (fode:step-fode (fp:state) h (fp:params)))

(define-method (get-time (fp <fode-physics>))
  (fode:fode-time (fp:state fp)))

(define-method (draw-physics scene (fp <fode-physics>))
  ;; Draw the agent.
  (cons! (add-sphere scene (vector 
                            (object-x fp 0) 
                            (object-y fp 0) 
                            0)
                     (/ (agent-diameter fp) 2))
         (fp:scene-actors fp))
  ;; Draw the objects.
  (for-each 
   (lambda (i)
     (let ((position (vector (object-x fp i) 
                             (object-y fp i) 
                             0)))
       (if draw-display?
           (cons! (add-sphere scene 
                              position
                              (/ (object-diameter fp) 2)
                              #;object-diameter
                              #;(* .9 object-diameter)
                              #(0 0 0 1)
                              )
                  (fp:scene-actors fp))))) 
   (range 1 (1- (object-count fp)))))
