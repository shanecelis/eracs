;; fode-physics.scm

;(load "bullet-beer.scm")

(use-modules ((minimal-cognition fode)
             #:renamer (symbol-prefix-proc 'fode:)))

(define-class-public <fode-physics> (<physics>)
  (params #:init-value #f)
  (state #:getter fp:state #:init-value #f)
  (scene-actors #:accessor fp:scene-actors #:init-value '()))

(define-method (initialize (fp <fode-physics>) initargs)
  (next-method)
  ;(format #t "EFFECTOR ~a~%" (effector-func fp))
  (slot-set! fp 'params (fode:make-fode (object-count fp) (effector-func fp)))
  (slot-set! fp 'state (fode:make-fode-state (fp:params fp))))

(define-method (fp:params (fp <fode-physics>))
  ;(format #t "EFFECTOR ~a~%" (effector-func fp))
  (list (object-count fp) (fp:k-params fp) (effector-func fp)))

(define-method (fp:k-params (fp <fode-physics>))
  (cadr (slot-ref fp 'params)))

(define-method (object-x-ref (fp <fode-physics>) i)
  (fode:object-x (fp:state fp) i))

(define-method (object-y-ref (fp <fode-physics>) i)
  (fode:object-y (fp:state fp) i))

(define-method (object-x-set! (fp <fode-physics>) i v)
  (set! (fode:object-x (fp:state fp) i) v))

(define-method (object-y-set! (fp <fode-physics>) i v)
  (set! (fode:object-y (fp:state fp) i) v))

(define-method (object-vx-ref (fp <fode-physics>) i)
  (fode:object-vx (fp:k-params fp) i))

(define-method (object-vy-ref (fp <fode-physics>) i)
  (fode:object-vy (fp:k-params fp) i))

(define-method (object-vx-set! (fp <fode-physics>) i v)
  (if (and (= i 0) (not (= v 0.))) ;; agent
      (mylog 'pri-warn "Setting agent x velocity to non-zero ~a" v))
  (set! (fode:object-vx (fp:k-params fp) i) v))

(define-method (agent-motor-constant-set! (fp <fode-physics>) k)
  (set! (fode:object-vx (fp:k-params fp) 0) k))

(define-method (agent-motor-constant-ref (fp <fode-physics>))
  (object-vx-ref fp i))

(define-method (object-vy-set! (fp <fode-physics>) i v)
  (set! (fode:object-vy (fp:k-params fp) i) v))

(define-method (step-physics (fp <fode-physics>) h)
  ;(format #t "params ~a~%" (fp:params fp))
  (fode:step-fode (fp:state fp) h (fp:params fp) (step-count fp)))

(define-method (get-time (fp <fode-physics>))
  (fode:fode-time (fp:state fp)))

(define-method (set-time! (fp <fode-physics>) t)
  (fode:fode-time-set! (fp:state fp) t))

(define-method (draw-physics scene (fp <fode-physics>))
  (for-each (lambda (actor) (remove-actor scene actor)) (fp:scene-actors fp))
  (set! (fp:scene-actors fp) '())
  ;; Draw the agent.
  (cons! (add-sphere scene (vector 
                            (object-x fp 0) 
                            (object-y fp 0) 
                            0)
                     (/ agent-diameter 2))
         (fp:scene-actors fp))
  ;; Draw the objects.
  (for-each 
   (lambda (i)
     (let ((position (vector (object-x fp i) 
                             (object-y fp i) 
                             0)))
       
       (cons! (add-sphere scene 
                          position
                          (/ object-diameter 2)
                          #;object-diameter
                          #;(* .9 object-diameter)
                          #(0 0 0 1)
                          )
              (fp:scene-actors fp)))) 
   (range 1 (1- (object-count fp)))))

(define-method (undraw-physics scene (fp <fode-physics>))
  (when scene
   (for-each (lambda (actor) (remove-actor scene actor)) (fp:scene-actors fp))))


(define-method (reset-physics (fp <fode-physics>))
  #f)

;; (define object-x (make-procedure-with-setter object-x-ref object-x-set!))
;; (define object-y (make-procedure-with-setter object-y-ref object-y-set!))

;; (define object-vx (make-procedure-with-setter object-vx-ref object-vx-set!))
;; (define object-vy (make-procedure-with-setter object-vy-ref object-vy-set!))
