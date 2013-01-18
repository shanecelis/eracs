;; active-preferences.scm

(use-modules (linear-spline)
             (infix))

(define (make-spline)
  (make <linear-spline> #:domain '(-1. 1.) #:step-size 0.1))

(define (make-splines)
  (list->vector 
   (map 
    (lambda (i) 
      (make-spline))
    (range 0 7))))

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

(define (triangle-basis x)
  "This is a triangular basis element.  To produce whatever other kind
of triangle basis use the following transformation: (*
height (triangle-basis (/ (- x origin) base)))"
  (max 0 (if (>= x 0)
                      (- 1 (* 2 x))
                      (+ 1 (* 2 x)))))

(define* (triangle-basis* x #:optional (origin 0) (height 1) (base 1))
  "This is a triangular basis element of a particular height, base
length, and origin."
  (* height (triangle-basis (/ (- x origin) base))))

;; describe the triangle functions 
;; (list (joint-index origin height base) ...).
(define active-preferences-training '())
(define active-preferences-splines (make-splines))

(define active-preferences-primary-controller #f)

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


;; def hill-climber.scm
(define (active-pref-error weights active-pref-training)
  (let* ((nn (vector->nn weights neuron-count))
         (error-squared-elements 
          (map 
           (lambda (ap-entry)
             (match ap-entry
               ((index joint-value t-offset joint-offset spread)
                (let* ((output (nn-run nn (vector t-offset 1 1 1 1)))
                       (diff (: (joint-value + joint-offset) - (output @ index)))
                       (diffsq (* diff diff)))
                  diffsq))
               (_ #f)))
         active-pref-training)))
    ;; Sum them up.
    (apply + error-squared-elements)))

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
    
    (for-each 
     (lambda (i)
       ;; This spline-set isn't working right
       ;; I don't think.
       (spline-fill! (: splines @ i) 
                     (lambda (time)
                       (time-loop-value-set! robot time)
                       (let ((output (active-preferences-controller robot)))
                         (: output @ i))))) 
     (range 0 7))
    (time-loop-value-set! robot orig-time-loop)
    splines))

(define-interactive (plot-ap-brain)
  (plot-splines (ap->linear-splines (current-robot))))

(define-interactive (plot-ap-prefs)
  (plot-splines active-preferences-splines))

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
  (train-nn (ap->nn-training-values (current-robot)))
  (set! (controller (current-robot)) run-nn-brain))

(define ap-recording? #f)
(define-interactive (ap-record #:optional (event this-command-event))
  (set! ap-recording? (button-on? (car (osc-values event)))))

(define-interactive (ap-erase)
  (set! active-preferences-training '())
  (set! active-preferences-splines (make-splines)))

(define-key eracs-mode-map (kbd "osc-2-record") 'ap-record)
(define-key eracs-mode-map (kbd "osc-2-erase")  'ap-erase)
(define-key eracs-mode-map (kbd "osc-2-time-slider")  'goto-time)

(define-interactive (test-ap-prefs)
  (set! active-preferences-training 
        '((0 0.15 0.4 -0.8 2.0) 
          (0 0.0 -0.4  0.6 2.0))))
