;; active-preferences.scm

(use-modules (linear-spline)
             (gnuplot plot)
             (infix)
             (vector-math)
             (srfi srfi-9)
             (ice-9 match)
             (ice-9 receive)
             )

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

(define *triangle-base* 1.)

;; :: ((index . #(t h)) ...)
(define ap-given-indexed-points '())
(define ap-old-weights #f)

;; :: (t -> #(w1 w2 ...)) ((index . #(t h)) ...) -> (t -> #(v1 v2 ...))
#;(define (given-indexed-points->composite f gips)
  (define (get-index i)
    (filter (lambda (element) (= i (car element))) gips))
  (let* ((given-points-col (map (compose cdr get-index) (range 0 7)))
         (points-col (map (lambda (points index) 
                           
                            (given-points->points
                             (lambda (t)
                               (: (f t) @ index)) points)) 
                          given-points-col
                          (range 0 7)))
         (fs (map compose-triangles points-col)))
   (lambda (t)
     (list->vector (map fs t)))))

(define (partition-indexed-points indexed-points index-count)
  (define (get-points-for-index i)
    (filter (lambda (element) (= i (car element))) indexed-points))
  (map (lambda (i) 
         (map cdr (get-points-for-index i))) 
       (range 0 (1- index-count)))) 

(define (given-indexed-points->indexed-points* f gips index-count)
    (let* ((given-points-col (partition-indexed-points gips index-count))
           (points-col (map-i0 (lambda (points index)
                                 (map (lambda (point) 
                                        (cons index point)) 
                                      (given-points->points
                                       (lambda (t)
                                         (: (f t) @ index)) points))) 
                               given-points-col)))
      points-col))

(define (given-indexed-points->indexed-points f gips ic)
  (apply append (given-indexed-points->indexed-points* f gips ic)))

(define (given-points->points f points)
  (map (match-lambda 
        (#(t h)
         (vector t (- h (f t))))
        (else
         (throw 'invalid-points))) points))

(define (compose-triangles points)
  (receive (pos neg) (partition (lambda (point) (: (point @ 1) >= 0)) points)
    (lambda (t)
      (+ (apply max 0 (map (lambda (point) (triangle-basis* t (: point @ 0) (: point @ 1) *triangle-base*)) pos))
         (apply min 0 (map (lambda (point) (triangle-basis* t (: point @ 0) (: point @ 1) *triangle-base*)) neg))))))

(define (compose-triangles-indexed indexed-points index-count)
  (let* ((points (partition-indexed-points indexed-points index-count))
         (fs (map compose-triangles points)))
    (lambda (t)
      (list->vector (map (lambda (f) (f t)) fs)))))

(define (ap-controller robot)
  "This ap-controller uses the new composited triangles method."
  (define (f t)
    (with-time-loop-value robot t
     (active-preferences-primary-controller robot)))
  (let* ((t (time-loop-value robot))
         (indexed-points (given-indexed-points->indexed-points f
                          ap-given-indexed-points 8))
         (composite (compose-triangles-indexed indexed-points 8)))
    (vector+ (f t) (composite t))))

(define (ap-error old-weights given-indexed-points new-weights)
  (let ((old-nn (vector->nn old-weights neuron-count))
        (new-nn (vector->nn new-weights neuron-count)))
   (define (f t)
     (with-time-loop-value robot t
                           (nn-run old-nn (nn-input robot))))
   (define (g t)
     (with-time-loop-value robot t
                           (nn-run new-nn (nn-input robot))))
   (let* ((indexed-points (given-indexed-points->indexed-points 
                           f
                           given-indexed-points
                           8))
          (error-at-t (compute-error-indexed indexed-points f g 8)))
     (apply + (map error-at-t (range -1 1 0.1))))))

;; :: ((index . #(t h)) ...) (t -> #(v1 v2 ...)) (t -> #(w1 w2 ...)) -> (t -> error)
(define (compute-error-indexed indexed-points f g index-count)
  (define (take-index f i)
    (lambda args
      (vector-ref (apply f args) i)))
  (let* ((ppoints (partition-indexed-points indexed-points index-count))
         (ces (map-i0 
               (lambda (points i)
                 (compute-error points (take-index f i) (take-index g i)))
               ppoints)))
    (lambda (t)
      (apply + (map (lambda (ce) (ce t)) ces)))))

(define (compute-error points f g)
  ;; f is the old controller, g is the new controller
  (let ((composite (compose-triangles points))
        (delta (compose-triangles 
                (map (match-lambda
                      (#(t h)
                       (vector t 1.))) points))))
   (lambda (t)
     (let* ((actual (+ (f t) (composite t)))
            (new (g t)))
       (* (abs (- new actual)) (delta t))))))

;; def hill-climber.scm
(define (active-pref-error weights active-pref-training)
  (let* ((nn (vector->nn weights neuron-count))
         (error-squared-elements 
          (map 
           (lambda (ap-entry)
             (match ap-entry
               ((index joint-value t-offset joint-offset spread)
                (let* ((output (nn-run nn (vector t-offset 1 1 1 1)))
                       (diff (: (joint-value + joint-offset) - (output @ index))))
                  (* diff diff)))
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
  (let ((splines (make-splines)))
    
    (for-each 
     (lambda (i)
       ;; This spline-set isn't working right, so I'm using spline-fill!.
       (spline-fill! (: splines @ i) 
                     (lambda (time)
                       (with-time-loop-value 
                        robot time
                        (let ((output (active-preferences-controller robot)))
                          (: output @ i)))))) 
     (range 0 7))
    
    splines))

(define-interactive (plot-ap-brain)
  (plot-splines (ap->linear-splines (current-robot))))

(define-interactive (plot-ap-prefs)
  (plot-splines active-preferences-splines))

(define (ap->nn-training-values robot)
  (map (lambda (time)
         (with-robot-time robot time
                               (cons (nn-input robot) 
                                     (ap-controller robot))))
       (range 0. 3. 0.05)))

(define-interactive (ap-train)
  (train-nn (ap->nn-training-values (current-robot)))
  (set! (controller (current-robot)) run-nn-brain))

(define ap-recording? #f)
(define-interactive (ap-record #:optional (event this-command-event))
  (set! ap-recording? (button-on? (car (osc-values event)))))

(define-interactive (ap-erase)
  (set! active-preferences-training '())
  (set! active-preferences-splines (make-splines))
  (set! ap-given-indexed-points '()))

(define-key eracs-mode-map (kbd "osc-2-record") 'ap-record)
(define-key eracs-mode-map (kbd "osc-2-erase")  'ap-erase)
(define-key eracs-mode-map (kbd "osc-2-time-slider")  'goto-time)

(define-interactive (test-ap-prefs)
  (set! active-preferences-training 
        '((0 0.15 0.4 -0.8 2.0) 
          (0 0.0 -0.4  0.6 2.0))))

(define-interactive (test-ap-prefs3)
  (set! ap-given-indexed-points '((0 . #(0.5 0.7)) 
                                  (0 . #(-0.3 -0.75)))))

(define-interactive (test-ap-prefs4)
  (set! ap-given-indexed-points '((0 . #(1. 1.))
                                  (0 . #(0.5 -0.7))
                                  (0 . #(-0.35 0.5))
                                  (0 . #(-1.0 -1)))))

(define-interactive (test-ap-prefs5)
  (let ((bend-in (map (lambda (index) (cons index #(-0.5 -0.5))) (range 0 7)))
        (jump-out (map (lambda (index) (cons index #(0.5 0.5))) (range 0 7))))
   (set! ap-given-indexed-points (append bend-in jump-out))))


(define-interactive (pin-other-joints)
  "Use the ap-given-indexed-points, and pin the other ones for these specified values."
  (let ((new-points 
         (append-map! 
          (match-lambda
           ((0 . #(t h))
            (map (lambda (index)
                   (cons index (vector t 0.))) (range 1 7)))) 
          ap-given-indexed-points)))
    (set! ap-given-indexed-points (append! new-points ap-given-indexed-points))))

(define-interactive (test-ap-prefs2)
  (set! active-preferences-training
        '((0 -0.013119014301668 0.21100914478302 -1.04689856878169 2.0) (0 1.21231896258008 -0.56538169272244 0.0182762201811202 2.0) (0 0.894627140774226 -0.582041691988707 0.317691821805852 2.0) (0 1.18185874739353 -0.598701691254973 0.0182760683991385 2.0) (0 1.17576667399982 -0.598701691254973 0.00609207339370688 2.0) (0 1.16967460060611 -0.598701691254973 0.00609207339370665 2.0) (0 1.1574904538187 -0.598701691254973 0.0121841467874133 2.0) (0 1.15139838042499 -0.598701691254973 0.00609207339370688 2.0) (0 1.14530645881327 -0.598701691254973 0.006091921611725 2.0) (0 1.13921438541956 -0.598701691254973 0.00609207339370665 2.0) (0 1.12703023863215 -0.598701691254973 0.0121841467874135 2.0) (0 1.12093816523844 -0.598701691254973 0.00609207339370665 2.0) (0 1.10875401845103 -0.598701691254973 0.0121841467874135 2.0) (0 1.09047795005189 -0.598701691254973 0.0182760683991385 2.0) (0 1.07829380326447 -0.598701691254973 0.0121841467874133 2.0) (0 1.06001758308335 -0.598701691254973 0.0182762201811202 2.0) (0 1.05392550968965 -0.598701691254973 0.00609207339370665 2.0) (0 1.0295573678968 -0.598701691254973 0.0243681417928454 2.0) (0 1.01737322110939 -0.598701691254973 0.0121841467874133 2.0) (0 0.999097000928269 -0.632021689787507 0.0182762201811202 2.0) (0 0.413974446326525 -0.632021689787507 0.585122554601744 2.0) (0 0.950360565560597 -0.831941680982709 0.0243682935748268 2.0) (0 0.932084497161458 -0.831941680982709 0.0182760683991385 2.0) (0 0.901624130192925 -0.831941680982709 0.0304603669685336 2.0) (0 0.877255988400079 -0.831941680982709 0.0243681417928453 2.0) (0 0.852887694825253 -0.831941680982709 0.0243682935748267 2.0) (0 0.8224274796387 -0.831941680982709 0.0304602151865521 2.0) (0 0.80415125945758 -0.831941680982709 0.0182762201811201 2.0) (0 0.779783117664735 -0.831941680982709 0.0243681417928453 2.0) (0 0.755414824089908 -0.831941680982709 0.0243682935748268 2.0) (0 0.527299486656405 -0.831941680982709 0.228115337433503 2.0) (0 0.706678388722236 -0.848601680248976 0.0243682935748268 2.0) (0 0.676218173535684 -0.881921678781509 0.0304602151865521 2.0) (0 0.657941953354564 -0.881921678781509 0.0182762201811201 2.0) (0 0.633573811561719 -0.881921678781509 0.0243681417928453 2.0) (0 0.609205517986892 -0.881921678781509 0.0243682935748268 2.0) (0 -0.785612611152463 -0.881921678781509 1.39481812913936 2.0) (0 0.56046908261922 0.901498330757022 0.0243682935748268 2.0) (0 0.530008867432668 0.901498330757022 0.030460215186552 2.0) (0 0.499548500464134 0.901498330757022 0.0304603669685336 2.0) (0 0.475180358671289 0.901498330757022 0.0243681417928452 2.0) (0 0.450812065096462 0.901498330757022 0.0243682935748268 2.0) (0 0.42035184990991 0.901498330757022 0.030460215186552 2.0) (0 0.395983556335083 0.901498330757022 0.0243682935748268 2.0) (0 0.371615414542238 0.901498330757022 0.0243681417928452 2.0) (0 0.347247120967411 0.901498330757022 0.0243682935748268 2.0) (0 0.316786905780859 0.901498330757022 0.030460215186552 2.0) (0 0.292418612206033 0.901498330757022 0.0243682935748268 2.0) (0 0.261958397019481 0.901498330757022 0.030460215186552 2.0) (0 0.24368217683836 0.901498330757022 0.0182762201811201 2.0) (0 0.225406108439222 0.901498330757022 0.0182760683991386 2.0) (0 0.201037814864395 0.901498330757022 0.0243682935748268 2.0) (0 0.18944436089906 0.901498330757022 0.011593453965335 2.0) (0 0.164485526284136 0.734898338094354 0.0182760683991386 2.0) (0 0.146209306103016 0.734898338094354 0.0182762201811201 2.0) (0 0.127933085921896 0.734898338094354 0.0182762201811201 2.0) (0 0.115749090916464 0.734898338094354 0.0121839950054319 2.0) (0 0.0913807973416375 0.734898338094354 0.0243682935748268 2.0) (0 0.0852887239479308 0.734898338094354 0.0060920733937067 2.0) (0 0.0670126555487922 0.734898338094354 0.0182760683991386 2.0) (0 0.0548285087613788 0.734898338094354 0.0121841467874134 2.0) (0 0.0365522885802587 0.734898338094354 0.0182762201811201 2.0) (0 0.0243682935748268 0.734898338094354 0.0121839950054319 2.0) (0 0.0121841467874134 0.734898338094354 0.0121841467874134 2.0) (0 -0.00609207339370671 0.734898338094354 0.0182762201811201 2.0) (0 -0.024368217683836 0.734898338094354 0.0182761442901293 2.0) (0 -0.0365523644712495 0.734898338094354 0.0121841467874134 2.0) (0 -0.0609205821550855 0.734898338094354 0.024368217683836 2.0) (0 -0.0731046530515082 0.734898338094354 0.0121840708964226 2.0) (0 -0.0913807973416375 0.734898338094354 0.0182761442901293 2.0) (0 -0.0974728707353442 0.734898338094354 0.00609207339370671 2.0) (0 -0.115749015025474 0.734898338094354 0.0182761442901293 2.0) (0 -0.127933161812887 0.734898338094354 0.0121841467874134 2.0) (0 -0.14011723270931 0.734898338094354 0.0121840708964226 2.0) (0 -0.0100068586250294 0.684918340295553 -0.13011037408428 2.0) (0 -0.164485526284136 0.468338349834085 0.0121841467874134 2.0) (0 -0.182761670574266 0.468338349834085 0.0182761442901294 2.0) (0 -0.194945741470688 0.468338349834085 0.0121840708964226 2.0) (0 -0.213221961651809 0.468338349834085 0.0182762201811201 2.0) (0 -0.219313959154524 0.468338349834085 0.00609199750271591 2.0) (0 -0.231498105941938 0.468338349834085 0.0121841467874134 2.0) (0 -0.24368217683836 0.468338349834085 0.0121840708964226 2.0) (0 -0.261958397019481 0.451678350567818 0.0182762201811201 2.0) (0 -0.274142467915903 0.451678350567818 0.0121840708964226 2.0) (0 -0.292418688097023 0.451678350567818 0.0182762201811201 2.0) (0 -0.304602758993446 0.435018351301551 0.0121840708964226 2.0) (0 -0.328970976677282 0.435018351301551 0.0243682176838361 2.0) (0 -0.341155123464695 0.435018351301551 0.0121841467874134 2.0) (0 -0.359431267754825 0.435018351301551 0.0182761442901294 2.0) (0 -0.0477542220899383 0.418358352035284 -0.311677045664887 2.0) (0 -0.40207562972879 0.235098360106349 0.0121840708964226 2.0) (0 -0.420351774018919 0.235098360106349 0.0182761442901294 2.0) (0 -0.444720067593746 0.235098360106349 0.0243682935748268 2.0) (0 -0.462996211883876 0.235098360106349 0.0182761442901294 2.0) (0 -0.487364429567712 0.235098360106349 0.0243682176838361 2.0) (0 -0.505640573857841 0.235098360106349 0.0182761442901294 2.0) (0 -0.530008791541677 0.235098360106349 0.024368217683836 2.0) (0 -0.554377009225513 0.235098360106349 0.024368217683836 2.0) (0 -0.572653229406633 0.235098360106349 0.0182762201811202 2.0) (0 -0.597021447090469 0.235098360106349 0.024368217683836 2.0) (0 -0.621389664774305 0.235098360106349 0.024368217683836 2.0) (0 -0.645757806567151 0.235098360106349 0.0243681417928453 2.0) (0 -0.670126100141978 0.235098360106349 0.0243682935748268 2.0) (0 -0.694494241934823 0.235098360106349 0.0243681417928453 2.0) (0 -0.712770462115943 0.235098360106349 0.0182762201811201 2.0) (0 -0.743230753193486 0.235098360106349 0.0304602910775428 2.0) (0 -0.761506897483615 0.235098360106349 0.0182761442901294 2.0) (0 -0.779783117664735 0.235098360106349 0.0182762201811201 2.0) (0 -0.80415125945758 0.235098360106349 0.0243681417928453 2.0) (0 -0.828519553032407 0.235098360106349 0.0243682935748268 2.0) (0 -0.846795621431546 0.235098360106349 0.0182760683991385 2.0) (0 -0.865071841612666 0.235098360106349 0.0182762201811202 2.0) (0 -0.630709097832955 0.235098360106349 -0.234362743779711 2.0) (0 -0.907716203586631 0.0185183696448803 0.0182761442901294 2.0) (0 -0.919900350374045 0.0185183696448803 0.0121841467874134 2.0) (0 -0.944268568057881 0.0185183696448803 0.024368217683836 2.0) (0 -0.96254471234801 0.0185183696448803 0.0182761442901294 2.0) (0 -0.867544470414307 0.00185837037861347 -0.0950002419337034 2.0) (0 -0.775896637635213 -0.0148016288876534 -0.204924294893917 2.0) (0 -1.01737322110939 -0.214721620082855 0.0182761442901294 2.0) (0 -1.04174143879322 -0.214721620082855 0.024368217683836 2.0) (0 -1.05392550968965 -0.214721620082855 0.0121840708964227 2.0) (0 -1.07220172987077 -0.214721620082855 0.0182762201811202 2.0) (0 -1.08438587665818 -0.214721620082855 0.0121841467874133 2.0) (0 -1.10266202094831 -0.214721620082855 0.0182761442901294 2.0) (0 -1.11484609184473 -0.214721620082855 0.0121840708964227 2.0) (0 -1.12703023863215 -0.214721620082855 0.0121841467874133 2.0) (0 -1.13312231202585 -0.214721620082855 0.00609207339370665 2.0) (0 -1.14530638292228 -0.214721620082855 0.0121840708964227 2.0) (0 -1.15139845631598 -0.214721620082855 0.00609207339370665 2.0) (0 -1.05541468367404 -0.231381619349122 -0.0959837726419426 2.0) (0 -0.970246900974584 -0.264701617881656 -0.187243552844115 2.0) (0 0.0 -0.514601606875658 -1.18795074489624 2.0))))


(define (ap-set-target-angles robot index angle)
  (when ap-recording?
      (cons! (list 
              ;; joint index
              index
              ;; actual value at joint
              (vector-ref (target-angles robot) index)
              ;; time loop
              (time-loop-value robot)
              ;; offset
              (- angle
                 (vector-ref (target-angles robot) index))
              ;; base
              *triangle-base*)
             active-preferences-training)
      (let ((spline (make-spline))
            (value-offset (- angle
                             (vector-ref (target-angles robot) index)))
            (time-offset (time-loop-value robot))
            (spread *triangle-base*)
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
      (call-with-limit 1. ;Hz
                       (lambda () (plot-ap-brain)))
                                        ;(plot-ap-brain)
      
      ;; Let's capture it one MORE way!
      (let ((height (: angle + ((target-angles robot) @ index)))
            (t (time-loop-value robot)))
       (cons! (cons index (vector t height)) ap-given-indexed-points))))

(add-hook! post-window-open-hook
          (lambda ()
            (display "HERE!")
            (newline)
            (add-hook! set-target-angles-hook ap-set-target-angles)
            (set! active-preferences-primary-controller run-nn-brain)
            ;;(cons! (cons 'ap-brain active-preferences-controller) brains)
            (cons! (cons 'ap-brain ap-controller) brains)
            
            (let ((wc (neuron-count->weight-count neuron-count)))
              (set! ap-old-weights (make-vector wc 0.)))) 
          #t) 

