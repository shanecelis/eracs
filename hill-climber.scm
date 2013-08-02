;; hill-climber.scm
;; requires the following procedures 
;; neuron-count
;; set-nn-weights!
;; (get-robot-position)
;; (restart-physics)
;; mid-level-nn-brain

(use-modules ((rnrs) #:select (vector-map vector-for-each mod))
             (emacsy emacsy)
             (nsga-ii)
             (vector-math)
             (infix)
             (ice-9 q)
             (srfi srfi-1)  ;; take
             (srfi srfi-11) ;; let-values
             (srfi srfi-26) ;; cut cute
             (srfi srfi-4)  ;; uniform vectors
             (srfi srfi-4 gnu)  ;; uniform vectors
             (srfi srfi-69) ;; hash-table
             (rnrs io ports)
             (mathematica-plot plot))

(add-hook! emacsy-terminate-hook (lambda ()
                                   (mathematica-quit)))

(define (neuron-count->matrix-size node-counts)
  "Adds a bias weight to each layer except the output layer."
  (let ((counts (reverse node-counts)))
    (cons (car counts) (map 1+ (cdr counts)))))

(define (neuron-count->matrix-sizes node-counts)
  "Adds a bias weight to each input layer."
  (if (< (length node-counts) 2)
      '()
      (cons (list (1+ (car node-counts)) (cadr node-counts)) (neuron-count->matrix-sizes (cdr node-counts)))))

(define (neuron-count->weight-count neuron-count)
  (apply + (map (lambda (size) (apply * size)) 
                (neuron-count->matrix-sizes neuron-count))))

(define gene-count (neuron-count->weight-count neuron-count))

(define first-time #t)

(define (random-weight)
  (- 1 (random 2.)))

(define (random-brain)
  (let ((w (make-vector gene-count 0.)))
    (vector-map (lambda (x) 
                  (random-weight)) w)))

(define (prob p)
  (<= (random 1.) p))

(define (mutate-gene weight) 
  (if (prob 0.05)
      (random-weight)
      weight))

(define (mutate weights)
  (vector-map mutate-gene weights))

(define-interactive (randomize-brain)
  (set-nn-weights! (current-robot) (random-brain)))

(define-interactive 
  (write-brain #:optional
               (weights (get-nn-weights (current-robot)))
               (filename (read-file-name "Write brain to file: ")))
  (call-with-output-file filename 
    (lambda (port)
      ;(uniform-vector-write (any->f64vector weights) port)
      (put-bytevector port (u32vector (vector-length weights)))
      (put-bytevector port (any->f64vector weights)))))

(define-interactive
  (just-load-it)
  (call-interactively 'read-brain "/Users/shane/School/uvm/CSYS-395-evolutionary-robotics/eracs/results/r04/jump3-01/individual08.bin"))

(define-interactive 
  (read-brain #:optional
              (filename (read-file-name "Read brain from file: ")))
  (let ((count (make-u32vector 1))
        (weights #f))
   (call-with-input-file filename 
     (lambda (port)
       (get-bytevector-n! port count 0 (* 4 1))
       ;(get-bytevector-all port)
       (set! weights (make-f64vector (uniform-vector-ref count 0)))
       (get-bytevector-n! port weights 0 (* 8 (uniform-vector-ref count 0)))
       ;(uniform-vector-read! weights port)
       (when (called-interactively?)
        (set-nn-weights! (current-robot) weights))
       weights))))

(define-interactive (clear-brain)
  (set-nn-weights! (current-robot) (make-vector gene-count 0.)))

(define (evaluate-robot weights)
  (set-nn-weights! robot weights)
  (eval-robot))

 ;; simulated seconds

;; no op
(define-interactive (osc-noop)
 #f)

(define-key eracs-mode-map (kbd "osc-ping") 'osc-noop)

(define-interactive
  (hill-climber 
   #:optional 
   (max-generations 
    (read-from-string (read-from-minibuffer "max-evaluations: "))))
  (message "optimizer: starting")
  ;(message "hill-climber: starting")
  (let* ((parent (if (and #f first-time)
                     (begin 
                       (set! first-time #f) 
                       (random-brain))
                     (get-nn-weights (current-robot))))
        (parent-fitness (evaluate-robot parent))
        (child #f)
        (child-fitness 0))
   (let loop ((generation 0))
     (set! child (mutate parent))
     (set! child-fitness (evaluate-robot child))
     (message "eval ~a best-objective ~6f current-objective ~6f" generation parent-fitness child-fitness)
     (if (> child-fitness parent-fitness)
         (begin (set! parent child)
                (set! parent-fitness child-fitness)))
     (if (< generation max-generations)
      (loop (1+ generation))
      (begin (evaluate-robot parent))))))


(define path '())

(define (map-sleazy f . args)
  (let ((l (length (car args))))
   (apply map f (car args) (map (cut take <> l) (cdr args)))))

(define-interactive (draw-all-robot-paths)
  (if last-pareto-front
   (map (lambda (color weights) 
          (draw-robot-path weights color))
        (vector->list colors)
        (append! (map car (vector->list last-pareto-front)) 
                 (list last-seed-weights)))
   (message "No pareto front available.")))

(define (calc-robot-path weights)
  (let ((points '())
        (capture-frequency 10)) ;; ticks
    (define (capture-position robot)
      (if (= (mod (robot-tick robot) capture-frequency) 0)
          (cons! (robot-position robot) points)))
    (eval-robot weights #:step-fn capture-position)
    points))

(define-interactive (draw-robot-path #:optional 
                         (weights (get-nn-weights (current-robot)))
                         (color #(1. 1. 1. 1.)))
  (let ((points (calc-robot-path weights)))
    (cons! (add-line (current-scene) points color) path)))

(define-interactive 
  (plot-robot-path #:optional 
                   (weights (get-nn-weights (current-robot)))
                   (filename "path-plot.pdf")
                   (individual-number 1)
                   (two-views? #f))
  (let ((points (calc-robot-path weights)))
    ;; How do I get the obstacles?
    (mathematica 
     (apply format #f "Export[~a, ~a[~a, ~a, ~a]]" 
            (map sexp->mathematica 
                 (list filename
                       (if two-views?
                           'plotRobotPathAndObstaclesTwoViews
                           'plotRobotPathAndObstacles)
                       points 
                       obstacles 
                       individual-number))))
    (when (called-interactively?)
      (preview filename))))

(define-interactive (reload-mathematica)
  (mathematica "<<\"plot-front.m\""))


(define-interactive (clear-path)
  (for-each (cut remove-actor (current-scene) <>) path)
  (set! path '()))

(define last-fitness-func #f)
(define last-pareto-front (vector))
(define last-pareto-front-index 0)
(define last-seed-fitness #f)
(define last-seed-weights #f)

;; each element is of the form ((#<genes ...> . #<objective values...>) ...)
(define last-results '())

(define population-count 
  ;;4
  10
  )
(define generation-tick #f)

(define-interactive
  (optimize 
   #:optional 
   (fitness-fn
    (let*-values
        (((to-string from-string) (object-tracker
                                   (compose symbol->string procedure-name))))
      (from-string (completing-read
                    "Fitness function: " 
                    (map to-string fitness-functions)
                    ;:history* 'fitness-function
                    #:initial-input 
                    (and last-fitness-func (to-string last-fitness-func))))))
   (max-generations 
    (read-from-string (read-from-minibuffer "Generation count: "
                                            ;:history* 'generation-count
                                            )))
   (seed-population
    (if (and (called-interactively?) (not (null? last-results)))
        (map car (first last-results))
        (list (get-nn-weights (current-robot))))))
  
  (message "nsga-ii optimizing ~a" (fitness-desc fitness-fn))
  (if (called-interactively?) 
      ;; Let's the message be displayed before going into the big
      ;; optimization procedure.
      (block-yield))

  (let* (#;(seed-weights (car seed-population))
         (fitness-fn* (lambda (weights)
                        (with-fluids ((eval-robot-fluid eval-robot-headless))
                             (fitness-fn weights))))
         #;(original-fitness (fitness-fn* seed-weights))
         (objective-count (length (objectives fitness-fn)))
         ;; Had to use with-dynamic-state to make fluids work when crossing
         ;; into C code that called Scheme code.
         (results (nsga-ii-search 
                   fitness-fn*
                   #:objective-count objective-count
                   #:gene-count (neuron-count->weight-count neuron-count) ;504
                   #:population-count population-count
                   #:generation-count max-generations
                   #:generation-tick-func generation-tick
                   #:seed-population seed-population)))
    ;; Get rid of any duplicate individuals.
    (set! results (uniq results))
    (set! last-fitness-func fitness-fn)
    ;(set! last-seed-fitness original-fitness)
    ;(set! last-seed-weights seed-weights)
    (set! results (sort! results (lambda (a b)
                                        (: (cdr a) @ 0 > (cdr b) @ 0))))
    (cons! results last-results)
    (set! last-pareto-front (list->vector results))
    (message "Feasible fitnesses ~a" (map cdr results))
    (when (called-interactively?) 
        (call-interactively 'set-pareto-front-index 0)
        (call-interactively 'plot-front))
    (set! (controller (current-robot)) run-nn-brain)))

(define ql-pid #f)
(define (ql-show filename)
  (when ql-pid
      (kill ql-pid SIGINT)
      (set! ql-pid #f))
  (let ((pid (primitive-fork)))
    (if (= pid 0)
        ;; child
        (begin
          (display "CHILD!")
          (execlp "ql" filename))
        ;; parent
        (set! ql-pid pid))))

(define (my-sleep seconds)
  (usleep (inexact->exact (* seconds 1000))))

(define (uniq lst)
  (let ((hash (make-hash-table)))
    (for-each (lambda (item)
                (hash-table-set! hash item #t)) lst)
    (hash-table-keys hash)))

;; XXX I need an easier way of writing and running tests.

(define (preview filename)
  "Show the file using Preview.app.  When called on a file that's already open, it will update what's shown.  Nice."
  (catch 'system-error
    (lambda ()
      (system* "myexec" "open" "-ga" "Preview.app" filename)
      (system* "myexec" "appswitch" "-fa" "preview")
      (my-sleep 0.2)
      (system* "myexec" "appswitch" "-a" "eracs"))
    (lambda (key . args)
      (mylog "hill-climber" pri-warn "Got system error"))))


;; XXX need to handle display of fitness better.
(define-interactive (plot-front #:optional (filename "tmp-plot.pdf"))
    (let* ((results (vector->list last-pareto-front))
           (points (map cdr results))
           ;; Sort by the first element.
           (sorted-points points 
                          ;; Sorting them isn't necessary here anymore.
                          #;(sort points (lambda (a b)
                                          (< (: a @ 0) (: b @ 0)))))
           (objectives (or (objectives last-fitness-func)
                            '((minimize "objective 1")
                              (minimize "objective 2")))))
      
          (define (raw-fitness->display-fitness fitness-vector)
            (let ((adjustment (map (lambda (obj) 
                                     (case (car obj)
                                       ((maximize) -1)
                                       ((minimize) 1))) 
                                   objectives)))
              (vector* (list->vector adjustment) fitness-vector)))
          
      (mathematica 
       (apply format #f "exportPDF[~a, Show[plotFrontAndPoint[~a, ~a, ~a], AxesLabel -> ~a, AxesOrigin -> {0, 0}, PlotRange -> {{0,12},{0,12}}]];"
              (map sexp->mathematica
                   (list
                    filename
                    (map raw-fitness->display-fitness
                         sorted-points)
                    (1+ last-pareto-front-index)
                    (raw-fitness->display-fitness
                     last-seed-fitness)
                    (map cadr objectives)))))
      ;; Show the exported image.
      (when (called-interactively?)
        (call-interactively 'draw-all-robot-paths)
        (preview filename))))

(define-interactive (am-i-int)
  (message "this-command ~a, this-interactive-command ~a" this-command (fluid-ref (@@ (emacsy command) this-interactive-command)))
  (if (called-interactively?)
      (message "Yes, I was called interactively.")
      (message "No, I was not called interactively.")))

(define-interactive (am-not)
  (am-i-int))

(define-interactive 
  (set-pareto-front-index 
   #:optional 
   (index 
    (read-from-string (read-from-minibuffer "index: "))))
  (set! last-pareto-front-index (mod index (vector-length last-pareto-front)))
  (restart-physics)
  (set! (controller (current-robot)) run-nn-brain)
  (set-nn-weights! (current-robot) (car (: last-pareto-front @ last-pareto-front-index)))
  (when (called-interactively?)
   (draw-robot-path (car (: last-pareto-front @ last-pareto-front-index))
                    (: colors @ last-pareto-front-index))
   
   (plot-front)))

(define-interactive (goto-individual
                     #:optional
                     (number
                      (read-from-string (read-from-minibuffer "Individual number: "))))
  (if (or (not (number? number)) (< number 1) (> number (vector-length last-pareto-front)))
      (message "Invalid individual")
      (call-interactively 'set-pareto-front-index (1- number))))

(define-interactive (next-individual)
  (call-interactively 'set-pareto-front-index (1+ last-pareto-front-index)))

(define-interactive (prev-individual)
  (call-interactively 'set-pareto-front-index (1- last-pareto-front-index)))

(define-key eracs-mode-map (kbd ".") 'next-individual)
(define-key eracs-mode-map (kbd ",") 'prev-individual)

(define-interactive (test-read-brain)
  ;;(call-interactively 'read-brain "run3/exp-hlwp-trial1/individual-2.bin")
  ;;(call-interactively 'read-brain "run4/individual-1.bin")
  ;(call-interactively 'read-brain "/Users/shane/School/uvm/CSYS-395-evolutionary-robotics/eracs/results/r07/ap-jump1-10/individual08.bin")
  ;(call-interactively 'read-brain "/Users/shane/School/uvm/CSYS-395-evolutionary-robotics/eracs/results/r07/ap-jump1-03/individual05.bin")
  (call-interactively 'read-brain "/Users/shane/School/uvm/CSYS-395-evolutionary-robotics/eracs/results/r07/ap-jump1-06/individual02.bin")
  (set! *ditch-width* 1.)
  (update-jump-obstacles)
  (set! *target-position* (vector 0. 4. (- -3. *ditch-width*)))
  (set! eval-robot-time 20.)

  )
