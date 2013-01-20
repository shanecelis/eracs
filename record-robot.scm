(use-modules (emacsy emacsy))

(define (osc-set! path value)
  (if (last-osc-client)
      (send-osc-message (last-osc-client) 9000 path value)))

(define (button-set! path bool)
  (osc-set! path (if bool 
                     1.0
                     0.0)))

(define (record-button-set! bool)
  (button-set! "/2/record" bool))

(define (play-button-set! bool)
  (button-set! "/2/play" bool))

(define robot-macro-time-max nn-time-period)
(define robot-macros '())

(define (button-on? value)
  (> value 0))

(define recording? #f)
(define playing? #f)

(define (on-robot-macro-termination)
  (button-set! "/2/play" #f)
  ;;(osc-set! "/2/time-slider" 0)
  (if (and playing? (not (null? robot-macros)))
      ;; play it again.
      ;; doing this causes a vm-error with an unresumable continuation.
      ;(play-robot)
      #f))

;; (define (while-playing time)
;;   (format #t "while-playing ~a\n" time)
;;   (osc-set! "/2/time-slider" (/ time robot-macro-time-max)))

;; Using an extra lambda so while-playing can be redefined.
;; (add-hook! executing-temporal-kbd-macro-hook #.\ (while-playing %))

(define* (shift-time-for-kbd-macro! macro #:optional (time-offset 0))
  (for-each #.\ (slot-set! % 'time (- (time %) time-offset)) macro)
  macro)

(define (flatten lists-of-lists)
        (apply append lists-of-lists))

(define (merge-kbd-macro-by-time macros)
  (sort (flatten macros) #.\ (> (time %1) (time %2))))

;; last-nn-training-values holds a list of cons cells (sensor-input
;; . target-angles) in reverse chronological order.
(define last-nn-training-values '())

(define (record-nn-training-values)
  (when (= 0 (mod (tick-count robot) controller-update-frequency))
    (cons! (cons (nn-input robot) (nn-output robot)) 
           last-nn-training-values)))

(define-interactive (export-nn-training-values 
                     #:optional (filename (read-from-minibuffer "Filename: ")))
  (call-with-output-file filename 
    (lambda (port)
      (format port "~a ~a ~a~%" 
              (length last-nn-training-values)
              (vector-length (caar last-nn-training-values))
              (vector-length (cdar last-nn-training-values)))
      (for-each (lambda (in-out)
                  (format port "~{~a ~}~%" (vector->list (car in-out)))
                  (format port "~{~a ~}~%" (vector->list (cdr in-out))))
                last-nn-training-values))))

(define-interactive (train-nn 
                     #:optional (training-values last-nn-training-values))
  (let ((mse 
          (nn-train-epoch (nn-brain (current-robot)) 
                          (map car training-values) 
                          (map cdr training-values)
                          5000
                          0.001
                          )))
    (message "Trained NN had an MSE of ~a" mse)))

;; Maybe I want something like this:
;; (controller->time-series
;; (time-series->controller

(define-interactive record-robot 
  (let ((record-start #f)) ;; This is how you get a static variable
                           ;; scoped within one function.
    (lambda* (#:optional (start? #t))
             (when start?
               (let ((prev-brain (controller robot)))
                (set! (controller robot) low-level-brain)
                (set! record-start (emacsy-time))
                (kmacro-start-macro)
                (emacsy-event (make <dummy-event>)) ; Add a dummy event to
                                        ; preserve the start time.
                ;(osc-set! "/2/time-slider" 0.)
                (set! last-nn-training-values '())
                (add-hook! physics-tick-hook record-nn-training-values)
                (block-until 
                 #.\ (let ((time (- (emacsy-time) record-start)))
                       ;(osc-set! "/2/time-slider" (/ time robot-macro-time-max))
                       (or (not defining-kbd-macro?) (> time robot-macro-time-max))))
                (record-button-set! #f)
                (set! (controller robot) prev-brain)
                (remove-hook! physics-tick-hook record-nn-training-values)))
             (when defining-kbd-macro?
               (kmacro-end-macro)
               (if record-start
                   (shift-time-for-kbd-macro! last-kbd-macro record-start))
               (set! record-start #f)
               (cons! last-kbd-macro robot-macros)
               (osc-set! "/2/time-slider" 0)))))

(define-interactive (play-robot #:optional (play? #t))
  (if (and play? (not (null? robot-macros)))
      (begin
        (display "PLAY ON\n")
        (execute-temporal-kbd-macro (merge-kbd-macro-by-time robot-macros))
        (play-button-set! #f))  
      ;; Not sure how to stop the macros.
      (display "PLAY OFF\n")))

(add-hook! kbd-macro-termination-hook on-robot-macro-termination)

(define-interactive (osc-play #:optional (event this-command-event))
  (set! playing? (button-on? (car (osc-values event))))
  (play-robot playing?))

(define-interactive (osc-record #:optional (event this-command-event))
  (set! recording? (button-on? (car (osc-values event))))
  (record-robot recording?))

(define-interactive (osc-erase #:optional (event this-command-event))
  (when (button-on? (car (osc-values event)))
    (set! robot-macros 
          (if (pair? robot-macros)
              (begin 
                (message "Erased 1 macro; ~a macros left.\n" 
                         (1- (length robot-macros)))
                (cdr robot-macros))
              (begin
                (message "No macros to erase.")
                '())))))

(define-interactive (goto-time #:optional (arg #f))
  (let ((new-time (read-time arg)))
   (time-loop-value-set! robot new-time)
   (message "Controller time ~1,2f" new-time)))

(define-interactive (read-time #:optional (value #f))
  "Read the a [0, 1] time from user through either the current OSC
event or an input."
  (if value
      value
      (if (is-a? this-command-event <osc-event>)
          (car (osc-values this-command-event))
          ;; Must ask the user directly.
          (read-from-string (read-from-minibuffer (format #f "Goto time (current time ~1,2f): " (time-loop-value robot)))))))

(define-interactive (continue-runloop #:optional (event this-command-event))
  (if (is-a? event <osc-event>)
      (if (button-on? (car (osc-values event)))
          (time-loop-value-set! robot #f)
          (time-loop-value-set! robot (time-loop-value robot)))
      ;; What to do? Ask? No. Just continue running.
      (time-loop-value-set! robot #f)))


;(define-key eracs-mode-map (kbd "osc-2-play")   'osc-play)
;(define-key eracs-mode-map (kbd "osc-2-record") 'osc-record)
;(define-key eracs-mode-map (kbd "osc-2-erase")  'osc-erase)
(define-key eracs-mode-map (kbd "osc-2-play")   'continue-runloop)
