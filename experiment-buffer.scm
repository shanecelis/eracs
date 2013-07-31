;; experiment-buffer.scm

(define-module (experiment-buffer)
  #:use-module (oop goops)
  #:use-module (oop goops save)
  #:use-module (experiment)
  #:use-module (emacsy emacsy)
  #:use-module (experiment-gen-count-vs-select-attn)
  #:export (<experiment-buffer>
            load-experiment
            load-parameters
            load-champion
            )
  )

(eval-when (compile load eval)
           ;; Some trickery so we can access private procedures.
           (module-use! (resolve-module '(experiment-buffer)) (resolve-module '(guile-user)))
           #;(module-use! (resolve-module '(experiment-buffer)) (resolve-module '(experiment)))
           
           )
;; Defining a whole new class just to capture one value is a bit overkill.
;; Let's try just using local-variables.
(define-class <experiment-buffer> (<physics-buffer>)
  (experiment #:accessor eb:experiment #:init-value #f))

(define-method (emacsy-mode-line (buffer <experiment-buffer>))
  (if (eb:experiment buffer)
      (format #f "~a ~a (Experiment)"
              (buffer-name buffer)
              (emacsy-mode-line (eb:experiment buffer)))
      (next-method)))

(define-method (emacsy-mode-line (exp <experiment>))
  "")

(define-method (emacsy-mode-line (exp <parent-experiment>))
  (format #f "~a trials" (length (exp:child-experiments exp))))

#;(define-interactive (generate-parameters #:optional (buffer (current-buffer)))
  (generate-parameters (eb:experiment buffer)))

(define* (load-parameters #:optional (exp (local-var 'experiment)))
  (let ((ICs (get-ICs exp)))
    ;; XXX just grab the first one
    (set! choose-initial-conditions (car ICs)))
  ;; Set the physics
  (set! physics-class (exp:physics-class exp)))
(register-interactive 'load-parameters load-parameters)

(define* (load-champion #:optional (exp (local-var 'experiment)))
  (load-parameters exp)
  (let ((genomes (get-genomes exp)))
   (set! current-genome (car genomes))))
(register-interactive 'load-champion load-champion)

(define exp-keymap (make-keymap))

(define-key exp-keymap (kbd "o") 'open-child-experiment)
(define-key exp-keymap (kbd "c") 'load-champion)
(define-key exp-keymap (kbd "p") 'load-parameters)

(define-interactive (load-experiment #:optional
                                     (filename
                                      (read-file-name "Experiment filename: ")))
  
  (let ((objects (load-objects filename)))
    (switch-to-buffer filename)
    (use-local-map exp-keymap)
    (set! (local-var 'experiment) (assq-ref objects 'experiment))
    (local-var 'experiment)))

(define-interactive (open-child-experiment 
                     #:optional (i
                                 (read-from-string (read-from-minibuffer "Trial number: "))))
  (let ((exp (local-var 'experiment)))
    (switch-to-buffer (format #f "~a trial ~a/~a" (buffer-name) i (length (exp:child-experiments exp))))
    (use-local-map exp-keymap)
    (set! (local-var 'experiment) (list-ref (exp:child-experiments exp) i))))




