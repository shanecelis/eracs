;; experiment-buffer.scm

(define-module (experiment-buffer)
  #:use-module (oop goops)
  #:use-module (oop goops save)
  #:use-module (experiment)
  #:use-module (emacsy emacsy)
;  #:use-module (experiment-gen-count-vs-select-attn)
  #:export (<experiment-buffer>
            load-experiment)
  )

(eval-when (compile load eval)
           ;; Some trickery so we can access private procedures.
           (module-use! (resolve-module '(experiment-buffer)) (resolve-module '(guile-user)))
           #;(module-use! (resolve-module '(experiment-buffer)) (resolve-module '(experiment)))
           
           )

(define-class <experiment-buffer> (<physics-buffer>)
  (experiment #:accessor eb:experiment #:init-value #f)
  (experiment-index #:accessor eb:experiment-index #:init-value 0))

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

#;(define-interactive (next-experiment)
  
  )

(define-interactive (load-experiment #:optional
                                     (filename
                                      (read-file-name "Experiment filename: ")))
  (let ((objects (load-objects filename))
        (buffer (switch-to-buffer filename <experiment-buffer>)))
    (set! (eb:experiment buffer) (assq-ref objects 'experiment))))
