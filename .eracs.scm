; .eracs.scm
;
; XXX Having dot files is probably a bad idea.  They should be more
; explicit and visible.
(use-modules (convenience-lambda)
             ((rnrs) #:select (vector-map vector-for-each mod))
             (system repl server)
             (zeroconf)
             (osc)
             (emacsy emacsy)
             (statprof)
             (logging))

(define-interactive (reload-log-config)
  (log4c-reread))

(let ((orig message))
  (set! message
        (lambda args
          (mylog "emacsy.message" pri-info (apply orig args)))))

;; This should be substituted with
;; (define-advice message (after log4c-message activate)
;;    (log4c-message result . args)
;;    (mylog "emacsy.message" pri-info result))
;;
;; Or as an around
;; (define-advice message (around log4c-message activate)
;;    (log4c-message . args)
;;    (mylog "emacsy.message" pri-info (apply next-advice args)))

(set! emacsy-log-error (lambda (format-msg . args)
                    (apply mylog "emacsy" pri-error format-msg args)))

(set! emacsy-log-warning (lambda (format-msg . args)
                    (apply mylog "emacsy" pri-warn  format-msg args)))

(set! emacsy-log-info (lambda (format-msg . args)
                    (apply mylog "emacsy" pri-info  format-msg args)))

(set! emacsy-log-debug (lambda (format-msg . args)
                    (apply mylog "emacsy" pri-debug  format-msg args)))

(set! *random-state* (random-state-from-platform))
(set! debug-on-error? #f)
; (use-modules (system vm trace))
; (trace-calls-to-procedure make-stack)
(set! %load-hook (lambda (filename)
                   (mylog "load" pri-debug "Loading ~a .." filename)))
(define profile? #f)
(define spawn-threads? #f)

(define (stop-profiling)
    (statprof-stop)
    (statprof-display))

(when profile?
  (statprof-reset 0 50000 #t)
  (statprof-start)
  (add-hook! emacsy-terminate-hook stop-profiling))

;; Let's start a REPL.
(when (and spawn-threads? (not eracs-batch?))
  (spawn-server)
;; Let's start the OSC server and publish it.
  (start-osc-server)
  (spawn-publish-service "" "_osc._udp." "ERACS" 7770)
  ;; CTRL-c should still work but it doesn't after some threads are spawned.
  ;; This next line fixes that.
  #;(restore-signals))

(restore-signals)
;(load "quadruped-experiment.scm")

#;(use-modules (experiment)
             (experiment-buffer)
             (experiment-gen-count-vs-select-attn)
             (oop goops)
             (oop goops save)
             )

;; (load "beer-parameters.scm") 
;; (load "bullet-physics.scm")
;; (load "bullet-physics-car.scm")
;; (load "fode-physics.scm")
;; (load "beer-experiment.scm")

#;(add-hook! post-window-open-hook 
           (lambda ()
             (load-experiment "bullet-7.dat")
             ) 
           #t)
(define-interactive (start-server)
  (spawn-server))
;(load-experiment "experiment-max-speed-1.dat")
;(my-load-file "experiment-max-speed-1.dat")
;(load "experiment-max-speed-1.dat")
;<gen-count-vs-select-attn-dummy>
;(primitive-load "experiment-max-speed-1.dat")
;(load "exhibit-glbuffer-bug.scm")

