;; osc-registry.scm
(define-module (osc-registry)
  #:use-module (osc)
  #:use-module (oop goops)
  #:use-module (emacsy util))

(define-class-public <osc-registry> ()
  (host #:accessor host #:init-keyword #:host)
  (port #:accessor port #:init-keyword #:port #:init-value 9000)
  (osc-entries #:accessor osc-entries #:init-value '()))

(define-class <osc-registry-entry> ()
  (getter #:getter getter #:init-keyword #:getter)
  (setter #:getter setter #:init-keyword #:setter)
  ;; last-value should actually be on a per-host basis to be general.
  (last-value #:accessor last-value #:init-keyword #:last-value #:init-value #f)
  (path #:getter path #:init-keyword #:path))

(define-method-public (osc-register (r <osc-registry>) my-path getter setter)
  (set! (osc-entries r) 
        (cons 
         (make <osc-registry-entry> 
           #:path my-path #:getter getter #:setter setter #:last-value (getter))
         (osc-entries r))))

(define-method-public (osc-push (r <osc-registry>))
  "Push the current values to OSC."
  (for-each 
   (lambda (entry)
     (let ((value ((getter entry))))
       (when (or #f (not (equal? value (last-value entry))))
         (set! (last-value entry) value)
         (if (last-osc-client)
          (send-osc-message 
           (last-osc-client);(host r) 
           (port r) 
           (path entry)
           value)))))
   (osc-entries r)))
