(use-modules (experiment)
             (experiment-gen-count-vs-select-attn)
             (oop goops save))

(define exp-class <gen-count-vs-select-attn>)
(define trial-count 1)
(define (make-trial) 
  (make-instance exp-class #:task-count 3 #:max-gen 1))

(set! physics-class <bullet-physics-car>)

(unless (= 1 (length (program-arguments)))
    (format #t "Usage: redo-experiment <filename>~%")
    (exit 2))

(define objects (load-objects (car (program-arguments))))
(format #f "objects ~a" objects)

(define exp (assq-ref objects 'experiment))
;(define exp (car (exp:child-experiments experiment)))
;(define exp2 (deep-clone exp))
;; last-results was being clobbered by (optimize)... argh!
;(define last-results2 (exp:results exp2))
;(format #t "LAST RESULTS: ~a~%" last-results2)

;(generate-parameters! exp)
(format #t "PARAMETERS: ~a~%" (exp:parameters exp))

;(run-experiment! exp)
(format #t "DATA: ~a~%" (exp:data exp))

(analyze-data! exp)
(format #t "RESULTS: ~a~%" (exp:results exp))
;(format #t "LAST RESULTS: ~a~%" last-results2)
;(format #t "LAST RESULTS2: ~a~%" (exp:results exp2))

(exit 0)


