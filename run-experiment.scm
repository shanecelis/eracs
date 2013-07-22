#!./eracs -b -s ./run-experiment.scm
!#
(use-modules (experiment)
             (experiment-gen-count-vs-select-attn)
             (oop goops save))

(define exp-class <gen-count-vs-select-attn-trial>)
;(define exp-class <gen-count-vs-select-attn-dummy>)
(define trial-count 5)
(define (make-trial) 
  (make-instance exp-class 
                 #:task-count 5 
                 #:max-gen 50 
                 #:max-speed 1.
                 #:physics-class 
                   <fode-physics>
                   ;<bullet-physics-car>
                 ))

(define exp (make <gen-count-vs-select-attn>
              #:child-experiments
              (map (lambda (i) (make-trial)) (iota trial-count))))

(unless (= 1 (length (program-arguments)))
  (format #t "Usage: run-experiment <filename>~%")
  (exit 2))

(generate-parameters! exp)
(format #t "PARAMETERS: ~a~%" (exp:parameters exp))

(run-experiment! exp)
(format #t "DATA: ~a~%" (exp:data exp))

(analyze-data! exp)
(format #t "RESULTS: ~a~%" (exp:results exp))

(define filename (car (program-arguments)))
(call-with-output-file filename
    (lambda (port)
      (save-objects (acons 'experiment exp '()) 
                    port 
                    '() 
                    '((oop goops)
                      (oop goops save)
                      (experiment)
                      (experiment-gen-count-vs-select-attn)))))
(exit 0)


