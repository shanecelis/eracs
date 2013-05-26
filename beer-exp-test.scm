(define g1 (make-genome-for-n-ctrnn node-count))
(format #t "genome ~a~%" g1)
(format #t "fitness ~a~%" (beer-selective-attention g1))
(array-fill! g1 1.)
(format #t "fitness ~a~%" (beer-selective-attention g1))
(array-fill! g1 -1.)
(format #t "fitness ~a~%" (beer-selective-attention g1))

(format #t "fitness ~a~%" (beer-selective-attention))


(exit 0)
