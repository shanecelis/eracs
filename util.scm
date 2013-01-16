;(display (current-module))

(define (my-load-file filename)
  (with-backtrace* (lambda () (load filename))))
(export my-load-file)
