;; This should be added to (emacsy contrib osc) or (osc emacsy) something like that.
(define-module (osc-event)
  #:use-module (oop goops)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (<osc-event> osc-path osc-values handle-osc-event))

(define-class <osc-event> (<event>)
  (osc-path #:getter osc-path #:init-keyword #:osc-path)
  (osc-values #:getter osc-values #:init-keyword #:osc-values))

(define-method (event->kbd (event <osc-event>))
    (let* ((path (osc-path event))
           (parsed-path (string-tokenize path (char-set-delete char-set:graphic #\/)))
           (values (osc-values event))
           (start-index 1)                ;; 1 for TouchOSC, 0 for Control
         )
  (format #f "osc-~a" (string-join parsed-path "-"))))

(define-method (write (obj <osc-event>) port)
  (display "#<osc-event " port)
  (display (osc-path obj) port)
  (display " " port)
  (pp (osc-values obj) port)
  (display " " port)
  (display ">" port)
  )

(define-kbd-converter (kbd-entry->osc-event kbd-entry)
    (let ((regex "^(([ACHMsS]-)*)osc-([^ ]*)$"))
    (define (modifier-char->symbol  char)
      (case char 
        ((#\C) 'control)
        ((#\M) 'meta)
        ((#\S) 'shift)
        ((#\A) 'super)
        (else (warn "invalid character")
              '())))
    (define (get-modifier-keys match)
      (let* ((str (match:substring match 1)))
        (if str
            (map modifier-char->symbol 
                 (filter (lambda (x) (not (char=? x #\-))) (string->list str)))
            '())))
    (let ((match (string-match regex kbd-entry)))
      (if match
          (let* ((mod-keys (get-modifier-keys match)))
            (make <osc-event> #:osc-path (match:substring match 3) #:osc-values #f))
          #f))))


(define (handle-osc-event event)
  (emacsy-event (make <osc-event> #:osc-path (car event) #:osc-values (cdr event))))
