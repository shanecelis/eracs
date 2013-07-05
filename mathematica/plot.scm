;; http://sourceware.org/ml/guile/1998-08/msg00344.html

;;;;    Copyright (C) 1997 Glenn Moloney
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

;; Description:
;;
;;	Provides a simple scheme interface for plotting data via mathematica

;; Usage:
;;   Single data sets:
;;	(mathematica-plot . args)
;;   Plot several data sets on one graph:
;;	(mathematica-multiplot . args)
;;   Plot a list data points:
;;	(mathematica-plot-points . args)
;;   Pass arbitrary commands to mathematica:
;;	(mathematica . args)
;;
;; Examples:
;;    Single Curves:
;;	(mathematica-plot 0 1 2 3)			; Plot the given Y values
;;	(mathematica-plot #(0 1 2 3))
;;	(mathematica-plot '(0 1 2 3))
;;	(mathematica-plot #(0 1 2 3) #(0 1 2 3))	; Plot the given X and Y values
;;	(mathematica-plot '(0 1 2 3) '(0 1 2 3))
;;    Multiple Curves:
;;	(mathematica-multiplot #(0 1 2 3) #(0 2 4 6))	; Two data sets
;;	(mathematica-multiplot '(0 1 2 3) '(0 2 4 6))	; Two data sets
;;	(mathematica-multiplot '((0 1 2 3)(0 1 2 3))
;;			   '((0 1 2 3)(0 2 4 6)))
;;	(mathematica-multiplot '(#(0 1 2 3)#(0 1 2 3))
;;			   '(#(0 1 2 3)#(0 2 4 6)))
;;    Lists of points:
;;	(mathematica-plot-points '((1 . 1)(2 . 4)(3 . 9)(4 . 16)(5 . 25)))
;;	(mathematica-plot-points '((1 1)(2 4)(3 9)(4 16)(5 25))
;;			     '((1 1)(2 2)(3 3)(4 4)))
;;    Set the X axis label:
;;	(mathematica "set xlabel 'X axis'")

(define-module (mathematica plot)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (logging)
  #:use-module (os process)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export (sexp->mathematica)
  )

(define mathematica-pid #f)
(define mathematica-port #f)
(define mathematica-input #f)

(define mathematica-input-buffer "")
(define mathematica-input-buffer-pos 0)

(define (mathematica-open)
  (match (run-with-pipe "w" "./mymathematica")
    ((pid . (input . output))
     (set! mathematica-pid pid)
     (set! mathematica-port output)
     (set! mathematica-input input)
     #;(call-with-new-thread 
      (let ((state (current-dynamic-state)))
       (lambda ()
         ;; XXX ran into a bug
         ;;fluid_ref (fluid=0x1016d0690) at fluids.c:226
         ;;226       SCM fluids = DYNAMIC_STATE_FLUIDS (SCM_I_CURRENT_THREAD->dynamic_state);
         (set-current-dynamic-state state)
         (while mathematica-input
           (mylog "mathematica.output" pri-debug "~a" (read-line mathematica-input) ))))))
    ((pid . output)
     (set! mathematica-pid pid)
     (set! mathematica-port output)
     (set! mathematica-input #f)
     (mylog "mathematica" pri-debug "Got an output port ~a" mathematica-port)
     )
    (else
     (throw 'unable-to-open-mathematica)
     )
    )
  ;(set! mathematica-port (open-input-output-pipe "./mymathematica"))
  #;(mathematica "<<JavaGraphics`")
  #;(mathematica "<<\"plot-front.m\"")
  )

#;(define (expect regex timeout)
  ;; read

  (string-match regex )
  )

(define (read-non-block port)
  (call-with-output-string 
   (lambda (str-port)
     (let ((last-char #\a))
      (while (and (char-ready? port) (not (eof-object? last-char)))
                                        ;(display (read-line port) str-port)
        (set! last-char (read-char port))
        (write-char last-char str-port))))))

(define (read-string-non-blocking port)
  (call-with-output-string 
   (lambda (str-port)
     (while (char-ready? port)
       (write-char (read-char port) str-port)))))

(define-public (mathematica . cmd)
  (if (not mathematica-port)
      (mathematica-open))
  (for-each (lambda (arg) 
              (display arg mathematica-port)
              (newline mathematica-port)
              (force-output mathematica-port)
              (mylog "mathematica.input" pri-debug "~a" arg)
              ;(unread-char (read-char mathematica-port) mathematica-port)
              ;(mylog "mathematica.output" pri-debug "~a" (drain-input mathematica-port))
              ;(mylog "mathematica.output" pri-debug "~a" (read-string-non-blocking mathematica-input))
           
              )
            cmd))

(define-public (mathematica-enddata)
  ;(display "e" mathematica-port)
  (newline mathematica-port))

(define-public (mathematica-endplot)
  (force-output mathematica-port))

(define-public (mathematica-close)
  (if mathematica-port
      (begin
        (mathematica-endplot)
        (mathematica "Quit[];")
        (close-output-port mathematica-port)
        (set! mathematica-port #f))))

(define-public (mathematica-figure n)
  (if (not mathematica-port)
      (mathematica-open))
  ;(mathematica "set term aqua " n)
  
  )

(define-public (mathematica-newplot n)
  (if (not mathematica-port)
      (mathematica-open))
  ;(mathematica "set style data " mathematica-line-style)
  ;; (apply mathematica
  ;;        "plot '-'"
  ;;        (make-list (1- n) ", '-'"))
  
  )

(define-public (mathematica-plot-point . point)
  (for-each (lambda (arg)
              (display arg mathematica-port)
              (display " " mathematica-port))
            point)
  (newline mathematica-port))

(define (mathematica-plot-dataset data)
  (cond ((vector? data)			; #(1 2 3 4)
         (array-for-each mathematica-plot-point data))
        ((and (list? data)		; '(#(1 2 3 4) #(10 20 30 40))
              (vector? (car data)))
         (apply array-for-each mathematica-plot-point data))
        ((and (list? data)		; '(1 2 3 4) 
              (number? (car data)))
         (for-each mathematica-plot-point data))
        ((and (list? data)		; '((1 2 3 4)(10 20 30 40))
              (list? (car data))
              (number? (caar data)))
         (apply for-each mathematica-plot-point data)))
  (mathematica-enddata))

(define (mathematica-plot-pointset data)
  (cond ((and (list? data)		; '((1 1)(2 4)(3 9))
              (list? (car data))
              (number? (caar data)))
         (for-each (lambda (arg) (apply mathematica-plot-point arg))
                   data))
        ((and (list? data)		; '((1 . 1)(2 . 4)(3 . 9))
              (pair? (car data))
              (number? (caar data)))
         (for-each (lambda (arg) (mathematica-plot-point (car arg) (cdr arg)))
                   data))
        (#t
         (display "Invalid data set")))
  (mathematica-enddata))

(define-public (mathematica-multiplot . data)
  (mathematica-newplot (length data))
  (for-each mathematica-plot-dataset data)
  (mathematica-endplot))

(define-public (mathematica-plot . data)
  (mathematica-multiplot data))

(define-public (mathematica-plot-points . data)
  (mathematica-newplot (length data))
  (for-each mathematica-plot-pointset data)
  (mathematica-endplot))

(define-public (list->mathematica-list xs)
  "Convert the list into a mathematica list. Does not descend into child lists."
  (format #f "{~{~a~^, ~}}" xs))

(define-public (list->mathematica-list* xs)
  "Convert sublists into mathematica lists."
  (list->mathematica-list (map (lambda (x)
                                 (if (list? x)
                                     (list->mathematica-list* x)
                                     x)) xs)))

(define (vector->mathematica-list xs)
  (list->mathematica-list (vector->list xs)))

(define (vector->mathematica-list* xs)
  (list->mathematica-list* (vector->list xs)))

(define-public (mathematica-quit)
  (when mathematica-pid
    (mathematica "Quit[]")
    (waitpid mathematica-pid)
    (set! mathematica-pid #f)))

(define-method (sexp->mathematica (sexp <vector>))
  (sexp->mathematica (vector->list sexp)))

(define-method (sexp->mathematica (sexp <string>))
  (format #f "\"~a\"" sexp))

(define-method (sexp->mathematica (sexp <integer>))
  (format #f "~d" sexp))

(define-method (sexp->mathematica (sexp <real>))
  (format #f "~f" sexp))

(define-method (sexp->mathematica (sexp <symbol>))
  (format #f "~a" (symbol->string sexp)))

(define-method (sexp->mathematica (sexp <list>))
  (format #f "{~{~a~^,~}}" (map sexp->mathematica sexp)))

(define-method (sexp->mathematica (sexp <boolean>))
  (if sexp
      "True"
      "False"))
