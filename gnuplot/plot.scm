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
;;	Provides a simple scheme interface for plotting data via gnuplot

;; Usage:
;;   Single data sets:
;;	(gnuplot-plot . args)
;;   Plot several data sets on one graph:
;;	(gnuplot-multiplot . args)
;;   Plot a list data points:
;;	(gnuplot-plot-points . args)
;;   Pass arbitrary commands to gnuplot:
;;	(gnuplot . args)
;;
;; Examples:
;;    Single Curves:
;;	(gnuplot-plot 0 1 2 3)			; Plot the given Y values
;;	(gnuplot-plot #(0 1 2 3))
;;	(gnuplot-plot '(0 1 2 3))
;;	(gnuplot-plot #(0 1 2 3) #(0 1 2 3))	; Plot the given X and Y values
;;	(gnuplot-plot '(0 1 2 3) '(0 1 2 3))
;;    Multiple Curves:
;;	(gnuplot-multiplot #(0 1 2 3) #(0 2 4 6))	; Two data sets
;;	(gnuplot-multiplot '(0 1 2 3) '(0 2 4 6))	; Two data sets
;;	(gnuplot-multiplot '((0 1 2 3)(0 1 2 3))
;;			   '((0 1 2 3)(0 2 4 6)))
;;	(gnuplot-multiplot '(#(0 1 2 3)#(0 1 2 3))
;;			   '(#(0 1 2 3)#(0 2 4 6)))
;;    Lists of points:
;;	(gnuplot-plot-points '((1 . 1)(2 . 4)(3 . 9)(4 . 16)(5 . 25)))
;;	(gnuplot-plot-points '((1 1)(2 4)(3 9)(4 16)(5 25))
;;			     '((1 1)(2 2)(3 3)(4 4)))
;;    Set the X axis label:
;;	(gnuplot "set xlabel 'X axis'")

(define-module (gnuplot plot)
  #:use-module (ice-9 popen))

(define gnuplot-port #f)
(define-public gnuplot-line-style "linespoints")

(define (gnuplot-open)
  (set! gnuplot-port (open-output-pipe "./mygnuplot")))

(define-public (gnuplot . cmd)
  (if (not gnuplot-port)
      (gnuplot-open))
  (for-each (lambda (arg) (display arg gnuplot-port))
	    cmd)
  (newline gnuplot-port)
  (force-output gnuplot-port))

(define-public (gnuplot-enddata)
  (display "e" gnuplot-port)
  (newline gnuplot-port))

(define-public (gnuplot-endplot)
  (force-output gnuplot-port))

(define-public (gnuplot-close)
  (if gnuplot-port
      (begin
	(gnuplot-endplot)
	(gnuplot "quit")
	(close-output-port gnuplot-port)
	(set! gnuplot-port #f))))

(define-public (gnuplot-figure n)
  (if (not gnuplot-port)
      (gnuplot-open))
  (gnuplot "set term aqua " n))

(define-public (gnuplot-newplot n)
  (if (not gnuplot-port)
      (gnuplot-open))
  (gnuplot "set style data " gnuplot-line-style)
  (apply gnuplot
	 "plot '-'"
	 (make-list (1- n) ", '-'")))

(define-public (gnuplot-plot-point . point)
  (for-each (lambda (arg)
	      (display arg gnuplot-port)
	      (display " " gnuplot-port))
	    point)
  (newline gnuplot-port))

(define (gnuplot-plot-dataset data)
  (cond ((vector? data)			; #(1 2 3 4)
	 (array-for-each gnuplot-plot-point data))
	((and (list? data)		; '(#(1 2 3 4) #(10 20 30 40))
	      (vector? (car data)))
	 (apply array-for-each gnuplot-plot-point data))
	((and (list? data)		; '(1 2 3 4) 
	      (number? (car data)))
	 (for-each gnuplot-plot-point data))
	((and (list? data)		; '((1 2 3 4)(10 20 30 40))
	      (list? (car data))
	      (number? (caar data)))
	 (apply for-each gnuplot-plot-point data)))
  (gnuplot-enddata))

(define (gnuplot-plot-pointset data)
  (cond ((and (list? data)		; '((1 1)(2 4)(3 9))
	      (list? (car data))
	      (number? (caar data)))
	 (for-each (lambda (arg) (apply gnuplot-plot-point arg))
		   data))
	((and (list? data)		; '((1 . 1)(2 . 4)(3 . 9))
	      (pair? (car data))
	      (number? (caar data)))
	 (for-each (lambda (arg) (gnuplot-plot-point (car arg) (cdr arg)))
		   data))
	(#t
	 (display "Invalid data set")))
  (gnuplot-enddata))

(define-public (gnuplot-multiplot . data)
  (gnuplot-newplot (length data))
  (for-each gnuplot-plot-dataset data)
  (gnuplot-endplot))

(define-public (gnuplot-plot . data)
  (gnuplot-multiplot data))

(define-public (gnuplot-plot-points . data)
  (gnuplot-newplot (length data))
  (for-each gnuplot-plot-pointset data)
  (gnuplot-endplot))

