
;; Lisp Preamble

#| physics-ui.scm
DO NOT EDIT - automatically generated from physics-ui.scm.

;; Copyright

Copyright (C) 2012 Shane Celis 

;; License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

|#

;; Load modules.

(use-modules (vector-math))
(use-modules (emacsy emacsy))
(use-modules (convenience-lambda))


;; Variables

(define physics-tick-hook (make-hook))

;; Procedures

(define (pause?)
  (get-parameter 'pause))

(define (set-pause! value)
  (set-parameter! 'pause value))

;; Commands

(define-interactive (toggle-pause)
  (if (paused? (current-buffer))
      (begin
        (message "Unpaused.")
        (set! (paused? (current-buffer)) #f))
      (begin
        (message "Paused.")
        (set! (paused? (current-buffer)) #t))))

;; Key Bindings

(define-key eracs-mode-map (kbd "p") 'toggle-pause)

