
#line 43 "boiler-plate.nw"
#| FILENAME
DO NOT EDIT - automatically generated from FILENAME.

#line 22 "boiler-plate.nw"
// Copyright (C) 2012 Shane Celis 
#line 25 "boiler-plate.nw"
/*
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
*/
#line 48 "boiler-plate.nw"
|#

#line 39 "physics-ui.nw"
(use-modules (vector-math))
(use-modules (emacsy emacsy))
(use-modules (convenience-lambda))


#line 14 "physics-ui.nw"
(define physics-tick-hook (make-hook))

#line 19 "physics-ui.nw"
(define (pause?)
  (get-parameter 'pause))

(define (set-pause! value)
  (set-parameter! 'pause value))

#line 26 "physics-ui.nw"
(define-interactive (toggle-pause)
  (if (paused? (current-buffer))
      (begin
        (message "Unpaused.")
        (set! (paused? (current-buffer)) #f))
      (begin
        (message "Paused.")
        (set! (paused? (current-buffer)) #t))))

#line 36 "physics-ui.nw"
(define-key eracs-mode-map (kbd "p") 'toggle-pause)
