(let ((orig emacsy-mode-line))
  (set! 
   emacsy-mode-line 
   (lambda ()
     (with-buffer (recent-buffer)
                  (format #f "~a FPS ~1,0f sim-time ~1,1f short-time ~1,2f long-time ~1,1f" 
                          (orig) 
                          (get-parameter 'FPS) 
                          (let ((sim (current-sim))) (if sim (sim-time sim) 0.))
                          (time-loop-value (current-robot))
                          (long-time-loop-value (current-robot))
                          
                          )))))
(set-parameter! 'camera-position #(4 4 3))
(load "quadruped2.scm")
(load "active-preferences.scm")
(load "record-robot.scm")
(load "fitness-functions.scm")
(load "colors.scm")

(load "hill-climber.scm")

;(optimize 1)

;(switch-to-brain low-level-brain)
#;(add-hook! post-window-open-hook 
           (lambda ()
             (randomize-brain)
             (set! (controller (current-robot)) run-nn-brain)
             (clear-brain)
             (set! (controller (current-robot)) ap-controller)) 
           #t)

;(load "beer-experiment.scm")
