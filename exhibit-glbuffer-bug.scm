(define my-line #f)

(define my-line-length -1)

(define my-add-actors (make-guardian))

(add-hook! post-window-open-hook 
           (lambda ()
             (set! my-line (add-line (current-scene)
                                     '( #(0 0 0) #(-1 -1 -1) #(1 0 0))))
             ;(my-add-actors my-line)
             ) 
           #t)



(start-server)


(add-hook! physics-tick-hook
           (lambda ()
             (when (current-scene)
               (add-line (current-scene)
                        '(#(0 0 0) #(1 1 1)))
               (add-sphere (current-scene)
                           #(0 0 0)
                           1)
               (when my-line
                 (format #t "my-line-length is ~a~%" my-line-length)
                 (set! my-line-length (+ -0.1 my-line-length))
                 (update-line my-line (list #(0 0 0) (let ((v my-line-length))
                                                       (vector v v v)))))
               (when (and my-line (<  my-line-length -5))
                 (remove-actor (current-scene) my-line)
                 (remove-actor (current-scene) my-line)
                 (set! my-line #f)
                 (set! my-line (add-line (current-scene)
                                     '( #(0 0 0) #(-1 -1 -1) #(1 0 0))))
                 )
               (cleanup-actors)
               #;(let loop ((item (my-add-actors)))
                 (when item
                   (format #t "released object ~a from guardian~%" item)
                   
                   (loop (my-add-actors))))
               #;(gc)
               )))
