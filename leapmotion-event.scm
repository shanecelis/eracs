(define-module (leapmotion-event)
  #:use-module (LeapGuile)
  #:use-module ((LeapGuile-primitive) :renamer (symbol-prefix-proc 'primitive:))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (fold iota)) ;; fold iota
  #:use-module ((emacsy emacsy) #:hide (state position))
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (logging)
  #:use-module ((guile-user) #:select (get-modifier-key-list))
  #:export (leapmotion-event-init
            make-leapmotion-poller
            <leapmotion-event>
            <leapmotion-hand-event>
            <leapmotion-pointable-event>
            le:hand
            le:pointable
            le:frame
            le:event-type))

(define-class <leapmotion-event> (<modifier-key-event>)
  (frame #:getter le:frame #:init-keyword #:le:frame)
  (event-type #:getter le:event-type #:init-keyword #:le:event-type #:init-value 'frame))

(define-class <leapmotion-hand-event> (<leapmotion-event>)
  (hand #:getter le:hand #:init-keyword #:le:hand))

(define-class <leapmotion-pointable-event> (<leapmotion-hand-event>)
  (pointable #:getter le:pointable #:init-keyword #:le:pointable))


#|

How do we describe leap motion events?

lm-circle
lm-tap
lm-swipe
lm-screen-tap
lm-finger-{1,2,3...}
lm-hand-
lm-hand-1-finger-{1,2,3}
lm-oldest-finger
lm-pointer
lm-frame

|#

;; Turn this into debug output.

(define-method (event->kbd (event <leapmotion-event>))
  (let ((mods (next-method)))
    (format #f "~a~a~a" mods "lm-" (le:event-type event))))

;; Wow. I just spent a ton of time debugging because I did this. Don't. Stop!
(define (my-format port . args)
  (apply mylog "leapmotion-event" pri-trace args))

(define-kbd-converter (kbd-entry->leapmotion-event kbd-entry)
  (match (strip-off-modifier-keys kbd-entry)
    ((mod-keys kbd-entry)
     (let* ((regex "^lm-([^ ]*)$")
            (rmatch (string-match regex kbd-entry)))
       (if rmatch
           (make <leapmotion-event> #:modifier-keys mod-keys
                 #:le:event-type (string->symbol (match:substring rmatch 1)))
           #f)))))

(define-class <emacsy-listener> (<SwigDirector-Listener>)
  ;; XXX this plumbing should be hidden
  #:new-function primitive:new-SwigDirector-Listener)

(define-method (onInit (self <emacsy-listener>) (controller <Controller>))
  (my-format #t "Initialized~%"))

(define-method (onConnect (self <emacsy-listener>) (controller <Controller>))
  (my-format #t "Connected~%")
  (enableGesture controller (primitive:Gesture-TYPE-CIRCLE))
  (enableGesture controller (primitive:Gesture-TYPE-KEY-TAP))
  (enableGesture controller (primitive:Gesture-TYPE-SCREEN-TAP))
  (enableGesture controller (primitive:Gesture-TYPE-SWIPE)))

(define-method (onDisconnect (self <emacsy-listener>) (controller <Controller>))
  (my-format #t "Disconnected~%"))

(define-method (onExit (self <emacsy-listener>) (controller <Controller>))
  (my-format #t "Exited~%"))

(define (*List->list List-object)
  "Convert a HandList, PointableList, etc. to a regular scheme list."
  (map (lambda (i) (elem-ref List-object i)) (iota (count List-object))))

(define pi (acos -1))

(define (radians->degrees rad)
  (/ (* rad 180.) pi))

(define-method (onFrame (self <emacsy-listener>) (controller <Controller>))
  ;; Get the most recent frame and report some basic information
  (let ((frame-proc frame)
        (frame (frame controller)))
    (emacsy-event (make <leapmotion-event> 
                    #:le:frame frame 
                    #:discrete-event? #f
                    #:modifier-keys (get-modifier-key-list)))
    (for-each (lambda (finger index)
                (emacsy-event (make <leapmotion-pointable-event> 
                    #:le:frame frame 
                    #:le:pointable (string->symbol (format #f "finger-~a" index))
                    #:discrete-event? #f
                    #:modifier-keys (get-modifier-key-list))))
              (*List->list fingers) (iota (count (fingers frame)) 1))
   (my-format #t "Frame id: ~a, timestamp: ~a, hands: ~a, fingers: ~a, tools: ~a, gestures: ~a~%"
           (id frame)
           (timestamp frame)
           (count (hands frame))
           (count (fingers frame))
           (count (tools frame))
           (count (gestures frame)))
   (unless (empty (hands frame))
     ;; Get the first hand
     (let* ((hand (elem-ref (hands frame) 0))
            (fingers (fingers hand)))
       ;; Check if the hand has any fingers
       (unless (empty fingers)
         ;; Calculate the hand's average finger tip position
         (let ((avg-pos (make <Vector>)))
           (Leap+ avg-pos avg-pos)
           (set! avg-pos (fold Leap+ avg-pos (map tipPosition (*List->list fingers))))
           (set! avg-pos (Leap/ avg-pos (count fingers)))
           (my-format #t "Hand has ~a fingers, average finger tip position ~a~%" (count fingers) avg-pos)))
       
       ;; Get the hand's sphere radius and palm position
       (my-format #t "Hand sphere radius: ~a mm, palm position: ~a~%"
               (sphereRadius hand)
               (palmPosition hand))
       
       ;; Get the hand's normal vector and direction
       (let ((normal (palmNormal hand))
             (direction (direction hand)))
         ;; Calculate the hand's pitch, roll, and yaw angles
         (apply format #t
                "Hand pitch: ~a degrees, roll: ~a degrees, yaw: ~a degrees~%"
                (map radians->degrees
                      (list (pitch direction)
                            (roll direction)
                            (yaw direction)))))))
   
   ;; Get gestures
   (let ((gestures (gestures frame)))
     (for-each 
      (lambda (a-gesture)
        (cond
         ;; Handle Circle Gesture
         ((= (type a-gesture) (primitive:Gesture-TYPE-CIRCLE))
          (let* ((circle (make <CircleGesture> #:args (list a-gesture)))
                 (clockwise? (<= (angleTo 
                                  (direction (pointable circle))
                                  (normal circle))
                                (/ pi 4.)))
                 ;; Calculate angle swept since last frame
                (sweptAngle 
                 (if (or #t (= (state circle) (primitive:Gesture-STATE-START)))
                     0.
                     (let ((previousUpdate 
                            (make <CircleGesture> 
                              #:args (list (gesture (frame-proc controller 1) 
                                                    (id circle))))))
                     (* (- (progress circle) (progress previousUpdate))
                        2 pi)))))
           (my-format 
            #t
            "Circle id: ~a, state: ~a, progress: ~a, radius: ~a, angle ~a ~a ~%"            (id circle)
            (state circle)
            (progress circle)
            (radius circle)
            (radians->degrees sweptAngle)
            (if clockwise?
                "clockwise"
                "counterclockwise"))))
         ;; Swipe Gesture
         ((= (type a-gesture) (primitive:Gesture-TYPE-SWIPE))
          (let ((swipe (make <SwipeGesture> #:args (list a-gesture))))
            (my-format 
             #t
             "Swipe id: ~a, state: ~a, direction: ~a, speed ~a~%"
             (id swipe)
             (state swipe)
             (direction swipe)
             (speed swipe))))
         ;; Key Tap Gesture
         ((= (type a-gesture) (primitive:Gesture-TYPE-KEY-TAP))
          (let ((tap (make <KeyTapGesture> #:args (list a-gesture))))
            (my-format
             #t
             "Key Tap id: ~a, state: ~a, position: ~a, direction: ~a"
             (id tap)
             (state tap)
             (position tap)
             (direction tap))))
         ;; Screen Tap Gesture
         ((= (type a-gesture) (primitive:Gesture-TYPE-SCREEN-TAP))
          (let ((tap (make <ScreenTapGesture> #:args (list a-gesture))))
            (my-format
             #t
             "Screen Tap id: ~a, state: ~a, position: ~a, direction: ~a"
             (id tap)
             (state tap)
             (position tap)
             (direction tap))))))
      (*List->list gestures)))))


(define-method (onFocusGained (self <emacsy-listener>) (controller <Controller>))
  (my-format #t "Focus Gained~%"))

(define-method (onFocusLost (self <emacsy-listener>) (controller <Controller>))
  (my-format #t "Focus Lost~%"))

(define listener #f)
(define controller #f)

(define (leapmotion-event-init)
  ;; Create a sample listener and controller.
  (set! listener (make <emacsy-listener>))
  (set! controller (make <Controller>))
  ;; XXX This plumbing needs to be hidden.
  (setSelf listener listener)
  ;; Have the sample listener receive events from the controller
  (addListener controller listener))

(define (make-leapmotion-poller)
  (set! controller (make <Controller>))
  (lambda ()
    (let ((the-frame (frame controller)))
     (emacsy-event (make <leapmotion-event> 
                     #:le:frame the-frame
                     #:discrete-event? #f
                     #:modifier-keys (get-modifier-key-list)))
     (apply for-each 
      (lambda (the-hand hand-index)
        (emacsy-event (make <leapmotion-hand-event> 
                                    #:le:frame the-frame 
                                    #:le:hand the-hand
                                    #:le:event-type (string->symbol (format #f "hand-~a" hand-index))
                                    #:discrete-event? #f
                                    #:modifier-keys (get-modifier-key-list)))
        (for-each (lambda (the-finger index)
                    (emacsy-event (make <leapmotion-pointable-event> 
                                    #:le:frame the-frame 
                                    #:le:hand the-hand
                                    #:le:event-type (string->symbol (format #f "hand-~a-finger-~a" hand-index index))
                                    #:le:pointable the-finger
                                    #:discrete-event? #f
                                    #:modifier-keys (get-modifier-key-list))))
                  (*List->list (fingers the-hand)) (iota (count (fingers the-frame)) 1)))
      (let ((my-hands (hands the-frame)))
        (list (*List->list my-hands) 
              (iota (count my-hands) 1)))))))

