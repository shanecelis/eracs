(define-module (leapmotion-event)
  #:use-module (LeapGuile)
  #:use-module ((LeapGuile-primitive) :renamer (symbol-prefix-proc 'primitive:))
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1) #:select (fold)) ;; fold
  #:use-module ((emacsy emacsy) #:hide (state position))
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (logging)
  #:export (leapmotion-event-init
            <leapmotion-event>
            lme:frame))

(define-class <leapmotion-event> (<key-event>)
  (lme:frame #:getter lme:frame #:init-keyword #:lme:frame))

#|

How do we describe leap motion events?

lm-circle
lm-finger
lm-pointer
lm-frame

|#

;; Turn this into debug output.
(define (format port . args)
  (apply mylog "leapmotion-event" pri-trace args))

(define-method (event->kbd (event <leapmotion-event>))
  (let ((mods (map string (map modifier-symbol->char (modifier-keys event)))))
    (string-join `(,@mods "lm-frame") "-")))

(define-kbd-converter (kbd-entry->leapmotion-event kbd-entry)
    (let ((regex "^(([ACHMsS]-)*)lm-([^ ]*)$"))
    (define (get-modifier-keys match)
      (let* ((str (match:substring match 1)))
        (if str
            (map modifier-char->symbol 
                 (filter (lambda (x) (not (char=? x #\-))) (string->list str)))
            '())))
    (let ((match (string-match regex kbd-entry)))
      (if match
          (let* ((mod-keys (get-modifier-keys match)))
            (make <leapmotion-event> #:modifier-keys mod-keys))
          #f))))

(define-class <emacsy-listener> (<SwigDirector-Listener>)
  ;; XXX this plumbing should be hidden
  #:new-function primitive:new-SwigDirector-Listener)

(define-method (onInit (self <emacsy-listener>) (controller <Controller>))
  (format #t "Initialized~%"))

(define-method (onConnect (self <emacsy-listener>) (controller <Controller>))
  (format #t "Connected~%")
  (enableGesture controller (primitive:Gesture-TYPE-CIRCLE))
  (enableGesture controller (primitive:Gesture-TYPE-KEY-TAP))
  (enableGesture controller (primitive:Gesture-TYPE-SCREEN-TAP))
  (enableGesture controller (primitive:Gesture-TYPE-SWIPE)))

(define-method (onDisconnect (self <emacsy-listener>) (controller <Controller>))
  (format #t "Disconnected~%"))

(define-method (onExit (self <emacsy-listener>) (controller <Controller>))
  (format #t "Exited~%"))

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
    (emacsy-event (make <leapmotion-event> #:lme:frame frame
                        #:modifier-keys '(shift)
                        #:command-char #\A ;; bullshit
                        ))
   (format #t "Frame id: ~a, timestamp: ~a, hands: ~a, fingers: ~a, tools: ~a, gestures: ~a~%"
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
           (format #t "Hand has ~a fingers, average finger tip position ~a~%" (count fingers) avg-pos)))
       
       ;; Get the hand's sphere radius and palm position
       (format #t "Hand sphere radius: ~a mm, palm position: ~a~%"
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
           (format 
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
            (format 
             #t
             "Swipe id: ~a, state: ~a, direction: ~a, speed ~a~%"
             (id swipe)
             (state swipe)
             (direction swipe)
             (speed swipe))))
         ;; Key Tap Gesture
         ((= (type a-gesture) (primitive:Gesture-TYPE-KEY-TAP))
          (let ((tap (make <KeyTapGesture> #:args (list a-gesture))))
            (format
             #t
             "Key Tap id: ~a, state: ~a, position: ~a, direction: ~a"
             (id tap)
             (state tap)
             (position tap)
             (direction tap))))
         ;; Screen Tap Gesture
         ((= (type a-gesture) (primitive:Gesture-TYPE-SCREEN-TAP))
          (let ((tap (make <ScreenTapGesture> #:args (list a-gesture))))
            (format
             #t
             "Screen Tap id: ~a, state: ~a, position: ~a, direction: ~a"
             (id tap)
             (state tap)
             (position tap)
             (direction tap))))))
      (*List->list gestures)))))


(define-method (onFocusGained (self <emacsy-listener>) (controller <Controller>))
  (format #t "Focus Gained~%"))

(define-method (onFocusLost (self <emacsy-listener>) (controller <Controller>))
  (format #t "Focus Lost~%"))

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

