(in-package :pong)

(defgame pong ()
  ((player
    :accessor player)
   (computer
   :accessor computer)
   (ball
    :accessor ball))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Pong"))

(defmethod post-initialize ((this pong))
  ;; Initialize the players.
  (let* ((paddle-l (make-paddle (vec2 *paddle-offset* *center-y*)))
         (player-l (make-player paddle-l))
         (paddle-r (make-paddle (vec2 (- *width* *paddle-offset*) *center-y*)))
         (player-r (make-player paddle-r)))
    (setf (player this) player-l)
    (setf (computer this) player-r))
  ;; Initialize the ball
  (setf (ball this) (make-ball))
  ;; Bind movement keys.
  (bind-button :up :pressed (lambda () (move (paddle (player this)) :up)))
  (bind-button :up :released (lambda () (halt (paddle (player this)))))
  (bind-button :down :pressed (lambda () (move (paddle (player this)) :down)))
  (bind-button :down :released (lambda () (halt (paddle (player this))))))

(defmethod act ((this pong))
  (let ((paddle-l (paddle (player this)))
        (paddle-r (paddle (computer this))))
    (update-paddle paddle-l)
    (update-paddle paddle-r)
    (update-ball (ball this) paddle-l paddle-r)))

(defmethod draw ((this pong))
  (draw-background)
  (draw-center-line)
  (display (player this))
  (display (computer this))
  (display (ball this)))

(defun start-pong ()
  (start 'pong))
