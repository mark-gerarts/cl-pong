(in-package :pong)

(defgame pong ()
  ((player
    :accessor player)
   (ball
    :accessor ball))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Pong"))

(defmethod post-initialize ((this pong))
  ;; Initialize the players.
  (let* ((paddle-left (make-paddle (vec2 *paddle-offset* *center-y*)))
         (player-left (make-player paddle-left)))
    (setf (player this) player-left))
  ;; Initialize the ball
  (let ((ball (make-ball :velocity (vec2 -1 0.1))))
    (setf (ball this) ball))
  ;; Bind movement keys.
  (bind-button :up :pressed (lambda () (move (paddle (player this)) :up)))
  (bind-button :up :released (lambda () (halt (paddle (player this)))))
  (bind-button :down :pressed (lambda () (move (paddle (player this)) :down)))
  (bind-button :down :released (lambda () (halt (paddle (player this))))))

(defmethod act ((this pong))
  (update (paddle (player this)))
  (update (ball this)))


(defmethod draw ((this pong))
  (draw-background)
  (draw-center-line)
  (display (player this))
  (display (ball this)))

(defun start-pong ()
  (start 'pong))
