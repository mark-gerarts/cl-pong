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

(defmethod handle-score ((this pong) (player player))
  (add-point player)
  (sleep 1)
  (setf (ball this) (make-ball)))

(defmethod check-point-score ((this pong))
  (multiple-value-bind (top right bottom left) (get-edges (ball this))
    (declare (ignore top bottom))
    (when (< left 0)
      (handle-score this (computer this)))
    (when (> right *width*)
      (handle-score this (player this)))))

(defmethod post-initialize ((this pong))
  ;; Initialize the players.
  (let* ((paddle-l (make-paddle (vec2 *paddle-offset* *center-y*)))
         (player-l (make-player paddle-l))
         (paddle-r (make-paddle (vec2
                                 (- *width* *paddle-offset* *paddle-width*)
                                 *center-y*)))
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
        (paddle-r (paddle (computer this)))
        (ball (ball this)))
    (update-computer paddle-r ball)
    (update-paddle paddle-l)
    (update-paddle paddle-r)
    (update-ball ball paddle-l paddle-r)
    (check-point-score this)))

(defmethod draw ((this pong))
  (draw-background)
  (draw-center-line)
  (display (player this))
  (display (computer this))
  (display (ball this))
  ;; Draw the scores.
  (draw-score (score (player this)) :left)
  (draw-score (score (computer this)) :right))

(defun start-pong ()
  (start 'pong))
