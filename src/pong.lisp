(in-package :pong)

(defgame pong ()
  ((player
    :accessor player))
  (:viewport-width *width*)
  (:viewport-height *height*)
  (:viewport-title "Pong"))

(defmethod post-initialize ((this pong))
  (let* ((paddle-left (make-paddle (vec2 *paddle-offset* *center-y*)))
         (player-left (make-player paddle-left)))
    (setf (player this) player-left)))

(defmethod act ((this pong)))


(defmethod draw ((this pong))
  (draw-background)
  (draw-center-line)
  (display (player this)))

(defun start-pong ()
  (start 'pong))
