(in-package :pong)

(defclass game () ())

(defclass paddle ()
  ((location
    :initarg :location
    :accessor location
    :documentation "The bottom-right corner of a paddle.")))

(defun make-paddle (location)
  (make-instance 'paddle :location location))

(defmethod display ((paddle paddle))
  (draw-rect (location paddle)
             *paddle-width*
             *paddle-height*
             :fill-paint *primary*))

(defclass player ()
  ((paddle
    :initarg :paddle
    :accessor paddle)))

(defun make-player (paddle)
  (make-instance 'player :paddle paddle))

(defmethod display ((player player))
  (display (paddle player)))
