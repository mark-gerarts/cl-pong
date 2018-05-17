(in-package :pong)

;; Dimensions
(defvar *width* 800)
(defvar *height* 600)
(defvar *center-x* (/ *width* 2))
(defvar *center-y* (/ *height* 2))

;; Paddle variables
(defvar *paddle-width* 10)
(defvar *paddle-height* 60)
(defvar *paddle-offset* 20)

;; Colours
(defvar *background* (vec4 0 0 0 1))
(defvar *primary* (vec4 1 1 1 1))

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

(defmethod draw-background ((this pong))
  (draw-rect (vec2 0 0) *width* *height* :fill-paint *background*))

(defmethod draw ((this pong))
  (draw-background this)
  (display (player this)))

(defun start-pong ()
  (start 'pong))
