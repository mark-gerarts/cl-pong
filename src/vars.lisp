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
