(in-package :pong)

(defclass game () ())

;; Paddle

(defclass paddle ()
  ((location
    :initarg :location
    :accessor location
    :documentation "The bottom-right corner of a paddle.")
   (velocity
    :initform (vec2 0 0)
    :accessor velocity)
   (movement-speed
    :initarg :speed
    :initform *default-movement-speed*
    :accessor movement-speed
    :documentation "The vertical velocity")))

(defun make-paddle (location)
  (make-instance 'paddle :location location))

(defmethod display ((paddle paddle))
  (draw-rect (location paddle)
             *paddle-width*
             *paddle-height*
             :fill-paint *primary*))

(defmethod check-edges ((paddle paddle))
  (let* ((bottomx (x (location paddle)))
         (topx (+ *paddle-height* bottomx)))
    (cond
      ((< bottomx 0)
       (setf (x (location paddle)) 0))
      ((> topx *height*)
       (setf (x (location paddle)) (- *height* *paddle-height*))))))

(defmethod update ((paddle paddle))
  (setf (location paddle) (add (location paddle) (velocity paddle)))
  (check-edges paddle))

(defmethod move ((paddle paddle) direction)
  (let* ((vy (movement-speed paddle))
         (velocity (case direction
                     (:up (vec2 0 vy))
                     (:down (vec2 0 (- vy))))))
    (setf (velocity paddle) velocity)))

(defmethod halt ((paddle paddle))
  (setf (velocity paddle) (vec2 0 0)))

;; Player

(defclass player ()
  ((paddle
    :initarg :paddle
    :accessor paddle)))

(defun make-player (paddle)
  (make-instance 'player :paddle paddle))

(defmethod display ((player player))
  (display (paddle player)))

;; Ball

(defclass ball ()
  ((location
    :initform (vec2 *center-x* *center-y*)
    :accessor location)
   (velocity
    :initarg :velocity
    :accessor velocity)))

(defun make-ball (&key (velocity (vec2 0 0)))
  (make-instance 'ball :velocity velocity))

(defmethod display ((ball ball))
  (draw-circle (location ball) *ball-radius* :fill-paint *primary*))

(defmethod reverse-direction ((ball ball) direction)
  (case direction
    (:x (setf (x (velocity ball)) (- (x (velocity ball)))))
    (:y (setf (y (velocity ball)) (- (y (velocity ball)))))))

(defmethod check-edges ((ball ball))
  (let* ((location (location ball))
         (x (x location))
         (y (y location))
         (r *ball-radius*)
         (top (+ y r))
         (right (+ x r))
         (bottom (- y r))
         (left (- x r)))
    (when (> top *height*) (progn
                             (setf (y (location ball)) (- *height* r))
                             (reverse-direction ball :y)))
    (when (minusp bottom) (progn
                             (setf (y (location ball)) r)
                             (reverse-direction ball :y)))
    (when (> right *width*) (progn
                             (setf (x (location ball)) (- *width* r))
                             (reverse-direction ball :x)))
    (when (minusp left) (progn
                          (setf (x (location ball)) r)
                          (reverse-direction ball :x)))))

(defmethod update ((ball ball))
  (setf (location ball) (add (location ball) (velocity ball)))
  (check-edges ball))
