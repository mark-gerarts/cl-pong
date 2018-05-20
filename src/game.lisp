(in-package :pong)

;; Objects

(defclass object ()
  ((location
    :initarg :location
    :accessor location)))

(defgeneric get-edges (object)
  (:documentation "Should return the top, right, bottom and left edge, in that
                   order"))

(defclass movable (object)
  ((velocity
    :initarg :velocity
    :initform (vec2 0 0)
    :accessor velocity)))

(defmethod apply-velocity ((movable movable))
  (setf (location movable) (add (location movable) (velocity movable))))

;; Paddle

(defclass paddle (movable)
  ((movement-speed
    :initarg :speed
    :initform *default-movement-speed*
    :accessor movement-speed
    :documentation "The vertical velocity"))
  (:documentation "Location is the coordinate of the bottom left corner"))

(defun make-paddle (location)
  (make-instance 'paddle :location location))

(defmethod display ((paddle paddle))
  (draw-rect (location paddle)
             *paddle-width*
             *paddle-height*
             :fill-paint *primary*))

(defmethod get-edges ((paddle paddle))
  (let* ((location (location paddle))
         (left (x location))
         (bottom (y location))
         (top (+ bottom *paddle-height*))
         (right (+ left *paddle-width*)))
    (values top right bottom left)))

(defmethod check-edges ((paddle paddle))
  (let* ((bottomx (x (location paddle)))
         (topx (+ *paddle-height* bottomx)))
    (print bottomx)
    (cond
      ((< bottomx 0)
       (setf (x (location paddle)) 0))
      ((> topx *height*)
       (setf (x (location paddle)) (- *height* *paddle-height*))))))

(defmethod update-paddle ((paddle paddle))
  (apply-velocity paddle)
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

(defclass ball (movable) ())

(defun make-ball ()
  ;; Create a random starting velocity that is more likely to be horizontal.
  (let* ((velocity (vec2 (- (random 2.0) 1) (- (random 1.0) 0.5)))
         (velocity (normalize velocity))
         (velocity (mult velocity *ball-speed*))
         (location (vec2 *center-x* *center-y*)))
    (make-instance 'ball :velocity velocity :location location)))

(defmethod display ((ball ball))
  (draw-circle (location ball) *ball-radius* :fill-paint *primary*))

(defmethod get-edges ((ball ball))
  (let* ((location (location ball))
         (x (x location))
         (y (y location))
         (r *ball-radius*)
         (top (+ y r))
         (right (+ x r))
         (bottom (- y r))
         (left (- x r)))
    (values top right bottom left)))

(defmethod reverse-direction ((ball ball) direction)
  (case direction
    (:x (setf (x (velocity ball)) (- (x (velocity ball)))))
    (:y (setf (y (velocity ball)) (- (y (velocity ball)))))))

(defmethod collides-with-p ((ball ball) (paddle paddle))
  (multiple-value-bind (btop bright bbottom bleft) (get-edges ball)
    (multiple-value-bind (ptop pright pbottom pleft) (get-edges paddle)
      (and (> btop pbottom) (< bbottom ptop)
           (> bleft pright) (< bright pleft)))))

(defmethod check-for-collision ((ball ball) paddle-l paddle-r)
  (multiple-value-bind (top right bottom left) (get-edges ball)
    (declare (ignore right left))
    (when (> top *height*)
      (progn
        (setf (y (location ball)) (- *height* *ball-radius*))
        (reverse-direction ball :y)))
    (when (minusp bottom)
      (progn
        (setf (y (location ball)) *ball-radius*)
        (reverse-direction ball :y)))
    (when (or (collides-with-p ball paddle-l) (collides-with-p ball paddle-r))
      (reverse-direction ball :x))))

(defmethod update-ball ((ball ball) paddle-l paddle-r)
  (apply-velocity ball)
  (check-for-collision ball paddle-l paddle-r))
