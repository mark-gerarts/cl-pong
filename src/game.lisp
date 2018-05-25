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
  (let* ((bottomy (y (location paddle)))
         (topy (+ *paddle-height* bottomy)))
    (cond
      ((< bottomy 0)
       (setf (y (location paddle)) 0))
      ((> topy *height*)
       (setf (y (location paddle)) (- *height* *paddle-height*))))))

(defmethod update-paddle ((paddle paddle))
  (apply-velocity paddle)
  (check-edges paddle))

(defmethod get-center ((paddle paddle))
  (let* ((x (x (location paddle)))
         (x (+ x (/ *paddle-width* 2)))
         (y (y (location paddle)))
         (y (+ y (/ *paddle-height* 2))))
    (vec2 x y)))

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
    :accessor paddle)
   (score
    :initform 0
    :accessor score)))

(defun make-player (paddle)
  (make-instance 'player :paddle paddle))

(defmethod display ((player player))
  (display (paddle player)))

(defmethod add-point ((player player))
  (incf (score player)))

;; Ball

(defclass ball (movable) ())

(defun make-ball ()
  ;; We create a random starting velocity that will never point to the bottom or
  ;; top edge. We do this by creating a random poing on the left or right edge
  ;; and make the velocity have this direction.
  (let* ((py (random *height*))
         (px (if (= 1 (random 2)) 0 *width*))
         (center (vec2 *center-x* *center-y*))
         (velocity (subt (vec2 px py) center))
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
  (let* ((r *ball-radius*)
         (location (location ball))
         (leftx (- (x location) r))
         (lefty (- (y location) r))
         (rightx (+ (x location) r))
         (righty (+ (y location) r)))
    (multiple-value-bind (ptop pright pbottom pleft) (get-edges paddle)
      (and (< leftx pright) (> rightx pleft)
           (< lefty ptop) (> righty pbottom)))))

(defmethod check-for-paddle-collision ((ball ball) (paddle paddle))
  (when (collides-with-p ball paddle)
    (progn
      (reverse-direction ball :x)
      ;; Increase the ball velocity if the paddle is moving.
      (incf (y (velocity ball)) (/ (y (velocity paddle)) 2)))))

(defmethod check-for-collision ((ball ball) paddle-l paddle-r)
  (multiple-value-bind (top right bottom left) (get-edges ball)
    (declare (ignore right left))
    ;; The ball hits the top of the screen.
    (when (> top *height*)
      (progn
        (setf (y (location ball)) (- *height* *ball-radius*))
        (reverse-direction ball :y)))
    ;; The ball hits the bottom of the screen.
    (when (minusp bottom)
      (progn
        (setf (y (location ball)) *ball-radius*)
        (reverse-direction ball :y)))
    ;; Collision with either paddle.
    (check-for-paddle-collision ball paddle-r)
    (check-for-paddle-collision ball paddle-l)))

(defmethod update-ball ((ball ball) paddle-l paddle-r)
  (apply-velocity ball)
  (check-for-collision ball paddle-l paddle-r))

(defmethod update-computer ((paddle paddle) (ball ball))
  ;; Naive AI implementation. The paddle moves according to the y-position of
  ;; the ball, limited to a maximum velocity.
  (let* ((paddley (y (get-center paddle)))
         (bally (y (location ball)))
         (dir (if (> paddley bally)
                  (vec2 0 (- *ai-max-movement-speed*))
                  (vec2 0 *ai-max-movement-speed*))))
    (setf (velocity paddle) dir)))
