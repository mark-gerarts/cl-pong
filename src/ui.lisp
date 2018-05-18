(in-package :pong)

(defun draw-background ()
  (draw-rect (vec2 0 0) *width* *height* :fill-paint *background*))

(defun draw-center-line ()
  (let* ((block-height 25)
         (block-width 10)
         (block-padding 20)
         (segment-length (+ block-height block-padding))
         (x (- *center-x* (/ block-width 2)))
         (n (/ *height* segment-length)))
    (loop for i upto n do
      (draw-rect (vec2 x (* i segment-length))
                 block-width
                 block-height
                 :fill-paint *primary*))))
