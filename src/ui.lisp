(in-package :pong)

;; Register resources
(register-resource-package 'pong
                           (asdf:system-relative-pathname :pong "assets/"))
(define-font 'pong::fonts-default "RobotoMono-Regular.ttf")

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

(defun draw-score (score location)
  (let* ((posy (- *height* 100))
         (posx (if (eq location :left)
                 (- *center-x* 140)
                 (+ *center-x* 100)))
         (pos (vec2 posx posy)))
    (draw-text (write-to-string score) pos
               :fill-color *primary*
               :font (make-font 'pong::fonts-default 100))))
