(defpackage :pong
  (:use :cl :trivial-gamekit)
  (:import-from :cl-bodge :vector-length :normalize)
  (:export :start-pong))
