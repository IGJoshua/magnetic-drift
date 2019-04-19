;;;; input.lisp

(in-package #:magnetic-drift)

(defvar *input* nil)

(defclass input ()
  ((dir :accessor dir
        :initform (v! 0 0))
   (brake :accessor brake
          :initform nil)))

(defun handle-input ()
  (let* ((w (if (cepl.skitter:key-down-p cepl.skitter:key.w) 1.0 0.0))
         (a (if (cepl.skitter:key-down-p cepl.skitter:key.a) 1.0 0.0))
         (s (if (cepl.skitter:key-down-p cepl.skitter:key.s) 1.0 0.0))
         (d (if (cepl.skitter:key-down-p cepl.skitter:key.d) 1.0 0.0))
         (x (+ (- a) d))
         (y (+ (- s) w)))
    (setf (x (dir *input*)) x
          (y (dir *input*)) y)
    (v2-n:normalize (dir *input*)))
  (setf (brake *input*) (cepl.skitter:key-down-p cepl.skitter:key.lshift)))
