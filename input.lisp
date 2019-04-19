;;;; input.lisp

(in-package #:magnetic-drift)

(defvar *input* nil)

(defclass input ()
  ((dir :accessor dir
        :initform (v! 0 0))
   (cam-dir :accessor cam-dir
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
  (let* ((up (if (cepl.skitter:key-down-p cepl.skitter:key.up) 1.0 0.0))
         (dn (if (cepl.skitter:key-down-p cepl.skitter:key.down) 1.0 0.0))
         (lft (if (cepl.skitter:key-down-p cepl.skitter:key.left) 1.0 0.0))
         (rgt (if (cepl.skitter:key-down-p cepl.skitter:key.right) 1.0 0.0))
         (x (+ (- lft) rgt))
         (y (+ (- dn) up)))
    (setf (x (cam-dir *input*)) x
          (y (cam-dir *input*)) y)
    (v2-n:normalize (cam-dir *input*)))
  (setf (brake *input*) (cepl.skitter:key-down-p cepl.skitter:key.lshift)))
