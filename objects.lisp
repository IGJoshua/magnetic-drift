;;;; objects.lisp

(in-package #:magnetic-drift)

(defvar *camera* nil)

(defclass camera ()
  ((pos :initargs :pos :accessor pos
        :initform (v! 0 0))
   (zoom :initargs :zoom :accessor zoom
         :initform 1)))
