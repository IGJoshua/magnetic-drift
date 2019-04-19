;;;; magnetic-drift.lisp

(in-package #:magnetic-drift)

(defvar *running* nil)
(defvar *camera* nil)

(defclass camera ()
  ((pos :initargs :pos :accessor pos
        :initform (v! 0 0))
   (zoom :initargs :zoom :accessor zoom
         :initform 1)))

(defun update (dt)
  (declare (ignore dt))
  (step-host)
  (update-repl-link))

(defun init ()
  (unless *quad-stream*
    (setf *quad-stream* (nineveh:get-quad-stream-v2)))
  (unless *textures*
    (setf *textures* (make-hash-table :test 'equal)))
  (unless *camera*
    (setf *camera* (make-instance 'camera))))

(defun run-loop ()
  (init)
  (slynk-mrepl::send-prompt (find (bt:current-thread) (slynk::channels)
                                  :key #'slynk::channel-thread))
  (setf *running* t)
  (let* ((fps 12.0)
         (seconds-per-frame (/ fps))
         (seconds-per-internal-unit (/ internal-time-units-per-second))
         (last-frame-seconds 0.0)
         (next-frame-seconds 0.0))
    (loop :while (and *running* (not (shutting-down-p)))
          :do (continuable
                (let ((current-time (* seconds-per-internal-unit (get-internal-real-time))))
                  (loop :while (> current-time
                                  next-frame-seconds)
                        :do (progn
                              (update seconds-per-frame)
                              (setf last-frame-seconds current-time)
                              (setf next-frame-seconds (+ current-time seconds-per-frame))))
                  (render (* (- current-time last-frame-seconds) fps)))))))

(defun stop-loop ()
  (setf *running* nil))
