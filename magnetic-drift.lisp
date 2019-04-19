;;;; magnetic-drift.lisp

(in-package #:magnetic-drift)

(defvar *camera* nil)
(defvar *running* nil)
(defparameter *scene-systems* '(global-move-camera move-cars))

(defun update (dt)
  (step-host)
  (update-repl-link)
  (handle-input)
  (loop :for system :in *scene-systems*
        :do (run-system (gethash system *systems*) dt)))

(defun init ()
  (init-systems)
  (init-renderer)
  (unless *camera*
    (setf *camera* (make-camera)))
  (unless *input*
    (setf *input* (make-instance 'input))))

(defun run-loop ()
  (init)
  (slynk-mrepl::send-prompt (find (bt:current-thread) (slynk::channels)
                                  :key #'slynk::channel-thread))
  (setf *running* t)
  (let* ((fps 120.0)
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
