;;;; magnetic-drift.lisp

(in-package #:magnetic-drift)

(defvar *running* nil)

(defun update (dt)
  (declare (ignore dt))
  (step-host)
  (update-repl-link))

(defun init ()
  (slynk-mrepl::send-prompt (find (bt:current-thread) (slynk::channels)
                                  :key #'slynk::channel-thread))
  (setf *running* t)
  (unless *quad-stream*
    (setf *quad-stream* (nineveh:get-quad-stream-v2)))
  (unless *textures*
    (setf *textures* (make-hash-table :test 'equal))))

(defun run-loop ()
  (init)
  (loop :while (and *running* (not (shutting-down-p)))
        :do (continuable
              (update 0)
              (render))))

(defun stop-loop ()
  (setf *running* nil))
