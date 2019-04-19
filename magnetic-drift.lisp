;;;; magnetic-drift.lisp

(in-package #:magnetic-drift)

(defvar *running* nil)

(defvar *quad-stream* nil)

(defun-g fullscreen-quad-vert ((uvs :vec2))
  (values (v! uvs 0 1)
          uvs))

(defun-g red-frag ((uvs :vec2))
  (v! 1 0 0 1))

(defpipeline-g red-fullscreen-quad ()
  :vertex (fullscreen-quad-vert :vec2)
  :fragment (red-frag :vec2))

(defun render ()
  (clear)
  (setf (cepl:viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (map-g #'red-fullscreen-quad *quad-stream*)
  (swap))

(defun update (dt)
  (declare (ignore dt))
  (step-host)
  (update-repl-link))

(defun init ()
  (slynk-mrepl::send-prompt (find (bt:current-thread) (slynk::channels)
                                  :key #'slynk::channel-thread))
  (setf *running* t)
  (unless *quad-stream*
    (setf *quad-stream* (nineveh:get-quad-stream-v2))))

(defun run-loop ()
  (init)
  (loop :while (and *running* (not (shutting-down-p)))
        :do (continuable
              (update 0)
              (render))))

(defun stop-loop ()
  (setf *running* nil))
