;;;; magnetic-drift.lisp

(in-package #:magnetic-drift)

(defvar *running* nil)

(defvar *quad-stream* nil)
(defvar *textures* nil)

(defun texture (filename)
  (alexandria:if-let ((tex (gethash filename *textures*)))
    tex
    (setf (gethash filename *textures*) (dirt:load-image-to-texture filename))))

(defun sampler-for-texture (filename)
  (sample (texture filename)))

(defun-g fullscreen-quad-vert ((positions :vec2))
  (values (v! positions 0 1)
          (/ (+ (v! 1 1) positions) 2)))

(defun-g red-frag ((uvs :vec2))
  (v! 1 0 0 1))

(defpipeline-g red-fullscreen-quad ()
  :vertex (fullscreen-quad-vert :vec2)
  :fragment (red-frag :vec2))

(defun-g texture-frag ((uvs :vec2)
                       &uniform
                       (sam :sampler-2d))
  (texture sam uvs))

(defpipeline-g textured-fullscreen-quad ()
  :vertex (fullscreen-quad-vert :vec2)
  :fragment (texture-frag :vec2))

(defun render ()
  (clear)
  (setf (cepl:viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (map-g #'textured-fullscreen-quad *quad-stream*
         :sam (sampler-for-texture "./res/car_blue_1.png"))
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
