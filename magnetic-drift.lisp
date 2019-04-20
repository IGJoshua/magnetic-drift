;;;; magnetic-drift.lisp

(in-package #:magnetic-drift)

(defvar *running* nil)

(defvar *camera* nil)
(defvar *car* nil)

(defparameter *scene-physics-systems* '(global-move-camera move-cars))

(define-global-system global-move-camera (dt)
  (v2-n:+ (slot-value (get-component *camera* 'position-component) 'pos)
          (v2:*s (cam-dir *input*)
                 (* 10000.0 dt
                    (if (brake *input*)
                        0.1 1.0)))))

(defclass player-movement-component (component)
  ())

(define-component-system move-cars (entity-id dt)
    (position-component player-movement-component) ()
  (v2-n:+ (slot-value (get-component entity-id 'position-component) 'pos)
          (v2:*s (dir *input*)
                 (* 1000.0 dt))))

(define-prototype car ()
  ((position-component)
   (player-movement-component)
   (rotation-component)))

(defun update (dt)
  (step-host)
  (update-repl-link)
  (handle-input)
  (loop :for system :in *scene-physics-systems*
        :do (run-system system dt)))

(defun spawn-default-entities ()
  (instantiate-prototype 'camera)
  (instantiate-prototype 'car))

(defun init ()
  (init-entities)
  (when (zerop (length (cepl-utils:hash-keys *entities*)))
    (spawn-default-entities))
  (init-systems)
  (init-renderer)
  (init-input))

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
