;;;; magnetic-drift.lisp

(in-package #:magnetic-drift)

(defvar *running* nil)

(defvar *camera* nil)
(defvar *car* nil)

(defparameter *scene-physics-systems* '(global-move-camera
                                        update-car-velocity
                                        update-car-angular-velocity
                                        move-objects-with-velocity
                                        rotate-objects-with-angular-velocity
                                        apply-directional-friction-to-objects
                                        apply-uniform-friction-to-objects
                                        mark-entities-for-collision
                                        check-collisions))

(defun update (dt)
  (step-host)
  (update-repl-link)
  (handle-input)
  (loop :for system :in *scene-physics-systems*
        :do (run-system system dt))
  (process-events))

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
  (let* ((fps 120.0f0)
         (seconds-per-frame (/ fps))
         (seconds-per-internal-unit (/ internal-time-units-per-second))
         (last-frame-seconds (* seconds-per-internal-unit (get-internal-real-time)))
         (next-frame-seconds (+ last-frame-seconds seconds-per-frame)))
    (loop :while (and *running* (not (shutting-down-p)))
          :do
             (continuable
               (loop :with current-time := (* seconds-per-internal-unit (get-internal-real-time))
                     :while (> current-time
                               next-frame-seconds)
                     :do
                        (update seconds-per-frame)
                        (setf last-frame-seconds next-frame-seconds
                              next-frame-seconds (+ next-frame-seconds seconds-per-frame))
                     :finally
                        (render (* (- current-time last-frame-seconds) fps)))))))

(defun stop-loop ()
  (setf *running* nil))
