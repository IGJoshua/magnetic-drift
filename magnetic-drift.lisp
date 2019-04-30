;;;; magnetic-drift.lisp

(in-package #:magnetic-drift)

(defvar *running* nil)

(defvar *camera* nil)
(defvar *car* nil)

(defvar *physics-rate* 1f0)
(defparameter *scene-physics-systems* '(update-lap-counter
                                        reset-mouse-over-entity
                                        size-texture-hitbox
                                        size-text-hitbox
                                        check-mouse-ui-hitbox
                                        check-mouse-over-entity
                                        check-mouse-state
                                        spawn-pause-menu
                                        tick-down-wait-component
                                        move-time-to-text
                                        update-car-velocity
                                        update-car-angular-velocity
                                        move-objects-with-velocity
                                        move-player-camera
                                        rotate-objects-with-angular-velocity
                                        apply-directional-friction-to-objects
                                        apply-uniform-friction-to-objects
                                        mark-entities-for-collision
                                        check-line-overlap
                                        check-collisions))

(defun update (dt)
  (step-host)
  (update-repl-link)
  (handle-input)
  (loop :with frame-dt := (* dt *physics-rate*)
        :for system :in *scene-physics-systems*
        :do (run-system system frame-dt dt))
  (process-events))

(defun spawn-default-entities ()
  (instantiate-prototype 'camera)
  (instantiate-prototype 'car)
  (let* ((texs (make-hash-table))
         (tilemap-component (make-instance 'tilemap-component
                                           :tiles (apply
                                                   #'vector
                                                   (loop :for y :in (cepl-utils:range 10)
                                                         :collect (apply
                                                                   #'vector
                                                                   (loop :for x :in (cepl-utils:range 5)
                                                                         :collect #\#))))
                                           :textures texs)))
    (setf (gethash #\# texs) "./res/land_dirt05.png")
    (tilemap-recalc-positions tilemap-component)
    (add-component
     (add-component
      (make-entity)
      (make-instance 'position-component))
     tilemap-component)))

(defun init ()
  (init-entities)
  (init-systems)
  (init-renderer)
  (init-input)
  (when (zerop (length (cepl-utils:hash-keys *entities*)))
    (load-scene "./res/lvl/main-menu.lvl")))

(defun send-repl ()
  (alexandria:when-let* ((slynk-mrepl (find-package :slynk-mrepl))
                         (send-prompt (find-symbol "SEND-PROMPT" slynk-mrepl)))
    (funcall send-prompt (find (bt:current-thread) (slynk::channels)
                               :key #'slynk::channel-thread))))

(defun run-loop ()
  (init)
  (send-repl)
  (setf *running* t)
  (let* ((fps 120.0f0)
         (seconds-per-frame (/ fps))
         (seconds-per-internal-unit (float (/ internal-time-units-per-second)
                                           0f0))
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
                        (render (* (- current-time last-frame-seconds) fps) 0))))))

(defun stop-loop ()
  (setf *running* nil))
