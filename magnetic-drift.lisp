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
                                        apply-uniform-friction-to-objects))

(defclass velocity-component (component)
  ((vel :initarg :vel
        :initform (v! 0f0 0f0))))

(defclass angular-velocity-component (component)
  ((ang-vel :initarg :ang-vel
            :initform 0f0)))

(defclass uniform-friction-component (component)
  ((coeff :initarg :coeff
          :initform 0.5)))

(define-component-system move-objects-with-velocity (entity-id dt)
    (velocity-component position-component) ()
  (with-components ((move velocity-component)
                    (pos position-component))
      entity-id
    (v2-n:+ (slot-value pos 'pos)
            (v2:*s (slot-value move 'vel)
                   dt))))

(define-component-system rotate-objects-with-angular-velocity (entity-id dt)
    (angular-velocity-component rotation-component) ()
  (with-components ((ang angular-velocity-component)
                    (rot rotation-component))
      entity-id
    (incf (slot-value rot 'rot)
          (* (slot-value ang 'ang-vel)
             dt))))

(define-component-system apply-uniform-friction-to-objects (entity-id dt)
    (velocity-component uniform-friction-component) ()
  (with-components ((move velocity-component)
                    (friction uniform-friction-component))
      entity-id
    (v2-n:*s (slot-value move 'vel)
            (- 1f0 (* dt (slot-value friction 'coeff))))))

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
