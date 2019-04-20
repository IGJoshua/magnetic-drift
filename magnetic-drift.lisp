;;;; magnetic-drift.lisp

(in-package #:magnetic-drift)

(defvar *running* nil)

(defvar *camera* nil)
(defvar *car* nil)

(defparameter *scene-physics-systems* '(global-move-camera update-car-velocity
                                        update-car-angular-velocity
                                        move-objects-with-velocity
                                        rotate-objects-with-angular-velocity
                                        apply-directional-friction-to-objects
                                        apply-uniform-friction-to-objects))

(define-global-system global-move-camera (dt)
  (when (entity-exists-p *camera*)
    (with-components ((pos-comp position-component))
        *camera*
      (v2-n:+ (slot-value pos-comp 'pos)
              (v2:*s (cam-dir *input*)
                     (* 10000.0 dt
                        (if (brake *input*)
                            0.1 1.0)))))))

(defclass velocity-component (component)
  ((vel :initarg :vel
        :initform (v! 0f0 0f0))))

(defclass angular-velocity-component (component)
  ((ang-vel :initarg :ang-vel
            :initform 0f0)))

(defclass player-input-component (component)
  ((accell :initarg :accell
           :initform 5000f0)
   (speed :initarg :speed
          :initform 10000f0)
   (turning-speed :initarg :turning-speed
                  :initform 1f0)))

(defclass uniform-friction-component (component)
  ((coeff :initarg :coeff
          :initform 0.5)))

(defclass directional-friction-component (component)
  ((low-coeff :initarg :low-coeff
              :initform 0.1)
   (high-coeff :initarg :high-coeff
               :initform 0.5)))

(define-component-system update-car-velocity (entity-id dt)
    (velocity-component player-input-component rotation-component) ()
  (with-components ((move velocity-component)
                    (rot rotation-component)
                    (input-comp player-input-component))
      entity-id
    (with-slots (rot) rot
      (let* ((input (y (dir *input*)))
             (facing-x (cos rot))
             (facing-y (sin rot))
             (to-add (v2-n:*s (v! facing-x facing-y)
                              (* (slot-value input-comp 'accell)
                                 dt
                                 input))))
        (with-slots (vel) move
          (unless (and (> (v2:dot to-add vel) 0)
                       (>= (v2:length vel) (slot-value input-comp 'speed)))
            (v2-n:+ vel
                    to-add)))))))

(define-component-system update-car-angular-velocity (entity-id dt)
    (angular-velocity-component player-input-component) ()
  (declare (ignore dt))
  (with-components ((vel angular-velocity-component)
                    (input player-input-component))
      entity-id
    (with-slots (ang-vel) vel
      (setf ang-vel (* (- (x (dir *input*)))
                       (slot-value input 'turning-speed))))))

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

(define-component-system apply-directional-friction-to-objects (entity-id dt)
    (velocity-component directional-friction-component rotation-component) ()
  (with-components ((vel velocity-component)
                    (friction directional-friction-component)
                    (rot rotation-component))
      entity-id
    (with-slots (vel) vel
      (with-slots (rot) rot
        (with-slots (high-coeff low-coeff) friction
          (v2-n:-
           vel
           (let* ((forward (v! (cos rot)
                               (sin rot)))
                  (right (- rot (/ pi 2)))
                  (right (v! (cos right)
                             (sin right)))
                  (forward-speed (v2:dot vel forward))
                  (right-speed (v2:dot vel right))
                  (forward-speed (* forward-speed low-coeff dt))
                  (right-speed (* right-speed
                                  (- high-coeff
                                     (* (/ high-coeff 1.4)
                                        (v2:absolute-dot right
                                                         (v2:normalize vel))))
                                  dt)))
             (v2-n:+ (v2-n:*s forward forward-speed)
                     (v2-n:*s right right-speed)))))))))

(define-prototype car (&optional pos rot scale) ((transform pos rot scale))
    ((player-input-component)
     (velocity-component)
     (angular-velocity-component)
     (directional-friction-component)
     (texture-component :texture "./res/car_blue_1.png"
                        :rotation (/ pi 2))))

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
