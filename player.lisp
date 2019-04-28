;;;; player.lisp

(in-package #:magnetic-drift)

(define-global-system global-move-camera (dt)
  (when (entity-exists-p *camera*)
    (with-components ((pos-comp position-component))
        *camera*
      (v2-n:+ (slot-value pos-comp 'pos)
              (v2:*s (cam-dir *input*)
                     (* 1000.0 dt
                        (if (brake *input*)
                            0.1 1.0)))))))

(defclass player-input-component (component)
  ((accell :initarg :accell
           :initform 600f0)
   (speed :initarg :speed
          :initform 10000f0)
   (turning-speed :initarg :turning-speed
                  :initform pi)
   (brake-strength :initarg :brake-strength
                   :initform 3f0)))

(defclass directional-friction-component (component)
  ((low-coeff :initarg :low-coeff
              :initform 0.1)
   (high-coeff :initarg :high-coeff
               :initform 0.5)))

(define-component-system update-car-velocity (entity-id dt)
    ((move velocity-component)
     (rot rotation-component)
     (input-comp player-input-component))
    ()
  (with-slots (rot) rot
    (with-slots (vel) move
      (let* ((input (y (dir *input*)))
             (facing-dir (v! (cos rot)
                             (sin rot)))
             (forward-speed (v2:dot facing-dir vel))
             (to-add (v2-n:-
                      (v2:*s facing-dir
                             (* (slot-value input-comp 'accell)
                                dt
                                input))
                      (v2:*s facing-dir
                             (if (brake *input*)
                                 (* forward-speed
                                    (slot-value input-comp 'brake-strength)
                                    dt)
                                 0f0)))))
        (unless (and (> (v2:dot to-add vel) 0)
                     (>= (v2:length vel) (slot-value input-comp 'speed)))
          (v2-n:+ vel
                  to-add))))))

(define-component-system update-car-angular-velocity (entity-id dt)
    ((ang-vel angular-velocity-component)
     (input player-input-component)
     (vel velocity-component)
     (rot rotation-component))
    ()
  (with-slots (ang-vel) ang-vel
    (with-slots (vel) vel
      (with-slots (rot) rot
        (let ((dir (v! (cos rot)
                       (sin rot))))
          (setf ang-vel (* (- (x (dir *input*)))
                           (slot-value input 'turning-speed)
                           (/ (min (v2:length-squared vel) 10000f0)
                              10000f0)
                           (v2:dot (v2:normalize vel)
                                   dir))))))))

(define-component-system apply-directional-friction-to-objects (entity-id dt)
    ((vel velocity-component)
     (friction directional-friction-component)
     (rot rotation-component))
    ()
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
                   (v2-n:*s right right-speed))))))))

(define-prototype car (&optional pos rot scale) ((transform pos rot scale))
    ((player-input-component)
     (velocity-component)
     (angular-velocity-component)
     (collider-component :radius 10)
     (directional-friction-component :low-coeff 0.9
                                     :high-coeff 7)
     (texture-component :texture "./res/car_blue_1.png"
                        :rotation (- (/ pi 2))
                        :scale (v! 0.125f0
                                   0.125f0)
                        :z-level 1)))
