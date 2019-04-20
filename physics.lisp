;;;; physics.lisp

(in-package #:magnetic-drift)

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
