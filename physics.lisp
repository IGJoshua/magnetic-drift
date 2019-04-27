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

(defclass collider-component (component)
  ((radius :initarg :radius
           :initform 200)))

(define-component-system move-objects-with-velocity (entity-id dt)
    ((move velocity-component)
     (pos position-component))
    ()
  (v2-n:+ (slot-value pos 'pos)
          (v2:*s (slot-value move 'vel)
                 dt)))

(define-component-system rotate-objects-with-angular-velocity (entity-id dt)
    ((ang angular-velocity-component)
     (rot rotation-component))
    ()
  (incf (slot-value rot 'rot)
        (* (slot-value ang 'ang-vel)
           dt)))

(define-component-system apply-uniform-friction-to-objects (entity-id dt)
    ((move velocity-component)
     (friction uniform-friction-component))
    ()
  (v2-n:*s (slot-value move 'vel)
           (- 1f0 (* dt (slot-value friction 'coeff)))))

(defvar *collidable-entities* nil)
(defvar *mobile-collidable-entities* nil)

(define-component-system mark-entities-for-collision (entity-id dt)
    (collider-component position-component
     &optional
     (velocity-component velocity-component))
    ()
  (push entity-id *collidable-entities*)
  (when velocity-component
    (push entity-id *mobile-collidable-entities*)))

(defun overlapping-p (a b)
  (with-components ((a-collider collider-component)
                    (a-pos position-component))
      a
    (with-components ((b-collider collider-component)
                      (b-pos position-component))
        b
      (let ((diff (v2:- (slot-value a-pos 'pos)
                        (slot-value b-pos 'pos))))
        (when (< (v2:length diff) (+ (slot-value a-collider 'radius)
                         (slot-value b-collider 'radius)))
          t)))))

(define-global-system check-collisions (dt)
  (declare (ignore dt))
  (loop :for entity-id :in *mobile-collidable-entities*
        :do (loop :for entity-other :in *collidable-entities*
                  :when (not (eql entity-id entity-other))
                    :do (when (overlapping-p entity-id entity-other)
                          (publish-event 'collision-event (cons entity-id entity-other)))))
  (setf *collidable-entities* nil)
  (setf *mobile-collidable-entities* nil))

(define-event-handler dislodge-collided-entities (event)
    collision-event
  (destructuring-bind (a . b) event
    (with-components ((a-collider collider-component)
                      (a-pos position-component)
                      (a-vel velocity-component))
        a
      (with-components ((b-collider collider-component)
                        (b-pos position-component)
                        (b-vel velocity-component))
          b
        (let* ((diff (v2:- (slot-value a-pos 'pos)
                           (slot-value b-pos 'pos)))
               (penetration-depth (max (- (+ (slot-value a-collider 'radius)
                                             (slot-value b-collider 'radius))
                                          (v2:length diff))
                                       0f0)))
          (when (> penetration-depth 0)
            (v2-n:normalize diff)
            ;; b is static
            (if (and a-vel (not b-vel))
                (v2-n:+ (slot-value a-pos 'pos)
                        (v2:*s diff penetration-depth))
                ;; a is static
                (if (and b-vel (not a-vel))
                    (v2-n:- (slot-value b-pos 'pos)
                            (v2:*s diff penetration-depth))
                    ;; neither is static
                    (let* ((half-depth (/ penetration-depth 2))
                           (half-diff (v2:*s diff half-depth)))
                      (v2-n:+ (slot-value a-pos 'pos)
                              half-diff)
                      (v2-n:- (slot-value b-pos 'pos)
                              half-diff))))))))))

(define-prototype tire (&optional pos scale) ()
    ((position-component :pos (if pos pos (v! 0 0)))
     (scale-component :scale (if scale scale (v! 1 1)))
     (velocity-component)
     (uniform-friction-component)
     (texture-component :texture "./res/tires_white.png"
                        :scale (v! 0.125
                                   0.125))
     (collider-component :radius 7)))
