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

(defparameter *collidable-entities* nil)

(define-component-system mark-entities-for-collision (entity-id dt)
    (collider-component position-component) ()
  (declare (ignore dt))
  (push entity-id *collidable-entities*))

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
  (loop :for entity-id :in *collidable-entities*
        :do (loop :for entity-other :in *collidable-entities*
                  :when (not (eql entity-id entity-other))
                    :do (when (overlapping-p entity-id entity-other)
                          (publish-event 'collision-event (cons entity-id entity-other)))))
  (setf *collidable-entities* nil))

(define-event-handler dislodge-collided-entities (event)
    collision-event
  (destructuring-bind (a . b) event
    (with-components ((a-collider collider-component)
                      (a-pos position-component))
        a
      (with-components ((b-collider collider-component)
                        (b-pos position-component))
          b
        (let* ((diff (v2:- (slot-value a-pos 'pos)
                           (slot-value b-pos 'pos)))
               (penetration-depth (/ (max (- (+ (slot-value a-collider 'radius)
                                                (slot-value b-collider 'radius))
                                             (v2:length diff))
                                          0f0)
                                     2f0)))
          (v2-n:normalize diff)
          (v2-n:*s diff
                 penetration-depth)
          (v2-n:+ (slot-value a-pos 'pos)
                  diff)
          (v2-n:- (slot-value b-pos 'pos)
                  diff))))))

(define-prototype tire (&optional pos scale) ()
    ((position-component :pos (if pos pos (v! 0 0)))
     (scale-component :scale (if scale scale (v! 1 1)))
     (velocity-component)
     (uniform-friction-component)
     (texture-component :texture "./res/tires_white.png")
     (collider-component :radius 25)))
