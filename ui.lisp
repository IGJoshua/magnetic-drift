;; ui.lisp

(in-package :magnetic-drift)

(defclass ui-hitbox-component (component)
  ((size :initarg :size
         :initform (v! 1 1))))

(defmethod copy-component ((component ui-hitbox-component))
  (with-slots (size) component
    (make-instance 'ui-hitbox-component :size (v! (x size) (y size)))))

(defvar *mouse-over-entity* nil)
(defvar *mouse-over-local-pos* nil)

(define-component-system check-mouse-ui-hitbox (entity-id alpha)
    ((ui-pos ui-position-component)
     (ui-hitbox ui-hitbox-component)
     &optional
     (scale-comp scale-component))
    ()
  (with-slots (anchor pos) ui-pos
    (with-slots (size) ui-hitbox
      (let* ((viewport-size (cepl:viewport-resolution (cepl:current-viewport)))
             (top-left (v2-n:+ (v2-n:* (v2:+ anchor (v2! 1 1)) viewport-size #.(v2! 1/2 1/2))
                                      (v2! (x pos) (- (y pos)))))
             (bottom-right (v2-n:+ (v2:* size (if scale-comp (slot-value scale-comp 'scale) #.(v! 1 1)))
                                   top-left))
             (mouse (skitter:mouse))
             (mouse-pos (skitter:mouse-pos mouse)))
        ;; Assume AABB need to account for rotation component later
        (when (and (<= (x top-left) (x mouse-pos) (x bottom-right))
                   (<= (y top-left) (y mouse-pos) (y bottom-right)))
          (setf *mouse-over-entity* entity-id
                *mouse-over-local-pos* (v2:- mouse-pos top-left)))))))

(defvar *prev-mouse-over-entity* nil)
(defvar *prev-mouse-over-local-pos* nil)

(define-global-system check-mouse-over-entity (alpha)
  (declare (ignore alpha))
  (cond
    ((and *prev-mouse-over-entity* *mouse-over-entity*
          (= *prev-mouse-over-entity* *mouse-over-entity*))
     ;; Stayed the same
     (publish-event 'mouse-event (list 'mouse-stay *mouse-over-entity* *mouse-over-local-pos*)))
    ((and *prev-mouse-over-entity* *mouse-over-entity*)
     ;; Changed hands
     (publish-event 'mouse-event (list 'mouse-leave *prev-mouse-over-entity* *prev-mouse-over-local-pos*))
     (publish-event 'mouse-event (list 'mouse-enter *mouse-over-entity* *mouse-over-local-pos*)))
    (*prev-mouse-over-entity*
     ;; Lost it
     (publish-event 'mouse-event (list 'mouse-leave *prev-mouse-over-entity* *prev-mouse-over-local-pos*)))
    (*mouse-over-entity*
     ;; Got it
     (publish-event 'mouse-event (list 'mouse-stay *mouse-over-entity* *mouse-over-local-pos*)))
    (t
     ;; Hovering over nothing
     (publish-event 'mouse-event '(mouse-stay nil nil))))
  (setf *prev-mouse-over-entity* *mouse-over-entity*
        *prev-mouse-over-local-pos* *mouse-over-local-pos*))

(defvar *mouse-down* nil
  "True if the mouse left click was down last we checked.")

(define-global-system check-mouse-state (alpha)
  (declare (ignore alpha))
  ;; Check for mouse click or release
  (let ((mouse-down-p (skitter:mouse-down-p 1)))
    (cond
      ((and *mouse-down* mouse-down-p)
       ;; nothing tbd
       )
      (mouse-down-p
       (publish-event 'mouse-event (list 'mouse-click *mouse-over-entity* *mouse-over-local-pos*)))
      (*mouse-down*
       (publish-event 'mouse-event (list 'mouse-release *mouse-over-entity* *mouse-over-local-pos*)))
      (t
       ;; nothing tbd
       ))
    (setf *mouse-down* mouse-down-p)))

(defclass mouse-trigger-component (component)
  ((callback :initarg :callback
             :initform (lambda (event-type entity-id local-pos)
                         (declare (ignore event-type entity-id local-pos))))))

(defmethod copy-component ((component mouse-trigger-component))
  (with-slots (callback) component
    (make-instance 'mouse-trigger-component :callback callback)))

(define-event-handler dispatch-mouse-event (event) mouse-event
  (destructuring-bind (event-type entity-id local-pos)
      event
    (when entity-id
      (with-components ((trigger mouse-trigger-component))
          entity-id
        (when trigger
          (funcall (slot-value trigger 'callback) event-type entity-id local-pos))))))
