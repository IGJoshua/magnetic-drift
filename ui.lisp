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

(define-global-system reset-mouse-over-entity (dt real-dt)
  (setf *mouse-over-entity* nil
        *mouse-over-local-pos* nil))

(define-component-system check-mouse-ui-hitbox (entity-id dt real-dt)
    ((ui-pos ui-position-component)
     (ui-hitbox ui-hitbox-component)
     &optional
     (scale-comp scale-component))
    ()
  (with-slots (anchor pos) ui-pos
    (with-slots (size) ui-hitbox
      (let* ((viewport-size (cepl:viewport-resolution (cepl:current-viewport)))
             (scale-vec (if scale-comp (slot-value scale-comp 'scale) #.(v! 1 1)))
             (scaled-size (v2:* scale-vec size))
             (top-left (v2-n:+ (v2-n:* (v2! (* (+ (x anchor) 1) 0.5f0)
                                            (- 1 (* (+ (y anchor) 1) 0.5f0)))
                                       viewport-size)
                               (v2! (x pos) (- (y pos)))
                               (v2! (* (x scaled-size) -1/2) (* (y scaled-size) -1/2))))
             (bottom-right (v2:+ scaled-size top-left))
             (mouse (skitter:mouse))
             (mouse-pos (skitter:mouse-pos mouse)))
        ;; Assume AABB need to account for rotation component later
        (when (and (<= (x top-left) (x mouse-pos) (x bottom-right))
                   (<= (y top-left) (y mouse-pos) (y bottom-right)))
          (setf *mouse-over-entity* entity-id
                *mouse-over-local-pos* (v2:- mouse-pos top-left)))))))

(defvar *prev-mouse-over-entity* nil)
(defvar *prev-mouse-over-local-pos* nil)

(define-global-system check-mouse-over-entity (dt real-dt)
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
     (publish-event 'mouse-event (list 'mouse-enter *mouse-over-entity* *mouse-over-local-pos*)))
    (t
     ;; Hovering over nothing
     (publish-event 'mouse-event '(mouse-stay nil nil))))
  (setf *prev-mouse-over-entity* *mouse-over-entity*
        *prev-mouse-over-local-pos* *mouse-over-local-pos*))

(defvar *mouse-down* nil
  "True if the mouse left click was down last we checked.")

(define-global-system check-mouse-state (dt real-dt)
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

(define-component-system size-texture-hitbox (entity-id dt real-dt)
    ((tex-comp texture-component)
     (hitbox-comp ui-hitbox-component))
    ()
  (let* ((sam (texture (slot-value tex-comp 'texture)))
         (tex (slot-value sam 'texture)))
    (destructuring-bind (width height) (texture-base-dimensions tex)
      (setf (slot-value hitbox-comp 'size) (v2! width height)))))

(define-component-system size-text-hitbox (entity-id dt real-dt)
    ((text-comp text-component)
     (hitbox-comp ui-hitbox-component))
    ()
  (multiple-value-bind (width height)
      (sdl2-ttf:size-text (ttf-font-from-text-comp text-comp) (slot-value text-comp 'text))
    (setf (slot-value hitbox-comp 'size) (v2! width height))))

(defun do-button-callback (callback on-pressed event-type entity-id local-pos)
  (funcall callback event-type entity-id local-pos)
    (when (eq event-type 'mouse-release)
      (funcall on-pressed entity-id local-pos)))

(defun make-button-callback (callback on-pressed)
  (lambda (event-type entity-id local-pos)
    (do-button-callback callback on-pressed event-type entity-id local-pos)))

(define-prototype texture-button (texture &key callback on-pressed anchor pos) ()
    ((ui-position-component :anchor (or anchor (v2! 0 0)) :pos (or pos (v2! 0 0)))
     (ui-hitbox-component)
     (mouse-trigger-component :callback (make-button-callback (or callback (lambda (x y z) (declare (ignore x y z))))
                                                              (or on-pressed (lambda (x y) (declare (ignore x y))))))
     (texture-component :texture texture)))
()
(defclass toggle-state ()
  ((callback :initarg :callback)
   (normal :initarg :normal)
   (hover :initarg :hover)
   (pressed :initarg :pressed)
   (pressed-p :initform nil)))

(defun do-texture-toggle (toggle-state event-type entity-id local-pos)
  (with-slots (callback normal hover pressed pressed-p) toggle-state
    (with-components ((tex-comp texture-component)) entity-id
      (when tex-comp
        (with-slots (texture) tex-comp
          (setf pressed-p
                (case event-type
                  ((mouse-click) t)
                  ((mouse-release mouse-leave) nil)
                  (t pressed-p)))
          (setf (slot-value tex-comp 'texture)
                (if pressed-p
                    pressed
                    (case event-type
                      (mouse-enter hover)
                      (mouse-leave normal)
                      (mouse-stay hover)
                      (t normal)))))))
    (funcall callback event-type entity-id local-pos)))

(defun make-toggle-callback (orig-callback normal hover pressed)
  (let ((state (make-instance 'toggle-state
                              :callback (or orig-callback (lambda (x y z) (declare (ignore x y z))))
                              :normal normal
                              :hover (or hover normal)
                              :pressed (or pressed hover normal))))
    (lambda (event-type entity-id local-pos)
      (do-texture-toggle state event-type entity-id local-pos))))

(define-prototype texture-toggle-button (normal &key callback on-pressed anchor pos hover pressed)
    ((texture-button normal
                     :anchor anchor :pos pos
                     :callback (make-toggle-callback callback normal hover pressed)
                     :on-pressed on-pressed))
    ())
