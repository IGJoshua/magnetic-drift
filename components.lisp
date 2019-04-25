;;;; components.lisp

(in-package #:magnetic-drift)

(defclass component () ())

(defgeneric copy-component (component))

(defun add-component (entity-id component)
  (when entity-id
    (setf (gethash (type-of component) (slot-value (gethash entity-id *entities*) 'components))
          component)
    entity-id))

(defun get-component (entity-id component-name)
  (when entity-id
    (gethash component-name (slot-value (gethash entity-id *entities*) 'components))))

(defun remove-component (entity-id component-name)
  (when entity-id
    (remhash component-name (slot-value (gethash entity-id *entities*) 'components))))

(defmacro with-components (component-list entity-id &body body)
  (unless body
    (uiop:style-warn "No body included. Did you forget to bind an entity id?"))
  (let ((id (gensym)))
    `(let ((,id ,entity-id))
       (let (,@(mapcar (lambda (elt)
                         (destructuring-bind (sym comp-sym) elt
                           `(,sym (get-component ,id ',comp-sym))))
                       component-list))
         ,@body))))

(defclass position-component (component)
  ((pos :initarg :pos
        :initform (v! 0 0))))

(defmethod copy-component ((comp position-component))
  (with-slots (pos) comp
      (make-instance 'position-component
                     :pos (v! (x pos) (y comp)))))

(defclass rotation-component (component)
  ((rot :initarg :rot
        :initform 0)))

(defmethod copy-component ((comp rotation-component))
  (make-instance 'rotation-component
                 :rot (slot-value comp 'rot)))

(defclass scale-component (component)
  ((scale :initarg :scale
          :initform (v! 1 1))))

(defmethod copy-component ((comp scale-component))
  (with-slots (scale) comp
    (make-instance 'scale-component
                   :scale (v! (x scale) (y scale)))))

(define-prototype transform (&optional pos rot scale) ()
    ((position-component :pos (if pos pos (v! 0 0)))
     (rotation-component :rot (if rot rot 0))
     (scale-component :scale (if scale scale (v! 1 1)))))

(defclass camera-component (component)
  ((zoom :initarg :zoom :accessor zoom
         :initform 0.1)
   (active-p :initarg :active-p :accessor active-p
             :initform nil)))

(defmethod copy-component ((comp camera-component))
  (make-instance 'camera-component
                 :zoom (zoom comp)))

(define-prototype camera () ()
  ((camera-component :zoom 0.75 :active-p t)
   (position-component)))
