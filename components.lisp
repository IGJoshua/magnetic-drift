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

(defclass position-component (component)
  ((pos :initarg :pos
        :initform (v! 0 0))))

(defmethod copy-component ((comp position-component))
  (with-slots (pos) comp
      (make-instance 'position-component
                     :pos (v! (x pos) (y comp)))))

(defmacro with-components (component-list entity-id &body body)
  (let ((id (gensym)))
    `(let ((,id ,entity-id))
       (let (,@(mapcar (lambda (elt)
                         (destructuring-bind (sym comp-sym) elt
                             `(,sym (get-component ,id ',comp-sym))))
                       component-list))
         ,@body))))

(defclass camera-component (component)
  ((zoom :initarg :zoom :accessor zoom
         :initform 0.1)
   (active-p :initarg :active-p :accessor active-p
             :initform nil)))

(defmethod copy-component ((comp camera-component))
  (make-instance 'camera-component
                 :zoom (zoom comp)))

(define-prototype camera ()
  ((camera-component :zoom 0.1 :active-p t)
   (position-component)))
