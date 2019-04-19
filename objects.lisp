;;;; objects.lisp

(in-package #:magnetic-drift)

(defvar *systems* (make-hash-table :test 'eq))
(defvar *entities* nil)
(defvar *next-entity-id* 1)

(defclass entity ()
  ((components :initform (make-hash-table :test 'eq))))

(defclass component () ())

(defgeneric copy-component (component))

(defun make-entity ()
  (setf (gethash *next-entity-id* *entities*)
        (make-instance 'entity))
  (let ((val *next-entity-id*))
    (incf *next-entity-id*)
    val))

(defvar *prototypes* (make-hash-table :test 'eq))
(defclass prototype ()
  ((instantiation-fun :initarg :instantiation-fun)))

(defmacro define-prototype (name superprototypes components)
  (let ((entity-id (gensym))
        (prototype (gensym))
        (superprototype (gensym)))
    `(setf (gethash ',name *prototypes*)
       (make-instance 'prototype
                      :instantiation-fun (lambda (,entity-id)
                                           (loop :for ,prototype :in ,superprototypes
                                                 :do (let ((,superprototype (gethash ,prototype *prototypes*)))
                                                       (funcall (slot-value ,superprototype 'instantiation-fun)
                                                                ,entity-id)))
                                           ,@(loop :for component
                                                     :in (mapcar (lambda (comp)
                                                                   `(make-instance ',(car comp) ,@(cdr comp)))
                                                                 components)
                                                   :collect `(add-component ,entity-id ,component))
                                           ,entity-id)))))

(define-prototype camera ()
  ((camera-component :zoom 0.1)
   (position-component)))

(defun instantiate-prototype (prototype)
  (let ((entity-id (make-entity)))
    (funcall (slot-value (gethash prototype *prototypes*) 'instantiation-fun) entity-id)
    entity-id))

(defun add-component (entity-id component)
  (setf (gethash (type-of component) (slot-value (gethash entity-id *entities*) 'components))
        component)
  entity-id)

(defun get-component (entity-id component-name)
  (gethash component-name (slot-value (gethash entity-id *entities*) 'components)))

(defclass position-component (component)
  ((pos :initarg :pos :accessor pos
        :initform (v! 0 0))))

(defmethod copy-component ((comp position-component))
  (make-instance 'position-component
                 :pos (v! (x (pos comp)) (y (pos comp)))))

(defclass camera-component (component)
  ((zoom :initarg :zoom :accessor zoom
         :initform 0.1)))

(defmethod copy-component ((comp camera-component))
  (make-instance 'camera-component
                 :zoom (zoom comp)))

(defun make-camera ()
  (instantiate-prototype 'camera))

(defgeneric run-system (system dt))

(defclass system ()
  ((system-fun :initarg :system-fun :accessor system-fun
               :initform (error "Must include a system function"))))

(defclass component-system (system)
  ((required-components :initarg :required-components :accessor required-components
                        :initform (error "Must include some required components"))
   (exclude-components :initarg :exclude-components :accessor exclude-components
                       :initform nil)))

(defun components-match-p (system entity-id)
  (let ((keyset (cepl-utils:hash-keys (slot-value (gethash entity-id *entities*)
                                                  'components))))
    (and (subsetp (required-components system) keyset)
         (not (intersection (exclude-components system) keyset)))))

(defmethod run-system ((system component-system) dt)
  (loop :for entity :in (cepl-utils:hash-keys *entities*)
        :do (when (components-match-p system entity)
              (funcall (system-fun system) entity dt))))

(defclass global-system (system)
  ())

(defmethod run-system ((system global-system) dt)
  (funcall (system-fun system) dt))

(defmacro define-component-system (name lambda-list
                                   required-components exclude-components
                                   &body body)
  `(setf (gethash ',name *systems*)
         (make-instance 'component-system
                        :system-fun (lambda ,lambda-list
                                      ,@body)
                        :required-components ',required-components
                        :exclude-components ',exclude-components)))

(defmacro define-global-system (name lambda-list
                                &body body)
  `(setf (gethash ',name *systems*)
         (make-instance 'global-system
                        :system-fun (lambda ,lambda-list
                                      ,@body))))

(defun init-systems ()
  (unless *entities*
    (setf *entities* (make-hash-table))
    (setf *next-entity-id* 1))
  (unless *systems*
    (setf *systems* (make-hash-table))))
