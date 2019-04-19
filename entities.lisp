;;;; objects.lisp

(in-package #:magnetic-drift)

(defvar *entities* nil)
(defvar *next-entity-id* 1)

(defclass entity ()
  ((components :initform (make-hash-table :test 'eq))))

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

(defun instantiate-prototype (prototype)
  (let ((entity-id (make-entity)))
    (funcall (slot-value (gethash prototype *prototypes*) 'instantiation-fun) entity-id)
    entity-id))

(defun init-entities ()
  (unless *entities*
    (setf *entities* (make-hash-table))
    (setf *next-entity-id* 1)
    (setf *camera* nil)
    (setf *car* nil)))
