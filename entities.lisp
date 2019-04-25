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

(defun entity-exists-p (entity-id)
  (gethash entity-id *entities*))

(defun destroy-entity (entity-id)
  (remhash entity-id *entities*))

(defvar *prototypes* (make-hash-table :test 'eq))
(defclass prototype ()
  ((instantiation-fun :initarg :instantiation-fun)))

(defmacro define-prototype (name arguments superprototypes
                            components
                            &optional entity-id-sym
                            &body body)
  (let ((entity-id (gensym))
        (superprototype (gensym)))
    `(setf (gethash ',name *prototypes*)
       (make-instance
        'prototype
        :instantiation-fun
        (lambda (,entity-id ,@arguments)
          ,@(loop :for (prototype . args) :in superprototypes
                  :collect `(let ((,superprototype (gethash ',prototype *prototypes*)))
                              (funcall (slot-value ,superprototype 'instantiation-fun)
                                       ,entity-id
                                       ,@args)))
          ,@(loop :for component
                    :in (mapcar (lambda (comp)
                                  `(make-instance ',(car comp) ,@(cdr comp)))
                                components)
                  :collect `(add-component ,entity-id ,component))
          ,(when entity-id-sym
             `(let ((,entity-id-sym ,entity-id))
                ,@body))
          ,entity-id)))))

(defun instantiate-prototype (prototype &rest args)
  (let ((entity-id (make-entity)))
    (apply (slot-value (gethash prototype *prototypes*) 'instantiation-fun) entity-id args)
    entity-id))

(defun init-entities ()
  (unless *entities*
    (setf *entities* (make-hash-table))
    (setf *next-entity-id* 1)
    (setf *camera* nil)
    (setf *car* nil)))
