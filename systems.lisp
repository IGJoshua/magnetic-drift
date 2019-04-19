;;;; systems.lisp

(in-package #:magnetic-drift)

(defvar *systems* (make-hash-table :test 'eq))

(defclass system ()
  ((system-fun :initarg :system-fun :accessor system-fun
               :initform (error "Must include a system function"))))

(defclass component-system (system)
  ((required-components :initarg :required-components :accessor required-components
                        :initform (error "Must include some required components"))
   (exclude-components :initarg :exclude-components :accessor exclude-components
                       :initform nil)))

(defclass global-system (system)
  ())

(defgeneric %run-system (system dt))
(defun run-system (system-sym dt)
  (%run-system (gethash system-sym *systems*) dt))

(defmethod %run-system ((system component-system) dt)
  (loop :for entity :in (cepl-utils:hash-keys *entities*)
        :do (when (components-match-p system entity)
              (funcall (system-fun system) entity dt))))

(defmethod %run-system ((system global-system) dt)
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

(defun components-match-p (system entity-id)
  (let ((keyset (cepl-utils:hash-keys (slot-value (gethash entity-id *entities*)
                                                  'components))))
    (and (subsetp (required-components system) keyset)
         (not (intersection (exclude-components system) keyset)))))

(defun init-systems ()
  (unless *systems*
    (setf *systems* (make-hash-table))))
