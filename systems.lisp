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

(defgeneric %run-system (system dt real-dt))
(defun run-system (system-sym dt real-dt)
  (%run-system (gethash system-sym *systems*) dt real-dt))

(defmethod %run-system ((system component-system) dt real-dt)
  (loop :for entity :in (cepl-utils:hash-keys *entities*)
        :do (funcall (system-fun system) entity dt real-dt)))

(defmethod %run-system ((system global-system) dt real-dt)
  (funcall (system-fun system) dt real-dt))

(defmacro define-component-system (name (entity-id dt real-dt)
                                   (&rest required-or-opt-components)
                                   (&rest exclude-components)
                                   &body body)
  (let ((required-components (loop :for spec :in required-or-opt-components
                                   :until (eq spec '&optional)
                                   :collecting spec))
        (optional-components (loop
                               :with opt-pos := (position '&optional required-or-opt-components)
                               :for spec :in (and opt-pos (nthcdr (1+ opt-pos) required-or-opt-components))
                               :collecting spec)))
    (alexandria:with-gensyms (entity-sym components-sym)
      (flet ((make-component-binding (binding)
               (etypecase binding
                 (list ; (name type)
                  (destructuring-bind (name type) binding
                    `(,name (gethash ',type ,components-sym))))
                 (symbol ; type only, no name
                  `(,(gensym) (gethash ',binding ,components-sym))))))
        `(setf (gethash ',name *systems*)
               (make-instance 'component-system
                              :system-fun (lambda (,entity-id ,dt ,real-dt)
                                            (declare (ignorable ,dt ,real-dt))
                                            (alexandria:when-let* ((,entity-sym (gethash ,entity-id *entities*))
                                                                   (,components-sym (slot-value ,entity-sym 'components)))
                                              (block ,name
                                                ;; Punk out early if excluded has any components
                                                ,@(when exclude-components
                                                    `((when (or ,@(mapcar (lambda (c) `(gethash ',c ,components-sym)) exclude-components))
                                                        (return-from ,name))))
                                                ;;Bind the required vars first
                                                (alexandria:when-let ,(mapcar #'make-component-binding required-components)
                                                  ;;Then the optional ones
                                                  (let ,(mapcar #'make-component-binding optional-components)
                                                    ,@body)))))
                              :required-components ',required-components
                              :exclude-components ',exclude-components))))))

(defmacro define-global-system (name (dt real-dt)
                                &body body)
  `(setf (gethash ',name *systems*)
         (make-instance 'global-system
                        :system-fun (lambda (,dt ,real-dt)
                                      (declare (ignorable ,dt ,real-dt))
                                      ,@body))))

(defun components-match-p (system entity-id)
  (let ((keyset (cepl-utils:hash-keys (slot-value (gethash entity-id *entities*)
                                                  'components))))
    (and (subsetp (required-components system) keyset)
         (not (intersection (exclude-components system) keyset)))))

(defun init-systems ()
  (unless *systems*
    (setf *systems* (make-hash-table))))
