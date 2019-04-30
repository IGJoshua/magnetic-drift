;;;; events.lisp

(in-package #:magnetic-drift)

(defvar *frame-events* nil)
(defvar *event-handlers* (make-hash-table :test 'eq))

(defmacro define-event-handler (name lambda-list event-type
                                &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (unless (gethash ',event-type *event-handlers*)
       (setf (gethash ',event-type *event-handlers*)
             (make-hash-table :test 'eq)))
     (setf (gethash ',name (gethash ',event-type *event-handlers*))
           #',name)
     ',name))

(defun publish-event (event-name event)
  (let ((event-cons (list (cons event-name event))))
    (if *frame-events*
        (setf (cdr (last *frame-events*))
              event-cons)
        (setf *frame-events* event-cons)))
  *frame-events*)

(defun process-events ()
  (loop :for pair := *frame-events* :then (cdr pair)
        :while pair
        :for ((event-name . event)) := pair
        :when (gethash event-name *event-handlers*)
        :do (maphash (lambda (handler-name handler-fun)
                       (declare (ignore handler-name))
                       (funcall handler-fun event))
                     (gethash event-name *event-handlers*)))
  (setf *frame-events* nil))
