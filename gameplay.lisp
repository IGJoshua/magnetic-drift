;;;; gameplay.lisp

(in-package #:magnetic-drift)

(defclass checkpoint-component (component)
  ((index
    :initarg :index
    :initform 0
    :documentation "The index number of this checkpoint.")
   (finish-line-p
    :initarg :finish-line-p
    :initform nil
    :documentation "Does this mark the finish line?")))

(defmethod copy-component ((component checkpoint-component))
  (with-slots (index finish-line-p) component
    (make-instance 'checkpoint-component :index index :finish-line-p finish-line-p)))

(define-prototype checkpoint (&key pos rot length index finish-line-p) ()
    ((position-component :pos (or pos (v! 0 0)))
     (rotation-component :rot (or rot 0))
     (line-trigger-component :length (or length 32))
     (checkpoint-component :index (or index 0) :finish-line-p finish-line-p)))

(defclass lap-counter-component (component)
  ((lap-count
    :initarg :lap-count
    :initform 0
    :documentation "The number of laps completed")
   (next-checkpoint
    :initarg :next
    :initform 0
    :documentation "The index of the next checkpoint to be hit")))

(defmethod copy-component ((component lap-counter-component))
  (with-slots (lap-count next-checkpoint) component
    (make-instance 'lap-counter-component :lap-count lap-count :next-checkpoint next-checkpoint)))

(define-event-handler check-checkpoint-reached (event) trigger-hit
  (destructuring-bind (entity collider normal depth) event
    (declare (ignore normal depth))
    (with-components ((checkpoint checkpoint-component))
        collider
      (when collider
        (with-components ((lap-counter lap-counter-component))
            entity
          (when (and lap-counter (= (slot-value checkpoint 'index)
                                    (slot-value lap-counter 'next-checkpoint)))
            (cond
              ((slot-value checkpoint 'finish-line-p)
               (incf (slot-value lap-counter 'lap-count))
               (setf (slot-value lap-counter 'next-checkpoint) 0)
               (publish-event 'checkpoint-reached (list entity collider))
               (publish-event 'lap-completed (list entity collider)))
              (t
               (incf (slot-value lap-counter 'next-checkpoint))
               (publish-event 'checkpoint-reached (list entity collider))))))))))

(defvar *lap-counter-text* nil)
(defvar *checkpoint-text* nil)

(define-component-system update-lap-counter (entity-id dt)
    ((lap-counter lap-counter-component))
    ()
  (with-components ((text-comp text-component))
      *lap-counter-text*
    (when text-comp
      (setf (slot-value text-comp 'text) (format nil "~A" (slot-value lap-counter 'lap-count)))))
  (with-components ((text-comp text-component))
      *checkpoint-text*
    (when text-comp
      (setf (slot-value text-comp 'text) (format nil "~A" (slot-value lap-counter 'next-checkpoint))))))

(defvar *laps-in-level* nil)

(define-event-handler check-level-completed (event) lap-completed
  (destructuring-bind (entity checkpoint) event
    (declare (ignore checkpoint))
    (with-components ((lap-counter lap-counter-component)
                      (player-input player-input-component))
        entity
      (when (and player-input
                 lap-counter
                 *laps-in-level*
                 (>= (slot-value lap-counter 'lap-count) *laps-in-level*))
        (publish-event 'level-complete entity)))))

(defvar *next-level* nil)

(define-event-handler load-level-on-completed (event) level-complete
  (declare (ignore event))
  (when *next-level*
    (let ((lvl *next-level*))
      (setf *next-level* nil)
      (load-scene lvl))))
