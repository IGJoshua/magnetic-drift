;;;; pause-menu.lisp

(in-package #:magnetic-drift)

(defclass wait-component (component)
  ((time :initarg :time
         :initform 3)))

(define-component-system tick-down-wait-component (entity-id dt real-dt)
    ((wait wait-component)) ()
  (with-slots (time) wait
    (if (> time 0f0)
        (progn
          (decf time real-dt)
          (setf *physics-rate* 0f0))
        (progn
          (destroy-entity entity-id)
          (setf *physics-rate* 1f0)))))

(define-component-system move-time-to-text (entity-id dt real-dt)
    ((text text-component)
     (wait wait-component))
    ()
  (setf (slot-value text 'text) (format nil "~a" (ceiling (slot-value wait 'time)))))

(define-prototype resume-clock (&optional time)
    ()
    ((wait-component :time (or time 5f0))
     (text-component :font "./res/kenney-fonts/Kenney Future.ttf"
                     :point-size 72)
     (ui-position-component)))

(defclass pause-menu-component (component)
  (resume-button
   main-menu-button))

(defun delete-pause-menu (pause-menu-id)
  (with-components ((menu pause-menu-component))
      pause-menu-id
    (with-slots (resume-button main-menu-button) menu
      (destroy-entity resume-button)
      (destroy-entity main-menu-button)))
  (destroy-entity pause-menu-id))

(define-prototype pause-menu (spawner)
    ()
    ((pause-menu-component))
    pause-menu
  (with-components ((menu pause-menu-component))
      pause-menu
    (setf (slot-value menu 'resume-button)
          (instantiate-prototype 'texture-toggle-button "./res/pause-menu/resume.png"
                                 :hover "./res/pause-menu/resume-hover.png"
                                 :pressed "./res/pause-menu/resume-press.png"
                                 :anchor (v! 0 0)
                                 :pos (v! 0 30)
                                 :on-pressed
                                 (lambda (id pos)
                                   (declare (ignore id pos))
                                   (with-components ((spawner-comp pause-menu-spawner-component))
                                       spawner
                                     (setf (slot-value spawner-comp 'spawned) nil))
                                   (delete-pause-menu pause-menu)
                                   (instantiate-prototype 'resume-clock 3f0))))
    (setf (slot-value menu 'main-menu-button)
          (instantiate-prototype 'texture-toggle-button "./res/pause-menu/main-menu.png"
                                 :hover "./res/pause-menu/main-menu-hover.png"
                                 :pressed "./res/pause-menu/main-menu-press.png"
                                 :anchor (v! 0 0)
                                 :pos (v! 0 -30)
                                 :on-pressed
                                 (lambda (id pos)
                                   (declare (ignore id pos))
                                   (load-scene "./res/lvl/main-menu.lvl"))))))

(defclass pause-menu-spawner-component (component)
  ((spawned :initform nil)))

(define-component-system spawn-pause-menu (entity-id dt real-dt)
    ((spawner pause-menu-spawner-component)) ()
  (when (and (pause *input*)
             (not (slot-value spawner 'spawned))
             (> dt 0))
    (setf (slot-value spawner 'spawned) t)
    (setf *physics-rate* 0f0)
    (instantiate-prototype 'pause-menu entity-id)))

(define-prototype pause-menu-spawner ()
    ()
    ((pause-menu-spawner-component)))
