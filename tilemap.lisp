;;;; tilemap.lisp

(in-package #:magnetic-drift)

(defclass tilemap-component (component)
  ((tiles :initarg :tiles
          :initform nil)
   (textures :initarg :textures
             :initform (make-hash-table))))

(define-component-system render-tilemap (entity-id alpha)
    (tilemap-component position-component) ()
  (declare (ignore alpha))
  (with-components ((tilemap tilemap-component)
                    (pos position-component))
      entity-id
    (declare (ignore tilemap) (ignore pos))
    nil))
