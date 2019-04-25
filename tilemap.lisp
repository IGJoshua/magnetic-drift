;;;; tilemap.lisp

(in-package #:magnetic-drift)

(defclass tilemap-component (component)
  ((tiles :initarg :tiles
          :initform nil)
   (textures :initarg :textures
             :initform (make-hash-table))
   (tile-size :initarg :tile-size
              :initform 16)))

(define-component-system render-tilemap (entity-id alpha)
    (tilemap-component position-component) ()
  (declare (ignore alpha))
  (with-components ((camera-pos position-component)
                    (camera-comp camera-component))
      *camera*
    (when (and camera-pos camera-comp)
      (with-components ((tilemap tilemap-component)
                        (pos position-component))
          entity-id
        (loop :for row :across (slot-value tilemap 'tiles)
              :for row-num :from 0
              :do
                 (loop :for tile :across row
                       :for col-num :from 0
                       :do
                          (let ((sam (texture "./res/Tiles/Asphalt road/road_asphalt01.png"))
                                (tile-size (slot-value tilemap 'tile-size)))
                            (map-g #'textured-object-quad *quad-stream*
                                   :quad->model
                                   (world-matrix (v! 0 0)
                                                 0
                                                 (v! tile-size tile-size))
                                   :model->world
                                   (world-matrix (v2-n:+
                                                  (v!
                                                   (* col-num
                                                      2 tile-size)
                                                   (* row-num
                                                      2 tile-size))
                                                  (slot-value pos 'pos))
                                                 0
                                                 (v! 1 1))
                                   :world->view
                                   (view-matrix (slot-value camera-pos 'pos)
                                                (let ((scale (/ (zoom camera-comp))))
                                                  (v! scale scale)))
                                   :view->projection (ortho-projection)
                                   :sam sam))))))))
