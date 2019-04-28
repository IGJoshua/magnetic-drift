;;;; tilemap.lisp

(in-package #:magnetic-drift)


(defun-g instanced-quad-vert ((position :vec2)
                              (offset :vec2)
                              &uniform
                              (quad->model :mat4)
                              (model->world :mat4)
                              (world->view :mat4)
                              (view->projection :mat4))
  (let* ((uvs position)
         (pos (v! position 0.0 1.0))
         (pos (* quad->model pos))
         (pos (+ pos (v! offset 0 0)))
         (pos (* model->world pos))
         (pos (* world->view pos))
         (pos (* view->projection pos)))
    (values pos
            uvs)))

(defpipeline-g instanced-quad ()
  :vertex (instanced-quad-vert :vec2 :vec2)
  :fragment (texture-frag :vec2))

;; TODO: Make the tilemap take a 2d list and construct the necessary
;;       instance data to make one draw call per texture
(defclass tilemap-component (component)
  ((tiles :initarg :tiles
          :initform nil)
   (textures :initarg :textures
             :initform (make-hash-table))
   (tile-size :initarg :tile-size
              :initform 32)
   (positions :initarg :positions
              :initform (make-hash-table))))

(defmethod copy-component ((comp tilemap-component))
  (let* ((copy (make-instance 'tilemap-component))
         (copy-hash (slot-value copy 'textures)))
    (setf (slot-value copy 'tiles)
          nil)
    (maphash (lambda (k v)
               (setf (gethash k copy) v))
             comp)))

(defun tilemap-recalc-positions (tilemap)
  (with-slots (positions tiles tile-size textures) tilemap
    (let ((quad (make-gpu-array
                 (list (v! 0 0)
                       (v! 1 0)
                       (v! 1 1)
                       (v! 0 0)
                       (v! 1 1)
                       (v! 0 1))
                 :element-type :vec2
                 :dimensions 6)))
      (maphash
       (lambda (k v)
         (declare (ignore v))
         (let* ((count 0)
                (l (loop :for row :across tiles
                         :for row-num :from 0
                         :nconc
                         (loop :for col :across row
                               :for col-num :from 0
                               :when (eql col k)
                                 :do (incf count)
                               :when (eql col k)
                                 :collect (v! (* col-num tile-size)
                                              (- (* row-num tile-size)))))))
           (when l
             (setf (gethash k positions)
                   (list
                    (make-buffer-stream
                     (list quad
                           (cons
                            (make-gpu-array
                             l
                             :element-type :vec2
                             :dimensions (length l))
                            1)))
                    count)))))
       textures))))

(defun load-tilemap (filepath)
  (with-open-file (file filepath)
    (let ((str nil)
          (tilemap (make-instance 'tilemap-component))
          (tiles (make-array 0 :fill-pointer t :adjustable t)))
      (loop :with state := nil
            :with objects-str := nil
            :for line := (read-line file nil nil)
            :while line
            :do
               (when (and (> (length line) 3)
                          (string= (subseq line 0 3)
                                   "---"))
                 (setf state (subseq line 3)))
               (alexandria:switch (state :test #'string=)
                 ("OBJECTS"
                  (unless (string= line
                                   "---OBJECTS")
                    (setf objects-str
                          (if objects-str
                              (format nil "~a~%~a" objects-str line)
                              line))))
                 ("TEXTURES"
                  (unless (string= line
                                   "---TEXTURES")
                    (let ((char (char line 0))
                          (tex (subseq line 2)))
                      (setf (gethash char (slot-value tilemap 'textures))
                            tex))))
                 ("MAP"
                  (unless (string= line
                                   "---MAP")
                    (let ((row (apply #'vector
                                      (loop :for char :across line
                                            :collect char))))
                      (vector-push-extend row tiles)))))
            :finally (setf str objects-str))
      (setf (slot-value tilemap 'tiles)
            tiles)
      (values
       tilemap
       (when str
         (with-input-from-string (str str)
           (cons 'progn
                 (loop :with sentinel := '#:EOF
                       :for form := (read str nil sentinel)
                       :until (eq form sentinel)
                       :collect form))))))))

(defun load-scene (filepath)
  (setf *entities* (make-hash-table))
  (let ((tilemap (add-component (make-entity) (make-instance 'position-component))))
    (multiple-value-bind (tilemap-component forms)
        (load-tilemap filepath)
      (tilemap-recalc-positions tilemap-component)
      (add-component tilemap tilemap-component)
      (eval forms))))

(define-component-system render-tilemap (entity-id alpha)
    ((tilemap tilemap-component)
     (pos position-component))
    ()
  (with-components ((camera-pos position-component)
                    (camera-comp camera-component))
      *camera*
    (when (and camera-pos camera-comp)
      (let ((tile-size (slot-value tilemap 'tile-size)))
        (maphash
         (lambda (k v)
           (let ((tex-name (gethash k (slot-value tilemap 'textures))))
             (when tex-name
               (let ((sam (texture tex-name)))
                 (with-instances (second v)
                   (map-g #'instanced-quad (first v)
                          :quad->model
                          (world-matrix (v! 0 (- tile-size))
                                        0
                                        (v! tile-size tile-size)
                                        -900)
                          :model->world
                          (world-matrix (slot-value pos 'pos)
                                        0
                                        (v! 1 1)
                                        0)
                          :world->view
                          (view-matrix (slot-value camera-pos 'pos)
                                       (let ((scale (/ (zoom camera-comp))))
                                         (v! scale scale)))
                          :view->projection (ortho-projection)
                          :sam sam))))))
         (slot-value tilemap 'positions))))))
