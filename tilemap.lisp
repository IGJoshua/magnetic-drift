;;;; tilemap.lisp

(in-package #:magnetic-drift)

;; TODO: Make the tilemap take a 2d list and construct the necessary
;;       instance data to make one draw call per texture
(defclass tilemap-component (component)
  ((tiles :initarg :tiles
          :initform nil)
   (textures :initarg :textures
             :initform (make-hash-table))
   (tile-size :initarg :tile-size
              :initform 16)))

(defmethod copy-component ((comp tilemap-component))
  (let* ((copy (make-instance 'tilemap-component))
         (copy-hash (slot-value copy 'textures)))
    (setf (slot-value copy 'tiles)
          nil)
    (maphash (lambda (k v)
               (setf (gethash k copy) v))
             comp)))

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
               (format t "Reading line: '~a'~%" line)
               (when (and (> (length line) 3)
                          (string= (subseq line 0 3)
                                   "---"))
                 (setf state (subseq line 3)))
               (format t "Currently in the ~a state~%" state)
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
      (loop :for row :across (slot-value tilemap 'tiles)
            :for row-num :from 0
            :do
               (loop :for tile :across row
                     :for col-num :from 0
                     :do
                        (let ((tex-name (gethash tile (slot-value tilemap 'textures))))
                          (when tex-name
                            (let ((sam (texture tex-name))
                                  (tile-size (slot-value tilemap 'tile-size)))
                              (map-g #'textured-object-quad *quad-stream*
                                     :quad->model
                                     (world-matrix (v! 0 0)
                                                   0
                                                   (v! tile-size tile-size)
                                                   -900)
                                     :model->world
                                     (world-matrix (v2-n:+
                                                    (v!
                                                     (* col-num
                                                        2 tile-size)
                                                     (* row-num
                                                        2 tile-size))
                                                    (slot-value pos 'pos))
                                                   0
                                                   (v! 1 1)
                                                   0)
                                     :world->view
                                     (view-matrix (slot-value camera-pos 'pos)
                                                  (let ((scale (/ (zoom camera-comp))))
                                                    (v! scale scale)))
                                     :view->projection (ortho-projection)
                                     :sam sam)))))))))
