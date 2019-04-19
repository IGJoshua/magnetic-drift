;;;; rendering.lisp

(in-package #:magnetic-drift)

(defvar *quad-stream* nil)
(defvar *textures* nil)
(defvar *blending-params* nil)

(defun texture (filename)
  (alexandria:if-let ((tex (gethash filename *textures*)))
    tex
    (setf (gethash filename *textures*) (dirt:load-image-to-texture filename))))

(defun sampler-for-texture (filename)
  (sample (texture filename)))

(defun-g fullscreen-quad-vert ((positions :vec2))
  (values (v! positions 0 1)
          (/ (+ (v! 1 1) positions) 2)))

(defun-g red-frag ((uvs :vec2))
  (v! 1 0 0 1))

(defpipeline-g red-fullscreen-quad ()
  :vertex (fullscreen-quad-vert :vec2)
  :fragment (red-frag :vec2))

(defun-g texture-frag ((uvs :vec2)
                       &uniform
                       (sam :sampler-2d))
  (texture sam uvs))

(defpipeline-g textured-fullscreen-quad ()
  :vertex (fullscreen-quad-vert :vec2)
  :fragment (texture-frag :vec2))

(defun-g quad-vert ((position :vec2)
                    &uniform
                    (quad->model :mat4)
                    (model->world :mat4)
                    (world->view :mat4)
                    (view->projection :mat4))
  (let* ((uvs (/ (+ (v! 1 1) position) 2.0))
         (pos (v! position 0.0 1.0))
         (pos (* quad->model pos))
         (pos (* model->world pos))
         (pos (* world->view pos))
         (pos (* view->projection pos)))
    (values pos
            uvs)))

(defpipeline-g textured-object-quad ()
  :vertex (quad-vert :vec2)
  :fragment (texture-frag :vec2))

(defun ortho-projection ()
  (destructuring-bind (x y) (cepl.viewports:viewport-dimensions (current-viewport))
    (rtg-math.projection:orthographic (float x) (float y) 0.1 1000.0)))

(defun view-matrix (pos scale)
  (m! (/ (x scale)) 0 0 (/ (- (x pos)) (x scale))
      0 (/ (y scale)) 0 (/ (- (y pos)) (y scale))
      0 0 1 0
      0 0 0 1))

(defun init-renderer ()
  (setf (clear-color) (v! 1.0 1.0 1.0 1))
  (unless *blending-params*
    (setf *blending-params* (make-blending-params))))

(defun render (dt)
  (declare (ignore dt))
  (clear)
  (setf (cepl:viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (let* ((tex (texture "./res/car_blue_1.png"))
         (sam (sample tex)))
    (destructuring-bind (x y) (texture-base-dimensions tex)
      (with-blending *blending-params*
        (map-g #'textured-object-quad *quad-stream*
               :quad->model (m! x 0 0 0
                                0 y 0 0
                                0 0 1 0
                                0 0 0 1)
               :model->world (m! 1 0 0 0
                                 0 1 0 0
                                 0 0 1 0
                                 0 0 0 1)
               :world->view (view-matrix (pos *camera*)
                                         (let ((scale (/ (zoom *camera*))))
                                           (v! scale scale)))
               :view->projection (ortho-projection)
               :sam sam))))
  (swap))
