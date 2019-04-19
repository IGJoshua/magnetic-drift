;;;; rendering.lisp

(in-package #:magnetic-drift)

(defvar *quad-stream* nil)
(defvar *textures* nil)
(defvar *blending-params* nil)

(defparameter *scene-render-systems* '(resize-viewport clear-fbo select-camera render-car swap))

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

(defun world-matrix (pos rot scale)
  (rtg-math.matrix4:* (rtg-math.matrix4:translation (v! pos 0))
                      (rtg-math.matrix4:rotation-z (float rot))
                      (rtg-math.matrix4:scale (v! scale 1))))

(defun set-clear-color-int (r g b)
  (setf (clear-color) (v! (/ r 255) (/ g 255) (/ b 255) 1)))

(defun init-renderer ()
  (setf (clear-color) (v! 0.57254905 0.65882355 0.7176471 1.0))
  (unless *quad-stream*
    (setf *quad-stream* (nineveh:get-quad-stream-v2)))
  (unless *textures*
    (setf *textures* (make-hash-table :test 'equal)))
  (unless *blending-params*
    (setf *blending-params* (make-blending-params))))

(define-global-system resize-viewport (alpha)
  (declare (ignore alpha))
  (setf (cepl:viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context)))))

(define-global-system clear-fbo (alpha)
  (declare (ignore alpha))
  (clear-fbo (fbo-bound (cepl-context))))

(define-global-system swap (alpha)
  (declare (ignore alpha))
  (swap))

(define-component-system select-camera (entity-id alpha)
    (position-component camera-component) ()
  (declare (ignore alpha))
  (when (active-p (get-component entity-id 'camera-component))
    (setf *camera* entity-id)))

(define-component-system render-car (entity-id alpha)
    (position-component) (camera-component)
  (declare (ignore alpha))
  (let* ((tex (texture "./res/car_blue_1.png"))
         (sam (sample tex))
         (pos-comp (get-component entity-id 'position-component)))
    (alexandria:when-let ((camera-pos (get-component *camera* 'position-component))
                          (camera-comp (get-component *camera* 'camera-component)))
      (destructuring-bind (x y) (texture-base-dimensions tex)
        (with-blending *blending-params*
          (map-g #'textured-object-quad *quad-stream*
                 :quad->model (m! x 0 0 0
                                  0 y 0 0
                                  0 0 1 0
                                  0 0 0 1)
                 :model->world (world-matrix (pos pos-comp) 0 (v! 1 1))
                 :world->view (view-matrix (pos camera-pos)
                                           (let ((scale (/ (zoom camera-comp))))
                                             (v! scale scale)))
                 :view->projection (ortho-projection)
                 :sam sam))))))

(defun render (alpha)
  (loop :for system :in *scene-render-systems*
        :do (run-system system alpha)))
