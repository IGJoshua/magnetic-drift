;;;; rendering.lisp

(in-package #:magnetic-drift)

(defvar *quad-stream* nil)
(defvar *textures* nil)
(defvar *fonts* nil
  "(truename point-size properties-mask) <=> ttf-font")

(defvar *blending-params* nil)

(defparameter *scene-render-systems* '(resize-viewport
                                       clear-fbo
                                       select-camera
                                       render-tilemap
                                       render-textured
                                       render-normal-text
                                       render-ui-texture
                                       render-ui-text
                                       swap))

(defun texture (filename)
  (alexandria:if-let ((tex (gethash filename *textures*)))
    tex
    (setf (gethash filename *textures*) (sample (dirt:load-image-to-texture filename)))))

(defun font-properties-bitmask (bold italic underline strike-through)
  (logior
   (if bold sdl2-ttf::+style-bold+ 0)
   (if italic sdl2-ttf::+style-italic+ 0)
   (if underline sdl2-ttf::+style-underline+ 0)
   (if strike-through sdl2-ttf::+style-strike-through+ 0)))

(defun ttf-font (font-path &key (point-size 12) bold italic underline strike-through)
  "Get a `ttf-font' for the file at `font-path'
  Returns a cached font, or loads a new one if necessary."
  (let* ((truename (truename font-path))
         (properties (font-properties-bitmask bold italic underline strike-through))
         (key (list truename point-size properties))
         (ttf-font (gethash key *fonts*)))
    (unless ttf-font
      (setf ttf-font (sdl2-ttf::ttf-open-font (uiop:native-namestring truename) point-size))
      (sdl2-ttf::ttf-set-font-style ttf-font properties)
      (setf (gethash key *fonts*) ttf-font))
    ttf-font))

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
  (let ((uvs (v! (x uvs)
                 (- 1 (y uvs)))))
    (texture sam uvs)))

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

(defun-g ui-vert ((position :vec2)
                  &uniform
                  (quad->model :mat4)
                  (model->clip :mat4))
  (let* ((uvs (/ (+ (v! 1 1) position) 2.0))
         (pos (v! position 0.0 1.0))
         (pos (* quad->model pos))
         (pos (* model->clip pos)))
    (values pos
            uvs)))

(defpipeline-g textured-ui-quad ()
  :vertex (ui-vert :vec2)
  :fragment (texture-frag :vec2))

(defun ortho-projection ()
  (destructuring-bind (x y) (cepl.viewports:viewport-dimensions (current-viewport))
    (rtg-math.projection:orthographic (float x) (float y) 0.1 1000.0)))

(defun view-matrix (pos scale)
  (m! (/ (x scale)) 0 0 (/ (- (x pos)) (x scale))
      0 (/ (y scale)) 0 (/ (- (y pos)) (y scale))
      0 0 1 0
      0 0 0 1))

(defun ui-matrix (pos scale)
  (m! (/ (x scale)) 0 0 (/ (+ (x pos)) (x scale))
      0 (/ (y scale)) 0 (/ (+ (y pos)) (y scale))
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
  (gl:disable :depth-test)
  (unless *quad-stream*
    (setf *quad-stream* (nineveh:get-quad-stream-v2)))
  (unless *textures*
    (setf *textures* (make-hash-table :test 'equal)))
  (unless *fonts*
    (setf *fonts* (make-hash-table :test 'equalp)))
  (unless *blending-params*
    (setf *blending-params* (make-blending-params)))
  (cepl.sdl2-ttf:init-cepl-sdl2-ttf))

(defun render (alpha)
  (loop :for system :in *scene-render-systems*
        :do (run-system system alpha)))

(defclass texture-component (component)
  ((texture :initarg :texture
            :initform (error "Cannot create a textured object without a texture"))
   (offset :initarg :offset
           :initform (v! 0 0))
   (rotation :initarg :rotation
             :initform 0)
   (scale :initarg :scale
          :initform (v! 1 1))))

(defmethod copy-component ((comp texture-component))
  (make-instance 'texture-component
                 :texture (slot-value comp 'texture-component)))

(define-global-system clear-fbo (alpha)
  (declare (ignore alpha))
  (clear-fbo (fbo-bound (cepl-context))))

(define-global-system resize-viewport (alpha)
  (declare (ignore alpha))
  (setf (cepl:viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context)))))

(define-global-system swap (alpha)
  (declare (ignore alpha))
  (swap))

(define-component-system select-camera (entity-id alpha)
    (position-component camera-component) ()
  (declare (ignore alpha))
  (when (active-p (get-component entity-id 'camera-component))
    (setf *camera* entity-id)))

(define-component-system render-textured (entity-id alpha)
    (position-component texture-component) (camera-component)
  (declare (ignore alpha))
  (with-components ((camera-pos position-component)
                    (camera-comp camera-component))
      *camera*
    (when (and camera-pos camera-comp)
      (with-components ((tex-comp texture-component)
                        (pos-comp position-component)
                        (rot-comp rotation-component)
                        (scale-comp scale-component))
          entity-id
        (let* ((sam (texture (slot-value tex-comp 'texture)))
               (tex (slot-value sam 'texture)))
          (destructuring-bind (x y) (texture-base-dimensions tex)
            (with-blending *blending-params*
              (map-g #'textured-object-quad *quad-stream*
                     :quad->model (world-matrix (slot-value tex-comp 'offset)
                                                (float (slot-value tex-comp 'rotation)
                                                       1f0)
                                                (v2-n:* (v! x y)
                                                        (slot-value tex-comp 'scale)))
                     :model->world (world-matrix (slot-value pos-comp 'pos)
                                                 (float (if rot-comp
                                                            (slot-value rot-comp 'rot)
                                                            0)
                                                        1f0)
                                                 (if scale-comp
                                                     (slot-value scale-comp 'scale)
                                                     (v! 1 1)))
                     :world->view (view-matrix (slot-value camera-pos 'pos)
                                               (let ((scale (/ (zoom camera-comp))))
                                                 (v! scale scale)))
                     :view->projection (ortho-projection)
                     :sam sam))))))))

(defclass text-component (component)
  ((font :initarg :font
         :initform (error "Cannot create a text object without a font"))
   (text :initarg :text
         :initform "")
   (color :initarg :color
          :initform (v! 255 255 255 0))
   (point-size :initarg :point-size
               :initform 12)
   (bold :initarg :bold
         :initform nil)
   (italic :initarg :italic
           :initform nil)
   (underline :initarg :underline
              :initform nil)
   (strike-through :initarg :strike-through
                   :initform nil)
   (offset :initarg :offset
           :initform (v! 0 0))
   (rotation :initarg :rotation
             :initform 0)
   (scale :initarg :scale
          :initform (v! 1 1))))

(defmethod copy-component ((comp text-component))
  (with-slots (font text color point-size
               bold italic underline strike-through
               offset rotation scale)
      comp
    (make-instance
     'text-component
     :font font
     :text (copy-seq text)
     :color color
     :point-size point-size
     :bold bold
     :italic italic
     :underline underline
     :strike-through strike-through
     :offset (v! (x offset) (y offset))
     :rotation rotation
     :scale (v! (x scale) (y scale)))))

(defun text-size-zero-p (ttf-font text)
  (cffi:with-foreign-objects ((w :int) (h :int))
    (sdl2-ttf::ttf-size-utf8 ttf-font text w h)
    (or (zerop (cffi:mem-ref w :int))
        (zerop (cffi:mem-ref h :int)))))

(defun text-to-tex (text font &optional (color (v! 255 255 255 0)))
  "Workaround for `cepl.sdl2-ttf:text-to-tex' which cancels autocollect on the rendered surface"
  (let* ((texture-surface (sdl2-ttf:render-utf8-blended
                           font text
                           (round (x color)) (round (y color))
                           (round (z color)) (round (w color))))
         (width (sdl2:surface-width texture-surface))
         (height (sdl2:surface-height texture-surface)))
    (unwind-protect
         (let ((carr (make-c-array-from-pointer
                      (list width height)
                      :uint8-vec4
                      (sdl2:surface-pixels texture-surface))))
           (make-texture carr))
      (sdl2:free-surface texture-surface)
      (autowrap:autocollect-cancel texture-surface))))

(define-component-system render-normal-text (entity-id alpha)
    (position-component text-component) (camera-component)
  (declare (ignore alpha))
  (with-components ((camera-pos position-component)
                    (camera-comp camera-component))
      *camera*
    (when (and camera-pos camera-comp)
      (with-components ((text-comp text-component)
                        (pos-comp position-component)
                        (rot-comp rotation-component)
                        (scale-comp scale-component))
          entity-id
        (let ((font (ttf-font (slot-value text-comp 'font)))
              (text (slot-value text-comp 'text)))
          (unless (text-size-zero-p font text)
            (let* ((tex (text-to-tex text font (slot-value text-comp 'color)))
                   (sam (sample tex)))
              (unwind-protect
                   (destructuring-bind (x y) (texture-base-dimensions tex)
                     (with-blending *blending-params*
                       (map-g #'textured-object-quad *quad-stream*
                              :quad->model (world-matrix (slot-value text-comp 'offset)
                                                         (float (slot-value text-comp 'rotation)
                                                                1f0)
                                                         (v2-n:* (v! x y)
                                                                 (slot-value text-comp 'scale)))
                              :model->world (world-matrix (slot-value pos-comp 'pos)
                                                          (float (if rot-comp
                                                                     (slot-value rot-comp 'rot)
                                                                     0)
                                                                 1f0)
                                                          (if scale-comp
                                                              (slot-value scale-comp 'scale)
                                                              (v! 1 1)))
                              :world->view (view-matrix (slot-value camera-pos 'pos)
                                                        (let ((scale (/ (zoom camera-comp))))
                                                          (v! scale scale)))
                              :view->projection (ortho-projection)
                              :sam sam)))
                (cepl:free sam)
                (cepl:free tex)))))))))

(defclass ui-position-component (component)
  ((anchor :initarg :anchor
           :initform (v! 0 0)
           :documentation "[-1,1] anchor to the viewport. -1,-1 is bottom-left")
   (pos :initarg :pos
        :initform (v! 0 0)
        :documentation "Offset from anchor in pixel coordinates.")))

(defmethod copy-component ((comp ui-position-component))
  (with-slots (pos) comp
    (make-instance 'position-component
                   :pos (v! (x pos) (y comp)))))

(defun render-ui-texture-impl (sam offset rotation scale pos anchor)
  (let ((tex (slot-value sam 'texture)))
    (destructuring-bind (x y) (texture-base-dimensions tex)
      (destructuring-bind (vx vy) (cepl.viewports:viewport-dimensions (current-viewport))
        (with-blending *blending-params*
          (map-g #'textured-ui-quad *quad-stream*
                 :quad->model (world-matrix offset
                                            (float rotation 1f0)
                                            (v2-n:* (v! x y) scale))
                 :model->clip (ui-matrix (v2-n:+ (v2-n:* (v! vx vy) anchor) pos)
                                         (v! vx vy))
                 :sam sam))))))

(define-component-system render-ui-texture (entity-id alpha)
    (ui-position-component texture-component) (camera-component)
  (declare (ignore alpha))
  (with-components ((camera-pos position-component)
                    (camera-comp camera-component))
      *camera*
    (when (and camera-pos camera-comp)
      (with-components ((tex-comp texture-component)
                        (pos-comp ui-position-component))
          entity-id
        (with-slots (texture offset rotation scale) tex-comp
          (with-slots (pos anchor) pos-comp
            (render-ui-texture-impl (texture texture) offset rotation scale pos anchor)))))))

(define-component-system render-ui-text (entity-id alpha)
    (ui-position-component text-component) (camera-component)
  (declare (ignore alpha))
  (with-components ((camera-pos position-component)
                    (camera-comp camera-component))
      *camera*
    (when (and camera-pos camera-comp)
      (with-components ((text-comp text-component)
                        (pos-comp ui-position-component))
          entity-id
        (let ((font (ttf-font (slot-value text-comp 'font)))
              (text (slot-value text-comp 'text)))
          (unless (text-size-zero-p font text)
            (let* ((tex (text-to-tex text font (slot-value text-comp 'color)))
                   (sam (sample tex)))
              (unwind-protect
                   (with-slots (offset rotation scale) text-comp
                     (with-slots (pos anchor) pos-comp
                       (render-ui-texture-impl sam offset rotation scale pos anchor)))
                (cepl:free sam)
                (cepl:free tex)))))))))
