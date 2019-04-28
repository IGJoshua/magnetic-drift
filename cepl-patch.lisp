;;;; cepl-patch.lisp

(in-package #:cepl.streams)

(defun+ init-buffer-stream-from-id (stream-obj
                                    vao-gl-object gpu-arrays
                                    index-array start length
                                    base-vertex retain-arrays)
  (when (not gpu-arrays)
    (assert (not index-array) () 'index-on-buffer-stream-with-no-gpu-arrays))
  (let* ((gpu-arrays (preprocess-gpu-arrays-for-vao gpu-arrays))
         ;; THIS SEEMS WEIRD BUT IF HAVE INDICES ARRAY THEN
         ;; LENGTH MUST BE LENGTH OF INDICES ARRAY NOT NUMBER
         ;; OF TRIANGLES
         (per-vert-gpu-arrays (remove-if #'consp gpu-arrays))
         (length (if gpu-arrays
                     length
                     1))
         (length (or length
                     (when index-array (first (dimensions index-array)))
                     (apply #'min (mapcar #'(lambda (x) (first (dimensions x)))
                                          per-vert-gpu-arrays)))))
    (assert (and (every #'cons-aware-1d-p gpu-arrays)
                 (if index-array (1d-p index-array) t))
            () "You can only make buffer-streams from 1D arrays")
    (setf (buffer-stream-start stream-obj) start
          (buffer-stream-length stream-obj) length
          (buffer-stream-managed stream-obj) t
          (buffer-stream-base-vertex stream-obj) (or base-vertex 0)
          (buffer-stream-vao stream-obj) (make-vao-from-id vao-gl-object
                                                           gpu-arrays
                                                           index-array)

          (buffer-stream-index-type stream-obj) (when index-array
                                                  (gpu-array-bb-element-type
                                                   index-array))

          (buffer-stream-gpu-arrays stream-obj) (when retain-arrays
                                                  (list gpu-arrays index-array)))
    stream-obj))
