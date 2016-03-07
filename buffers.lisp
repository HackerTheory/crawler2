(in-package :crawler2)

(defmethod make-buffers (stage)
  (with-slots (width height buffers tiles) stage
    (setf tiles (make-array `(,width ,height ,(length buffers))))
    (dolist (buffer buffers)
      (dotimes (x width)
        (dotimes (y height)
          (make-tile stage x y buffer))))
    tiles))

(defmethod current-buffer (stage)
  (first (buffers stage)))

(defmethod next-buffer (stage)
  (first (rotate (copy-seq (buffers stage)) -1)))

(defmethod swap-buffers (stage)
  (with-slots (buffers) stage
    (setf buffers (rotate buffers -1))))
