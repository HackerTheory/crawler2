(in-package :crawler2)

(defmethod make-buffers (stage)
  (with-slots (width height buffers tiles) stage
    (setf tiles (make-array `(,width ,height ,(length buffers))))
    (loop :for buffer :across buffers
          :do (dotimes (x width)
                (dotimes (y height)
                  (make-tile stage x y buffer))))
    tiles))

(defmethod current-buffer (stage)
  (aref (buffers stage) 0))

(defmethod next-buffer (stage)
  (aref (rotate (copy-seq (buffers stage)) -1)) 0)

(defmethod swap-buffers (stage)
  (with-slots (buffers) stage
    (setf buffers (rotate buffers -1))))
