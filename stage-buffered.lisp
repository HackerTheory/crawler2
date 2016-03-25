(in-package :crawler2)

(defclass buffered-stage (stage)
  ((buffers :reader buffers
            :initform (vector 0))))

(defmethod make-grid ((stage buffered-stage))
  (with-slots (width height buffers grid) stage
    (setf grid (make-array `(,width ,height ,(length buffers))))
    (loop :for buffer :across buffers
          :do (dotimes (x width)
                (dotimes (y height)
                  (make-cell stage x y :buffer buffer))))
    grid))

(defmethod cell ((stage buffered-stage) x y &key buffer)
  (let ((z (or buffer (current-buffer stage))))
    (aref (grid stage) x y z)))

(defmethod (setf cell) (value (stage buffered-stage) x y &key buffer)
  (let ((z (or buffer (next-buffer stage))))
    (setf (aref (grid stage) x y z) value)))

(defmethod make-cell :around ((stage buffered-stage) x y &key buffer)
  (setf (cell stage x y :buffer buffer) (call-next-method)))

(defmethod current-buffer (stage)
  (aref (buffers stage) 0))

(defmethod next-buffer (stage)
  (aref (rotate (copy-seq (buffers stage)) -1) 0))

(defmethod swap-buffers (stage)
  (with-slots (buffers) stage
    (setf buffers (rotate buffers -1))))
