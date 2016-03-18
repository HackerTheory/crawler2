(in-package :crawler2)

(defstruct (cell (:constructor %make-cell))
  x y carved-p region)

(defmethod print-object ((o cell) stream)
  (with-slots (x y) o
    (print-unreadable-object (o stream)
      (format stream "X:~S, Y:~S" x y))))

(defmethod valid-cell-p (stage x y)
  (with-slots (height width) stage
    (when (and (not (minusp x))
               (not (minusp y))
               (< x width)
               (< y height))
      (cell stage x y))))

(defmethod cell (stage x y &key buffer)
  (let ((z (or buffer (current-buffer stage))))
    (aref (grid stage) x y z)))

(defmethod (setf cell) (value stage x y &key buffer)
  (let ((z (or buffer (next-buffer stage))))
    (setf (aref (grid stage) x y z) value)))

(defmethod make-cell (stage x y buffer)
  (setf (cell stage x y :buffer buffer) (%make-cell :x x :y y)))

(defun convolve (stage layout filter effect)
  (with-slots (width height) stage
    (loop :with affected-p
          :for x :from 1 :below (1- width)
          :do (loop :for y :from 1 :below (1- height)
                    :for neighborhood = (funcall layout stage x y)
                    :when (funcall filter stage neighborhood)
                      :do (setf affected-p (or affected-p (funcall effect stage neighborhood))))
          :finally (return affected-p))))
