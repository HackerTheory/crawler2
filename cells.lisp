(in-package :crawler2)

(defstruct (cell
            (:conc-name nil)
            (:constructor %make-cell))
  x y walkablep region)

(defmethod print-object ((o cell) stream)
  (with-slots (x y walkablep region) o
    (print-unreadable-object (o stream)
      (format stream "X:~S, Y:~S" x y walkablep region))))

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

(defmethod count-cells (stage)
  (* (width stage) (height stage)))

(defun convolve (stage layout filter effect &key (start '(1 1)) (end '(-1 -1)))
  (with-slots (width height) stage
    (loop :with affected-p
          :for x :from (first start) :below (+ width (first end))
          :do (loop :for y from (second start) :below (+ height (second end))
                    :for neighborhood = (funcall layout stage x y)
                    :when (funcall filter neighborhood)
                      :do (let ((value (funcall effect neighborhood)))
                            (setf affected-p (or affected-p value))))
          :finally (return affected-p))))
