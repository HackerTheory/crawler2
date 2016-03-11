(in-package :crawler2)

(defstruct (cell
            (:conc-name nil)
            (:constructor %make-cell))
  x y walkablep region)

(defmethod print-object ((o cell) stream)
  (with-slots (x y walkablep region) o
    (print-unreadable-object (o stream)
      (format stream "X:~S, Y:~S" x y walkablep region))))

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
