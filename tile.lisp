(in-package :crawler2)

(defclass tile ()
  ((x :reader x
      :initarg :x)
   (y :reader y
      :initarg :y)
   (walkablep :accessor walkablep
              :initarg :walkablep
              :initform nil)
   (region :accessor region
           :initform nil)))

(defmethod print-object ((o tile) stream)
  (with-slots (x y walkablep region) o
    (print-unreadable-object (o stream)
      (format stream "X:~S, Y:~S" x y walkablep region))))

(defmethod tile (stage x y &key buffer)
  (let ((z (or buffer (current-buffer stage))))
    (aref (tiles stage) x y z)))

(defmethod (setf tile) (value stage x y &key buffer)
  (let ((z (or buffer (next-buffer stage))))
    (setf (aref (tiles stage) x y z) value)))

(defmethod make-tile (stage x y buffer)
  (setf (tile stage x y :buffer buffer) (make-instance 'tile :x x :y y)))

(defmethod count-tiles (stage)
  (* (width stage) (height stage)))
