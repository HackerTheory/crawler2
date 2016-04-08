(in-package :crawler2)

(defclass cell ()
  ((x :reader cell-x
      :initarg :x)
   (y :reader cell-y
      :initarg :y)
   (carvedp :accessor carvedp
            :initform nil)
   (region-id :accessor region-id
              :initform nil)
   (features :accessor features
             :initform '(:wall))
   (distance :accessor distance
             :initform -1)))

(defmethod print-object ((o cell) stream)
  (with-slots (x y) o
    (print-unreadable-object (o stream)
      (format stream "X:~S, Y:~S" x y))))

(defmethod make-cell (stage x y &key)
  (setf (cell stage x y) (make-instance 'cell :x x :y y)))

(defmethod cell (stage x y &key)
  (incf *cell-calls*)
  (aref (grid stage) x y))

(defmethod (setf cell) (value stage x y &key)
  (setf (aref (grid stage) x y) value))

(defmethod valid-cell-p (stage x y)
  (with-slots (height width) stage
    (when (and (not (minusp x))
               (not (minusp y))
               (< x width)
               (< y height))
      (cell stage x y))))

(defmethod stage-border-p (stage cell)
  (with-slots (width height) stage
    (with-slots (x y) cell
      (or (zerop x)
          (zerop y)
          (= x (1- width))
          (= y (1- height))))))

(defmethod carve (stage cell &key (region-id *region*) feature)
  (setf (carvedp cell) t
        (region-id cell) region-id)
  (when-let ((region (get-region stage region-id)))
    (pushnew cell (cells region)))
  (add-feature cell feature)
  (remove-feature cell :wall))

(defmethod uncarve (stage cell)
  (with-slots (carvedp region-id features) cell
    (when-let ((region (get-region stage region-id)))
      (deletef (cells region) cell))
    (setf carvedp nil
          region-id nil
          features '(:wall))))

(defmethod featuresp (cell &rest features)
  (intersection features (features cell)))

(defmethod remove-feature (cell feature)
  (deletef (features cell) feature))

(defmethod add-feature (cell feature)
  (pushnew feature (features cell)))

(defmacro profile-cell-calls (msg &body body)
  (let ((result (gensym)))
    `(let ((*cell-calls* 0))
       (let ((,result (progn ,@body)))
         (format t "~A: Cells looked up: ~A~%" ,msg *cell-calls*)
         ,result))))
