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
             :initform '(:wall))))

(defmethod print-object ((o cell) stream)
  (with-slots (x y) o
    (print-unreadable-object (o stream)
      (format stream "X:~S, Y:~S" x y))))

(defmethod make-cell :around (stage x y &key)
  (setf (cell stage x y) (call-next-method)))

(defmethod valid-cell-p (stage x y)
  (with-slots (height width) stage
    (when (and (not (minusp x))
               (not (minusp y))
               (< x width)
               (< y height))
      (cell stage x y))))

(defmethod cell (stage x y &key)
  (incf *cell-calls*)
  (aref (grid stage) x y))

(defmethod (setf cell) (value stage x y &key)
  (setf (aref (grid stage) x y) value))

(defmethod carve (stage cell &key (region (current-region stage)) feature)
  (setf (carvedp cell) t
        (region-id cell) region)
  (add-feature cell feature))

(defmethod uncarve (stage cell)
  (setf (carvedp cell) nil
        (region-id cell) nil
        (features cell) '(:wall)))

(defmethod featuresp (cell &rest features)
  (some (lambda (x) (member x features)) (features cell)))

(defmethod remove-feature (cell feature)
  (deletef (features cell) feature))

(defmethod add-feature (cell feature)
  (remove-feature cell :wall)
  (pushnew feature (features cell)))

(defun convolve (stage layout filter effect &key (x1 1) (x2 -1) (y1 1) (y2 -1))
  (with-slots (width height) stage
    (loop :with affectedp
          :for x :from x1 :below (+ width x2)
          :do (loop :for y :from y1 :below (+ height y2)
                    :for neighborhood = (funcall layout stage x y)
                    :when (funcall filter stage neighborhood)
                      :do (let ((value (funcall effect stage neighborhood)))
                            (setf affectedp (or affectedp value))))
          :finally (return affectedp))))

(defun collect-cells (stage layout filter &key (x1 1) (x2 -1) (y1 1) (y2 -1))
  (let ((cells))
    (convolve
     stage
     layout
     filter
     (lambda (s n)
       (declare (ignore s))
       (push n cells))
     :x1 x1
     :x2 x2
     :y1 y1
     :y2 y2)
    cells))

(defun process-cells (stage layout filter processor)
  (loop :with cells = (collect-cells stage layout filter)
        :while cells
        :do (loop :with neighborhood = (pop cells)
                  :while (funcall filter stage neighborhood)
                  :for new = (funcall processor stage neighborhood)
                  :when new
                    :do (push new cells))))

(defmacro profile-cell-calls (msg &body body)
  (let ((result (gensym)))
    `(let ((*cell-calls* 0))
       (let ((,result (progn ,@body)))
         (format t "~A: Cells looked up: ~A~%" ,msg *cell-calls*)
         ,result))))
