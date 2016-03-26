(in-package :crawler2)

(defclass cell ()
  ((x :reader cell-x
      :initarg :x)
   (y :reader cell-y
      :initarg :y)
   (carvedp :accessor carvedp
            :initform nil)
   (region-id :accessor region-id
              :initform nil)))

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

(defmethod carve (stage cell &optional (region-id (current-region stage)))
  (setf (carvedp cell) t
        (region-id cell) region-id))

(defmethod uncarve (stage cell)
  (setf (carvedp cell) nil
        (region-id cell) nil))

(defun convolve (stage layout filter effect)
  (with-slots (width height) stage
    (loop :with affectedp
          :for x :from 1 :below (1- width)
          :do (loop :for y :from 1 :below (1- height)
                    :for neighborhood = (funcall layout stage x y)
                    :when (funcall filter stage neighborhood)
                      :do (let ((value (funcall effect stage neighborhood)))
                            (setf affectedp (or affectedp value))))
          :finally (return affectedp))))

(defun collect-cells (stage layout filter)
  (let ((cells))
    (convolve
     stage
     layout
     filter
     (lambda (s n)
       (declare (ignore s))
       (push n cells)))
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
