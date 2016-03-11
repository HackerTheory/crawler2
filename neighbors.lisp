(in-package :crawler2)

(defstruct (neighborhood (:conc-name nil))
  stage
  x
  y
  (extent (make-extent))
  (set-fn #'nset-default)
  (map-fn #'nmap-default))

(defstruct (extent (:conc-name nil))
  (max-distance 1))

(defun stage-coords (neighborhood nx ny)
  (with-slots (x y) neighborhood
    (values (+ x nx) (+ y ny))))

(defun nmap (neighborhood func)
  (funcall (map-fn neighborhood) neighborhood func))

(defun neighborp (neighborhood x y)
  (funcall (set-fn neighborhood) neighborhood x y))

(defun nref (neighborhood x y)
  (with-slots (stage) neighborhood
    (when (neighborp neighborhood x y)
      (multiple-value-bind (stage-x stage-y) (stage-coords neighborhood x y)
        (valid-cell-p stage stage-x stage-y)))))

(defun origin (neighborhood)
  (nref neighborhood 0 0))

(defun n (neighborhood &optional (distance 1))
  (nref neighborhood 0 distance))

(defun nw (neighborhood &optional (distance 1))
  (nref neighborhood (- distance) distance))

(defun w (neighborhood &optional (distance 1))
  (nref neighborhood (- distance) 0))

(defun sw (neighborhood &optional (distance 1))
  (nref neighborhood (- distance) (- distance)))

(defun s (neighborhood &optional (distance 1))
  (nref neighborhood 0 (- distance)))

(defun se (neighborhood &optional (distance 1))
  (nref neighborhood distance (- distance)))

(defun e (neighborhood &optional (distance 1))
  (nref neighborhood distance 0))

(defun ne (neighborhood &optional (distance 1))
  (nref neighborhood distance distance))

(defmethod neighborhood :around (layout &key)
  (multiple-value-bind (set-fn map-fn extent-args) (call-next-method)
    (lambda (stage x y)
      (make-neighborhood :stage stage
                         :x x
                         :y y
                         :extent (apply #'make-extent extent-args)
                         :set-fn set-fn
                         :map-fn map-fn))))

(defmethod neighborhood ((layout (eql :ortho)) &rest extent-args)
  (values #'nset-ortho #'nmap-ortho extent-args))

(defmethod neighborhood ((layout (eql :diag)) &rest extent-args)
  (values #'nset-diag #'nmap-diag extent-args))

(defmethod neighborhood ((layout (eql :circle)) &rest extent-args)
  (values #'nset-circle #'nmap-default extent-args))

(defmethod neighborhood ((layout (eql :square)) &rest extent-args)
  (values #'nset-square #'nmap-square extent-args))

(defun nset-ortho (neighborhood x y)
  (with-slots (max-distance) (extent neighborhood)
    (and (<= (abs x) max-distance)
         (<= (abs y) max-distance)
         (or (and (zerop x) (zerop y))
             (and (zerop x) (not (zerop y)))
             (and (not (zerop x)) (zerop y))))))

(defun nmap-ortho (neighborhood func)
  (with-slots (max-distance) (extent neighborhood)
    (let ((results)
          (examined 0))
      (loop :for y :from (- max-distance) :to max-distance
            :for cell = (nref neighborhood 0 y)
            :do (incf examined)
            :when cell
              :collect (funcall func cell) :into results)
      (loop :for x :from (- max-distance) :below 0
            :for cell = (nref neighborhood x 0)
            :do (incf examined)
            :when cell
              :collect (funcall func cell) :into results)
      (loop :for x :from 1 :to max-distance
            :for cell = (nref neighborhood x 0)
            :do (incf examined)
            :when cell
              :collect (funcall func cell) :into results)
      (values results examined))))

(defun nset-diag (neighborhood x y)
  (with-slots (max-distance) (extent neighborhood)
    (and (<= (abs x) max-distance)
         (<= (abs y) max-distance)
         (= (abs x) (abs y)))))

(defun nmap-diag (neighborhood func)
  (with-slots (max-distance) (extent neighborhood)
    (let ((results)
          (examined 0))
      (loop :for x :from (- max-distance) :to max-distance
            :for cell = (nref neighborhood x (- x))
            :do (incf examined)
            :when cell
              :collect (funcall func cell) :into results)
      (loop :for x :from (- max-distance) :below 0
            :for cell = (nref neighborhood x x)
            :do (incf examined)
            :when cell
              :collect (funcall func cell) :into results)
      (loop :for x :from 1 :to max-distance
            :for cell = (nref neighborhood x x)
            :do (incf examined)
            :when cell
              :collect (funcall func cell) :into results)
      (values results examined))))

(defun nset-circle (neighborhood x y)
  (with-slots (max-distance) (extent neighborhood)
    (<= (+ (* x x) (* y y))
        (* max-distance max-distance))))

(defun nset-square (neighborhood x y)
  (with-slots (max-distance) (extent neighborhood)
    (and (>= x (- max-distance))
         (>= y (- max-distance))
         (<= x max-distance)
         (<= y max-distance))))

(defun nmap-square (neighborhood func)
  (with-slots (max-distance) (extent neighborhood)
    (let ((results)
          (examined 0))
      (loop :for y :from max-distance :downto (- max-distance)
            :do (loop :for x :from (- max-distance) :to max-distance
                      :for cell = (nref neighborhood x y)
                      :do (incf examined)
                      :when cell
                        :collect (funcall func cell) :into results))
      (values results examined))))

(defun nset-default (neighborhood x y)
  (nset-square neighborhood x y))

(defun nmap-default (neighborhood func)
  (nmap-square neighborhood func))
