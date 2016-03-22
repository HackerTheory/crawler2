(in-package :crawler2)

(defstruct (neighborhood (:conc-name nil)
                         (:constructor %make-neighborhood))
  stage
  x
  y
  (extent (make-extent))
  (set-fn #'nset-default)
  (map-fn #'nmap-default))

(defstruct (extent (:conc-name nil))
  (minimum 1)
  (maximum 1))

(defun stage-coords (neighborhood nx ny)
  (with-slots (x y) neighborhood
    (values (+ x nx) (+ y ny))))

(defun nmap (neighborhood func &rest args)
  (apply (map-fn neighborhood) neighborhood func args))

(defun nsetp (neighborhood x y)
  (funcall (set-fn neighborhood) neighborhood x y))

(defun nref (neighborhood x y)
  (with-slots (stage) neighborhood
    (when (nsetp neighborhood x y)
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

(defun make-neighborhood (set-fn map-fn extent-args)
  (lambda (stage x y)
    (%make-neighborhood :stage stage
                        :x x
                        :y y
                        :extent (apply #'make-extent extent-args)
                        :set-fn set-fn
                        :map-fn map-fn)))

(defmethod layout ((name (eql :ortho)) &rest extent-args)
  (make-neighborhood #'nset-ortho #'nmap-ortho extent-args))

(defmethod layout ((name (eql :diag)) &rest extent-args)
  (make-neighborhood #'nset-diag #'nmap-diag extent-args))

(defmethod layout ((name (eql :circle)) &rest extent-args)
  (make-neighborhood #'nset-circle #'nmap-default extent-args))

(defmethod layout ((name (eql :circle-outline)) &rest extent-args)
  (make-neighborhood #'nset-circle-outline #'nmap-default extent-args))

(defmethod layout ((name (eql :circle-outline+origin)) &rest extent-args)
  (make-neighborhood #'nset-circle-outline+origin #'nmap-default extent-args))

(defmethod layout ((name (eql :square)) &rest extent-args)
  (make-neighborhood #'nset-square #'nmap-square extent-args))

(defmethod layout ((name (eql :square-outline)) &rest extent-args)
  (make-neighborhood #'nset-square-outline #'nmap-square-outline extent-args))

(defmethod layout ((layout (eql :square-outline+origin)) &rest extent-args)
  (make-neighborhood #'nset-square-outline+origin #'nmap-square-outline+origin extent-args))

(defun nset-ortho (neighborhood x y)
  (with-slots (maximum) (extent neighborhood)
    (and (<= (abs x) maximum)
         (<= (abs y) maximum)
         (or (and (zerop x) (zerop y))
             (and (zerop x) (not (zerop y)))
             (and (not (zerop x)) (zerop y))))))

(defun nmap-ortho (neighborhood func &key max)
  (let ((results)
        (max (or max (maximum (extent neighborhood)))))
    (loop :for y :from (- max) :to max
          :for cell = (nref neighborhood 0 y)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from (- max) :below 0
          :for cell = (nref neighborhood x 0)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from 1 :to max
          :for cell = (nref neighborhood x 0)
          :when cell
            :do (push (funcall func cell) results))
    results))

(defun nset-diag (neighborhood x y)
  (with-slots (maximum) (extent neighborhood)
    (and (<= (abs x) maximum)
         (<= (abs y) maximum)
         (= (abs x) (abs y)))))

(defun nmap-diag (neighborhood func &key max)
  (let ((results)
        (max (or max (maximum (extent neighborhood)))))
    (loop :for x :from (- max) :to max
          :for cell = (nref neighborhood x (- x))
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from (- max) :below 0
          :for cell = (nref neighborhood x x)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from 1 :to max
          :for cell = (nref neighborhood x x)
          :when cell
            :do (push (funcall func cell) results))
    results))

(defun nset-circle (neighborhood x y)
  (with-slots (maximum) (extent neighborhood)
    (<= (+ (* x x) (* y y))
        (* maximum maximum))))

(defun nset-circle-outline (neighborhood x y)
  (with-slots (minimum maximum) (extent neighborhood)
    (and (nset-circle neighborhood x y)
         (> (+ (* x x) (* y y))
            (* (1- minimum) (1- minimum))))))

(defun nset-circle-outline+origin (neighborhood x y)
  (or (nset-circle-outline neighborhood x y)
      (and (zerop x)
           (zerop y))))

(defun nset-square (neighborhood x y)
  (nset-default neighborhood x y))

(defun nmap-square (neighborhood func &key max)
  (nmap-default neighborhood func max))

(defun nset-square-outline (neighborhood x y)
  (with-slots (minimum maximum) (extent neighborhood)
    (and (nset-square neighborhood x y)
         (not (and (> x (- minimum))
                   (> y (- minimum))
                   (< x minimum)
                   (< y minimum))))))

(defun nmap-square-outline (neighborhood func &key min max)
  (let ((results)
        (min (or min (minimum (extent neighborhood))))
        (max (or max (maximum (extent neighborhood)))))
    (loop :for y :from min :to max
          :do (loop :for x :from (- max) :to max
                    :for cell = (nref neighborhood x y)
                    :when cell
                      :do (push (funcall func cell) results)))
    (loop :for y :from (- max) :to (- min)
          :do (loop :for x :from (- max) :to max
                    :for cell = (nref neighborhood x y)
                    :when cell
                      :do (push (funcall func cell) results)))
    (loop :for y :from (1+ (- min)) :below min
          :do (loop :for x :from (- max) :to (- min)
                    :for cell = (nref neighborhood x y)
                    :when cell
                      :do (push (funcall func cell) results)))
    (loop :for y :from (1+ (- min)) :below min
          :do (loop :for x :from min :to max
                    :for cell = (nref neighborhood x y)
                    :when cell
                      :do (push (funcall func cell) results)))
    results))

(defun nset-square-outline+origin (neighborhood x y)
  (or (nset-square-outline neighborhood x y)
      (and (zerop x)
           (zerop y))))

(defun nmap-square-outline+origin (neighborhood func &key min max)
  (cons (funcall func (origin neighborhood))
        (nmap-square-outline neighborhood func :min min :max max)))

(defun nset-default (neighborhood x y)
  (with-slots (maximum) (extent neighborhood)
    (and (>= x (- maximum))
         (>= y (- maximum))
         (<= x maximum)
         (<= y maximum))))

(defun nmap-default (neighborhood func &key max)
  (let ((results)
        (max (or max (maximum (extent neighborhood)))))
    (loop :for y :from max :downto (- max)
          :do (loop :for x :from (- max) :to max
                    :for cell = (nref neighborhood x y)
                    :when cell
                      :do (push (funcall func cell) results)))
    results))
