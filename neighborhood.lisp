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
  (minimum 0)
  (maximum 1))

(defun stage-coords (neighborhood nx ny)
  (with-slots (x y) neighborhood
    (values (+ x nx) (+ y ny))))

(defun nmap (neighborhood func &rest args)
  (apply (map-fn neighborhood) neighborhood func args))

(defun nsetp (neighborhood x y)
  (funcall (set-fn neighborhood) neighborhood x y))

(defun nfilter (neighborhood filter)
  (remove nil (nmap neighborhood (lambda (x) (when (funcall filter x) x)))))

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

(defun cell-nh (stage cell layout)
  (with-slots (x y) cell
    (funcall layout stage x y)))

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

(defun nh-realize (nh-generator stage x y)
  (funcall nh-generator stage x y))

(defun nset-ortho (neighborhood x y)
  (with-slots (maximum) (extent neighborhood)
    (and (<= (abs x) maximum)
         (<= (abs y) maximum)
         (or (and (zerop x) (zerop y))
             (and (zerop x) (not (zerop y)))
             (and (not (zerop x)) (zerop y))))))

(defun nmap-ortho (neighborhood func)
  (let ((results)
        (max (maximum (extent neighborhood))))
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

(defun nmap-diag (neighborhood func)
  (let ((results)
        (max (maximum (extent neighborhood))))
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
         (not (<= (+ (* x x) (* y y))
                  (* minimum minimum))))))

(defun nset-circle-outline+origin (neighborhood x y)
  (or (nset-circle-outline neighborhood x y)
      (and (zerop x)
           (zerop y))))

(defun nset-square (neighborhood x y)
  (nset-default neighborhood x y))

(defun nmap-square (neighborhood func)
  (nmap-default neighborhood func))

(defun nset-square-outline (neighborhood x y)
  (with-slots (minimum maximum) (extent neighborhood)
    (and
     (not (and (>= x (- minimum))
               (>= y (- minimum))
               (<= x minimum)
               (<= y minimum)))
     (nset-square neighborhood x y))))

(defun nmap-square-outline (neighborhood func)
  (let ((results)
        (min (minimum (extent neighborhood)))
        (max (maximum (extent neighborhood))))
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
  (or (and (zerop x)
           (zerop y))
      (nset-square-outline neighborhood x y)))

(defun nmap-square-outline+origin (neighborhood func)
  (cons (funcall func (origin neighborhood))
        (nmap-square-outline neighborhood func)))

(defun nset-default (neighborhood x y)
  (with-slots (maximum) (extent neighborhood)
    (and (>= x (- maximum))
         (>= y (- maximum))
         (<= x maximum)
         (<= y maximum))))

(defun nmap-default (neighborhood func)
  (let ((results)
        (max (maximum (extent neighborhood))))
    (loop :for y :from max :downto (- max)
       :do (loop :for x :from (- max) :to max
              :for cell = (nref neighborhood x y)
              :when cell
              :do (push (funcall func cell) results)))
    results))

(defmacro nmap-early-exit-reduction (neighborhood func
                                     &key (test 'when)
                                       (reduction 'none)
                                       (early-exit-continuation nil))
  (let ((block-name (gensym))
        (cell (gensym))
        (early-exit-continuation-func (gensym)))

    `(block ,block-name
       (let ((,early-exit-continuation-func ,early-exit-continuation))
         (,reduction
          (nmap ,neighborhood
                (lambda (,cell)
                  (,test (funcall ,func ,cell)
                         (return-from ,block-name
                           (cond
                             ((eq ,early-exit-continuation-func nil) nil)
                             ((eq ,early-exit-continuation-func t) t)
                             ((functionp ,early-exit-continuation-func)
                              (funcall ,early-exit-continuation-func ,cell))
                             (t (error "nmap-early-exit-reduction: :early-execute-continuation is a thing of type: '~A' and I don't know what to do with it." (type-of ,early-exit-continuation-func)))))))))))))

;; Testing code. Please leave for a while. :)
(defun display-neighborhood (n)
  (let ((max-distance (maximum (extent n))))
    (format t "  NH Display: [x=~A, y=~A] max-distance = ~A~%" (x n) (y n)
            max-distance)
    (loop :for y :from max-distance :downto (- max-distance) :do
       (format t "    ")
       ;; First draw a strip of the neighborhood
       (loop :for x :from (- max-distance) :to max-distance :do
          (format t "~:[-~;X~]" (nref n x y)))
       (format t "~%"))))

(defun display-layout (layout)
  (let ((nh (nh-realize layout
                        (make-stage 'labyrinth :width 99 :height 99) 49 49)))
    (display-neighborhood nh)))
