(in-package :crawler2)

;; Names for axis dimensions (the columns) in the extent ranges array
;; WARNING: Names can overlap in integer depending upon context of use.
;;
;; GROUP 1
;; Define 1 min/max pair which is true and the same for all required dimensions.
;; Used for squares, circles, etc
(defparameter +nd-extent+ 0)
;;
;; GROUP 2
;; Define two axis for things which require different ranges in 2 dimensions.
;; Used for ellipses, rectangles, etc
(defparameter +width+ 0)
(defparameter +height+ 1)

(defstruct (neighborhood (:conc-name nil)
                         (:constructor %make-neighborhood))
  stage
  x
  y
  (extent (make-extent))
  (set-fn #'nset-default)
  (map-fn #'nmap-default))

(defstruct (extent (:conc-name nil)
                   (:constructor %make-extent))

  ;; This would normally be represented as a 2d array of n columns
  ;; representing the extents for each axis in n-dimensions, but it is
  ;; too slow to allocate and initialize that array since millions of
  ;; layouts can be created during convolves, so we hard code in a
  ;; limit of two dimensions here. It is the quest for speed that
  ;; contorted this code. We preserve the array interface in AXIS-MAX
  ;; and AXIS-MIN.
  (min-a0 0) ;; axis 0
  (max-a0 1)

  (min-a1 0) ;; axis 1
  (max-a1 1))

(defun axis-max (extent axis-id)
  (cond
    ((or (= axis-id +nd-extent+) (= axis-id +width+))
     (max-a0 extent))
    ((= axis-id +height+)
     (max-a1 extent))
    (t (error "axis-max out of bounds: axis-id = ~A~%" axis-id))))

(defun axis-min (extent axis-id)
  (cond
    ((or (= axis-id +nd-extent+) (= axis-id +width+))
     (min-a0 extent))
    ((= axis-id +height+)
     (min-a1 extent))
    (t (error "axis-min out of bounds: axis-id = ~A~%" axis-id))))

(defun axis-range (extent dim)
  (values (axis-min extent dim) (axis-max extent dim)))

;; This function does no error checking on mins and maxs, don't get it wrong.
;; They must be the same length. Later we can support fabricating unsupplied
;; information with good defaults.

(defun make-extent (&key (mins '(0)) (maxs '(1)))
  (%make-extent :min-a0 (or (first mins) 0)
                :max-a0 (or (first maxs) 1)
                :min-a1 (or (second mins) 0)
                :max-a1 (or (second maxs) 1)))

(defun stage-coords (neighborhood nx ny)
  (with-slots (x y) neighborhood
    (values (+ x nx) (+ y ny))))

(defun neighborhoodp (neighborhood)
  (when (eq (type-of neighborhood) 'neighborhood)
    neighborhood))

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
                        ;; TODO: FIX ME
                        :extent (apply #'make-extent extent-args)
                        :set-fn set-fn
                        :map-fn map-fn)))

(defun cell-nh (stage cell layout)
  (with-slots (x y) cell
    (funcall layout stage x y)))

(defmethod layout ((name (eql :h-sense)) &rest extent-args)
  (make-neighborhood #'nset-h-sense #'nmap-h-sense extent-args))

(defmethod layout ((name (eql :v-sense)) &rest extent-args)
  (make-neighborhood #'nset-v-sense #'nmap-v-sense extent-args))

(defmethod layout ((name (eql :orthogonal)) &rest extent-args)
  (make-neighborhood #'nset-orthogonal #'nmap-orthogonal extent-args))

(defmethod layout ((name (eql :diagonal)) &rest extent-args)
  (make-neighborhood #'nset-diagonal #'nmap-diagonal extent-args))

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

(defun nset-h-sense (neighborhood x y)
  (declare (ignore y))
  (and (<= (abs x) (axis-max (extent neighborhood) +nd-extent+))
       (not (zerop x))))

(defun nmap-h-sense (neighborhood func)
  (let ((results)
        (max (axis-max (extent neighborhood) +nd-extent+)))
    (loop :for x :from (- max) :below 0
       :for cell = (nref neighborhood x 0)
       :when cell
       :do (push (funcall func cell) results))
    (loop :for x :from 1 :to max
       :for cell = (nref neighborhood x 0)
       :when cell
       :do (push (funcall func cell) results))
    results))

(defun nset-v-sense (neighborhood x y)
  (declare (ignore x))
  (and (<= (abs y) (axis-max (extent neighborhood) +nd-extent+))
       (not (zerop y))))

(defun nmap-v-sense (neighborhood func)
  (let ((results)
        (max (axis-max (extent neighborhood) +nd-extent+)))
    (loop :for y :from (- max) :below 0
       :for cell = (nref neighborhood 0 y)
       :when cell
       :do (push (funcall func cell) results))
    (loop :for y :from 1 :to max
       :for cell = (nref neighborhood 0 y)
       :when cell
       :do (push (funcall func cell) results))
    results))

(defun nset-orthogonal (neighborhood x y)
  (let ((maximum (axis-max (extent neighborhood) +nd-extent+)))
    (and (<= (abs x) maximum)
         (<= (abs y) maximum)
         (or (and (zerop x) (zerop y))
             (and (zerop x) (not (zerop y)))
             (and (not (zerop x)) (zerop y))))))

(defun nmap-orthogonal (neighborhood func)
  (let ((results)
        (max (axis-max (extent neighborhood) +nd-extent+)))
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

(defun nset-diagonal (neighborhood x y)
  (let ((maximum (axis-max (extent neighborhood) +nd-extent+)))
    (and (<= (abs x) maximum)
         (<= (abs y) maximum)
         (= (abs x) (abs y)))))

(defun nmap-diagonal (neighborhood func)
  (let ((results)
        (max (axis-max (extent neighborhood) +nd-extent+)))
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
  (let ((maximum (axis-max (extent neighborhood) +nd-extent+)))
    (<= (+ (* x x) (* y y))
        (* maximum maximum))))

(defun nset-circle-outline (neighborhood x y)
  (let ((minimum (axis-min (extent neighborhood) +nd-extent+)))
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
  (let ((minimum (axis-min (extent neighborhood) +nd-extent+)))
    (and
     (not (and (>= x (- minimum))
               (>= y (- minimum))
               (<= x minimum)
               (<= y minimum)))
     (nset-square neighborhood x y))))

(defun nmap-square-outline (neighborhood func)
  (let ((results))
    (multiple-value-bind (min max)
        (axis-range (extent neighborhood) +nd-extent+)
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
      results)))

(defun nset-square-outline+origin (neighborhood x y)
  (or (and (zerop x)
           (zerop y))
      (nset-square-outline neighborhood x y)))

(defun nmap-square-outline+origin (neighborhood func)
  (cons (funcall func (origin neighborhood))
        (nmap-square-outline neighborhood func)))

(defun nset-default (neighborhood x y)
  (let ((maximum (axis-max (extent neighborhood) +nd-extent+)))
    (and (>= x (- maximum))
         (>= y (- maximum))
         (<= x maximum)
         (<= y maximum))))

(defun nmap-default (neighborhood func)
  (let ((results)
        (max (axis-max (extent neighborhood) +nd-extent+)))
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

(defun collect (stage layout filter &key (x1 1) (x2 -1) (y1 1) (y2 -1))
  (let ((items))
    (convolve
     stage
     layout
     filter
     (lambda (s n)
       (declare (ignore s))
       (push n items))
     :x1 x1
     :x2 x2
     :y1 y1
     :y2 y2)
    items))

(defun process (stage layout filter processor &key items (nh-generator #'identity))
  (loop :with items = (or items (collect stage layout filter))
     :while items
     :do (loop :with neighborhood = (funcall nh-generator (pop items))
            :while (funcall filter stage neighborhood)
            :for new = (funcall processor stage neighborhood)
            :when new
            :do (push new items))))

;; Testing code. Please leave for a while. :)
(defun display-neighborhood (n)
  (let ((max-distance (axis-max (extent n) +nd-extent+)))
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
