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

(defstruct (nh (:conc-name nil)
               (:constructor %make-nh))
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

(defun stage-coords (nh nx ny)
  (with-slots (x y) nh
    (values (+ x nx) (+ y ny))))

(defun nmap (nh func &rest args)
  (apply (map-fn nh) nh func args))

(defun nsetp (nh x y)
  (funcall (set-fn nh) nh x y))

(defun nfilter (nh filter)
  (remove nil (nmap nh (lambda (x) (when (funcall filter x) x)))))

(defun nref (nh x y)
  (with-slots (stage) nh
    (when (nsetp nh x y)
      (multiple-value-bind (sx sy) (stage-coords nh x y)
        (valid-cell-p stage sx sy)))))

(defun origin (nh)
  (nref nh 0 0))

(defun n (nh &optional (distance 1))
  (nref nh 0 distance))

(defun nw (nh &optional (distance 1))
  (nref nh (- distance) distance))

(defun w (nh &optional (distance 1))
  (nref nh (- distance) 0))

(defun sw (nh &optional (distance 1))
  (nref nh (- distance) (- distance)))

(defun s (nh &optional (distance 1))
  (nref nh 0 (- distance)))

(defun se (nh &optional (distance 1))
  (nref nh distance (- distance)))

(defun e (nh &optional (distance 1))
  (nref nh distance 0))

(defun ne (nh &optional (distance 1))
  (nref nh distance distance))

(defun make-nh (set-fn map-fn extent-args)
  (lambda (stage x y)
    (%make-nh :stage stage
              :x x
              :y y
              :extent (apply #'make-extent extent-args)
              :set-fn set-fn
              :map-fn map-fn)))

(defun cell-nh (stage cell layout)
  (with-slots (x y) cell
    (funcall layout stage x y)))

(defmethod layout ((name (eql :h-sense)) &rest extent-args)
  (make-nh #'nset-h-sense #'nmap-h-sense extent-args))

(defmethod layout ((name (eql :v-sense)) &rest extent-args)
  (make-nh #'nset-v-sense #'nmap-v-sense extent-args))

(defmethod layout ((name (eql :orthogonal)) &rest extent-args)
  (make-nh #'nset-orthogonal #'nmap-orthogonal extent-args))

(defmethod layout ((name (eql :diagonal)) &rest extent-args)
  (make-nh #'nset-diagonal #'nmap-diagonal extent-args))

(defmethod layout ((name (eql :ellipse)) &rest extent-args)
  (make-nh #'nset-ellipse #'nmap-default extent-args))

(defmethod layout ((name (eql :ellipse-outline)) &rest extent-args)
  (make-nh #'nset-ellipse-outline #'nmap-default extent-args))

(defmethod layout ((name (eql :ellipse-outline+origin)) &rest extent-args)
  (make-nh #'nset-ellipse-outline+origin #'nmap-default extent-args))

(defmethod layout ((name (eql :rect)) &rest extent-args)
  (make-nh #'nset-rect #'nmap-rect extent-args))

(defmethod layout ((name (eql :rect-outline)) &rest extent-args)
  (make-nh #'nset-rect-outline #'nmap-rect-outline extent-args))

(defmethod layout ((name (eql :rect-outline+origin)) &rest extent-args)
  (make-nh #'nset-rect-outline+origin #'nmap-rect-outline+origin extent-args))

(defun nh-realize (nh-generator stage x y)
  (funcall nh-generator stage x y))

(defun nset-h-sense (nh x y)
  (and (zerop y)
       (<= (abs x) (axis-max (extent nh) +nd-extent+))
       (not (zerop x))))

(defun nmap-h-sense (nh func)
  (let ((results)
        (max (axis-max (extent nh) +nd-extent+)))
    (loop :for x :from (- max) :below 0
       :for cell = (nref nh x 0)
       :when cell
       :do (push (funcall func cell) results))
    (loop :for x :from 1 :to max
       :for cell = (nref nh x 0)
       :when cell
       :do (push (funcall func cell) results))
    results))

(defun nset-v-sense (nh x y)
  (and (zerop x)
       (<= (abs y) (axis-max (extent nh) +nd-extent+))
       (not (zerop y))))

(defun nmap-v-sense (nh func)
  (let ((results)
        (max (axis-max (extent nh) +nd-extent+)))
    (loop :for y :from (- max) :below 0
       :for cell = (nref nh 0 y)
       :when cell
       :do (push (funcall func cell) results))
    (loop :for y :from 1 :to max
       :for cell = (nref nh 0 y)
       :when cell
       :do (push (funcall func cell) results))
    results))

(defun nset-orthogonal (nh x y)
  (let ((maximum (axis-max (extent nh) +nd-extent+)))
    (and (<= (abs x) maximum)
         (<= (abs y) maximum)
         (or (and (zerop x) (zerop y))
             (and (zerop x) (not (zerop y)))
             (and (not (zerop x)) (zerop y))))))

(defun nmap-orthogonal (nh func)
  (let ((results)
        (max (axis-max (extent nh) +nd-extent+)))
    (loop :for y :from (- max) :to max
       :for cell = (nref nh 0 y)
       :when cell
       :do (push (funcall func cell) results))
    (loop :for x :from (- max) :below 0
       :for cell = (nref nh x 0)
       :when cell
       :do (push (funcall func cell) results))
    (loop :for x :from 1 :to max
       :for cell = (nref nh x 0)
       :when cell
       :do (push (funcall func cell) results))
    results))

(defun nset-diagonal (nh x y)
  (let ((maximum (axis-max (extent nh) +nd-extent+)))
    (and (<= (abs x) maximum)
         (<= (abs y) maximum)
         (= (abs x) (abs y)))))

(defun nmap-diagonal (nh func)
  (let ((results)
        (max (axis-max (extent nh) +nd-extent+)))
    (loop :for x :from (- max) :to max
       :for cell = (nref nh x (- x))
       :when cell
       :do (push (funcall func cell) results))
    (loop :for x :from (- max) :below 0
       :for cell = (nref nh x x)
       :when cell
       :do (push (funcall func cell) results))
    (loop :for x :from 1 :to max
       :for cell = (nref nh x x)
       :when cell
       :do (push (funcall func cell) results))
    results))

(defun nset-ellipse (nh x y)
  (let ((maximum (axis-max (extent nh) +nd-extent+)))
    (<= (+ (* x x) (* y y))
        (* maximum maximum))))

(defun nset-ellipse-outline (nh x y)
  (let ((minimum (axis-min (extent nh) +nd-extent+)))
    (and (nset-ellipse nh x y)
         (not (<= (+ (* x x) (* y y))
                  (* minimum minimum))))))

(defun nset-ellipse-outline+origin (nh x y)
  (or (nset-ellipse-outline nh x y)
      (and (zerop x)
           (zerop y))))

(defun nset-rect (nh x y)
  (nset-default nh x y))

(defun nmap-rect (nh func)
  (nmap-default nh func))

(defun nset-rect-outline (nh x y)
  (let ((minimum (axis-min (extent nh) +nd-extent+)))
    (and
     (not (and (>= x (- minimum))
               (>= y (- minimum))
               (<= x minimum)
               (<= y minimum)))
     (nset-rect nh x y))))

(defun nmap-rect-outline (nh func)
  (let ((results))
    (multiple-value-bind (min max)
        (axis-range (extent nh) +nd-extent+)
      (loop :for y :from min :to max
         :do (loop :for x :from (- max) :to max
                :for cell = (nref nh x y)
                :when cell
                :do (push (funcall func cell) results)))
      (loop :for y :from (- max) :to (- min)
         :do (loop :for x :from (- max) :to max
                :for cell = (nref nh x y)
                :when cell
                :do (push (funcall func cell) results)))
      (loop :for y :from (1+ (- min)) :below min
         :do (loop :for x :from (- max) :to (- min)
                :for cell = (nref nh x y)
                :when cell
                :do (push (funcall func cell) results)))
      (loop :for y :from (1+ (- min)) :below min
         :do (loop :for x :from min :to max
                :for cell = (nref nh x y)
                :when cell
                :do (push (funcall func cell) results)))
      results)))

(defun nset-rect-outline+origin (nh x y)
  (or (and (zerop x)
           (zerop y))
      (nset-rect-outline nh x y)))

(defun nmap-rect-outline+origin (nh func)
  (cons (funcall func (origin nh))
        (nmap-rect-outline nh func)))

(defun nset-default (nh x y)
  (let ((maximum (axis-max (extent nh) +nd-extent+)))
    (and (>= x (- maximum))
         (>= y (- maximum))
         (<= x maximum)
         (<= y maximum))))

(defun nmap-default (nh func)
  (let ((results)
        (max (axis-max (extent nh) +nd-extent+)))
    (loop :for y :from max :downto (- max)
       :do (loop :for x :from (- max) :to max
              :for cell = (nref nh x y)
              :when cell
              :do (push (funcall func cell) results)))
    results))

(defmacro nmap-short (nh func &key (test 'when) (reduce 'none) (return-val nil))
  (let ((block-name (gensym))
        (cell (gensym))
        (return-func (gensym)))
    `(block ,block-name
       (let ((,return-func ,return-val))
         (,reduce
          (nmap ,nh
                (lambda (,cell)
                  (,test (funcall ,func ,cell)
                         (return-from ,block-name
                           (cond
                             ((eq ,return-func nil) nil)
                             ((eq ,return-func t) t)
                             ((functionp ,return-func)
                              (funcall ,return-func ,cell))))))))))))

(defun convolve (stage layout filter effect &key (x1 1) (x2 -1) (y1 1) (y2 -1))
  (with-slots (width height) stage
    (loop :with affectedp
          :for x :from x1 :below (+ width x2)
          :do (loop :for y :from y1 :below (+ height y2)
                    :for nh = (nh-realize layout stage x y)
                    :when (funcall filter stage nh)
                      :do (let ((value (funcall effect stage nh)))
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

(defun process (stage layout filter processor &key (items nil items-p) (nh-generator #'identity))
  (loop :with items = (if items-p items (collect stage layout filter))
        :while items
        :for nh = (funcall nh-generator (pop items))
        :when (funcall filter stage nh)
          :do (when-let ((new (funcall processor stage nh)))
                (push new items))))

(defun display-nh (nh)
  (let ((max-distance (axis-max (extent nh) +nd-extent+)))
    (format t "  NH Display: [x=~A, y=~A] max-distance = ~A~%" (x nh) (y nh)
            max-distance)
    (loop :for y :from max-distance :downto (- max-distance) :do
       (format t "    ")
       (loop :for x :from (- max-distance) :to max-distance :do
          (format t "~:[-~;X~]" (nref nh x y)))
       (format t "~%"))))

(defun display-layout (layout)
  (let ((nh (nh-realize layout (make-stage 'labyrinth :width 99 :height 99) 49 49)))
    (display-nh nh)))
