(in-package :crawler2)

(defclass neighborhood ()
  ((change-coords :reader change-coords
                  :initarg :change-coords)
   (in-neighborhood-pred :reader in-neighborhood-pred
                         :initarg :in-neighborhood-pred)
   (on-stage-pred :reader on-stage-pred
                  :initarg :on-stage-pred)))

(defmethod in-stage-p (stage x y)
  (with-slots (height width) stage
    (and (not (minusp x))
         (< x width)
         (not (minusp y))
         (< y height))))

(defmethod neighborhood-ortho (stage tile distance)
  (with-slots (width height) stage
    (with-slots (x y) tile
      (labels ((change-coords (nx ny)
                 (values (+ x nx) (+ y ny)))
               (in-neighborhood-pred (nx ny)
                 (and (<= (abs nx) distance)
                      (<= (abs ny) distance)
                      (or (and (zerop nx) (zerop ny))
                          (and (zerop nx) (not (zerop ny)))
                          (and (not (zerop nx)) (zerop ny)))))
               (on-stage-pred (nx ny)
                 (when (in-neighborhood-pred nx ny)
                   (multiple-value-bind (sx sy)
                       (change-coords nx ny)
                     (in-stage-p stage sx sy)))))
        (make-instance 'neighborhood
                       :in-neighborhood-pred #'in-neighborhood-pred
                       :on-stage-pred #'on-stage-pred
                       :change-coords #'change-coords)))))

(defmethod neighborhood-diag (stage tile distance)
  (with-slots (width height) stage
    (with-slots (x y) tile
      (labels ((change-coords (nx ny)
                 (values (+ x nx) (+ y ny)))
               (in-neighborhood-pred (nx ny)
                 (and (<= (abs nx) distance)
                      (<= (abs ny) distance)
                      (= (abs nx) (abs ny))))
               (on-stage-pred (nx ny)
                 (when (in-neighborhood-pred nx ny)
                   (multiple-value-bind (sx sy)
                       (change-coords nx ny)
                     (in-stage-p stage sx sy)))))
        (make-instance 'neighborhood
                       :in-neighborhood-pred #'in-neighborhood-pred
                       :on-stage-pred #'on-stage-pred
                       :change-coords #'change-coords)))))

(defmethod neighborhood-circle (stage tile radius)
  (with-slots (width height) stage
    (with-slots (x y) tile
      (labels ((change-coords (nx ny)
                 (values (+ x nx) (+ y ny)))
               (in-neighborhood-pred (nx ny)
                 (<= (+ (* nx nx) (* ny ny)) (* radius radius)))
               (on-stage-pred (nx ny)
                 (when (in-neighborhood-pred nx ny)
                   (multiple-value-bind (sx sy)
                       (change-coords nx ny)
                     (in-stage-p stage sx sy)))))
        (make-instance 'neighborhood
                       :in-neighborhood-pred #'in-neighborhood-pred
                       :on-stage-pred #'on-stage-pred
                       :change-coords #'change-coords)))))

(defmethod neighborhood-square (stage tile distance)
  (with-slots (width height) stage
    (with-slots (x y) tile
      (labels ((change-coords (nx ny)
                 (values (+ x nx) (+ y ny)))
               (in-neighborhood-pred (nx ny)
                 (and (>= nx (- distance))
                      (>= ny (- distance))
                      (<= nx distance)
                      (<= ny distance)))
               (on-stage-pred (nx ny)
                 (when (in-neighborhood-pred nx ny)
                   (multiple-value-bind (sx sy)
                       (change-coords nx ny)
                     (in-stage-p stage sx sy)))))
        (make-instance 'neighborhood
                       :in-neighborhood-pred #'in-neighborhood-pred
                       :on-stage-pred #'on-stage-pred
                       :change-coords #'change-coords)))))

(defmethod nref (stage n nx ny)
  (when (funcall (on-stage-pred n) nx ny)
    (multiple-value-bind (sx sy)
        (funcall (change-coords n) nx ny)
      (tile stage sx sy))))

(defmethod origin (s n)
  (nref s n 0 0))

(defmethod n (s n &optional (distance 1))
  (nref s n 0 distance))

(defmethod nw (s n &optional (distance 1))
  (nref s n (- distance) distance))

(defmethod w (s n &optional (distance 1))
  (nref s n (- distance) 0))

(defmethod sw (s n &optional (distance 1))
  (nref s n (- distance) (- distance)))

(defmethod s (s n &optional (distance 1))
  (nref s n 0 (- distance)))

(defmethod se (s n &optional (distance 1))
  (nref s n distance (- distance)))

(defmethod e (s n &optional (distance 1))
  (nref s n distance 0))

(defmethod ne (s n &optional (distance 1))
  (nref s n distance distance))

(defun display-neighborhood (s n x-min x-max y-min y-max)
  (format t "Display: (X: ~A..~A) (Y: ~A..~A)~%" x-min x-max y-min y-max)
  (loop :for row :to (abs (- y-max y-min))
        :do (loop :for col :to (abs (- x-max x-min))
                  :for x = (+ x-min col)
                  :for y = (- y-max row)
                  :do (format t "~:[.~;X~]" (nref s n x y)))
            (format t "~%"))
  (format t "~%"))

(defun neighbor-ortho-test (x y)
  (format t "Orthogonal neighbor test.~%")
  (let* ((stage (make-stage 'labyrinth))
         (nh (neighborhood-ortho stage (tile stage x y) 5)))
    (format t "Origin: ~S~%(N 5) must be T: ~S~%(NE 1) must be NIL: ~S~%(E 5) must be T: ~S~%"
            (origin stage nh)
            (n stage nh 5)
            (ne stage nh)
            (e stage nh 5))
    (display-neighborhood stage nh -10 10 -10 10)))

(defun neighbor-diag-test (x y)
  (format t "Diagonal neighborhood test.~%")
  (let* ((stage (make-stage 'labyrinth))
         (nh (neighborhood-diag stage (tile stage x y) 5)))
    (format t "Origin: ~S~%(N 1) must be NIL: ~S~%(NE 5) must be T: ~S~%(E 1) must be NIL: ~S~%"
            (origin stage nh)
            (n stage nh)
            (ne stage nh 5)
            (e stage nh))
    (display-neighborhood stage nh -10 10 -10 10)))

(defun neighbor-circle-test (x y)
  (format t "Circle neighborhood test.~%")
  (let* ((stage (make-stage 'labyrinth))
         (nh (neighborhood-circle stage (tile stage x y) 5)))
    (format t "Origin: ~S~%(N 5) must be T: ~S~%(NE 5) must be NIL: ~S~%(E 5) must be T: ~S~%"
            (origin stage nh)
            (n stage nh 5)
            (ne stage nh 5)
            (e stage nh 5))
    (display-neighborhood stage nh -10 10 -10 10)))

(defun neighbor-square-test (x y)
  (format t "Square neighborhood test.~%")
  (let* ((stage (make-stage 'labyrinth))
         (nh (neighborhood-square stage (tile stage x y) 5)))
    (format t "Origin: ~S~%(N 5) must be T: ~S~%(NE 5) must be T: ~S~%(E 5) must be T: ~S~%"
            (origin stage nh)
            (n stage nh 5)
            (ne stage nh 5)
            (e stage nh 5))
    (display-neighborhood stage nh -10 10 -10 10)))

(defun neighbor-tests (x y)
  (neighbor-ortho-test x y)
  (neighbor-diag-test x y)
  (neighbor-circle-test x y)
  (neighbor-square-test x y))
