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
                      (or (and (zerop nx))
                          (not (zerop ny))
                          (and (not (zerop nx))
                               (zerop ny)))))
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

(defmethod s (s n &optional (distance 1))
  (nref s n 0 (- distance)))

(defmethod e (s n &optional (distance 1))
  (nref s n distance 0))

(defmethod w (s n &optional (distance 1))
  (nref s n (- distance) 0))

(defun neighbor-test (x y)
  (let* ((stage (make-stage 'labyrinth))
         (nh (neighborhood-ortho stage (tile stage x y) 5)))
    (format t "Origin: ~S~%N:~S~%S:~S~%E:~S~%W:~S~%~%"
            (origin stage nh)
            (n stage nh)
            (s stage nh)
            (e stage nh)
            (w stage nh))))
