(in-package :crawler2)

(defclass stage ()
  ((width :reader width
          :initarg :width
          :initform 49)
   (height :reader height
           :initarg :height
           :initform 49)
   (grid :reader grid)
   (seed :reader seed
         :initarg :seed
         :initform (make-seed))
   (regions :accessor regions
            :initform (make-hash-table))))

(defmethod make-grid (stage)
  (with-slots (width height grid) stage
    (setf grid (make-array `(,width ,height)))
    (dotimes (x width)
      (dotimes (y height)
        (make-cell stage x y)))))

(defmethod ensure-dimensions (stage)
  (with-slots (width height) stage
    (setf width (max 10 width)
          height (max 10 height))))

(defmethod validate :around (stage)
  (call-next-method)
  (ensure-dimensions stage))

(defmethod validate (stage))

(defmethod build (stage))

(defmethod make-stage (stage-type &rest attrs)
  (let ((stage (apply #'make-instance stage-type attrs))
        (*region* *region*))
    (make-rng stage)
    (validate stage)
    (make-grid stage)
    (build stage)
    stage))
