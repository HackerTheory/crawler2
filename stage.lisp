(in-package :crawler2)

(defclass stage ()
  ((width :reader width
          :initarg :width
          :initform 49)
   (height :reader height
           :initarg :height
           :initform 49)
   (buffers :reader buffers
            :initform (list 0))
   (tiles :accessor tiles
          :initarg :tiles)))

(defmethod ensure-dimensions (stage)
  (with-slots (width height) stage
    (setf width (max 10 width)
          height (max 10 height))))

(defmethod build (stage))

(defmethod make-stage (stage-type &rest attrs)
  (let ((stage (apply #'make-instance stage-type attrs)))
    (ensure-dimensions stage)
    (make-buffers stage)
    (build stage)
    stage))
