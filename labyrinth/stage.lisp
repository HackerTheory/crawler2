(in-package :crawler2)

(defclass labyrinth (stage)
  ((room-size-min :reader room-size-min
                  :initarg :room-size-min
                  :initform 3)
   (room-size-max :reader room-size-max
                  :initarg :room-size-max
                  :initform 11)
   (room-density :reader room-density
                 :initarg :room-density
                 :initform 0.65)
   (corridor-windiness :reader corridor-windiness
                       :initarg :corridor-windiness
                       :initform 0)
   (rooms :accessor rooms
          :initform nil)))

(defmethod ensure-dimensions ((stage labyrinth))
  (with-slots (width height room-size-min room-size-max) stage
    (flet ((ensure (dimension)
             (let ((size (max (+ (* room-size-max 2) 3) dimension)))
               (if (evenp size)
                   (incf size)
                   size))))
      (setf width (ensure width)
            height (ensure height)))))

(defmethod validate ((stage labyrinth))
  (with-slots (room-size-min room-size-max room-density corridor-windiness) stage
    (setf room-size-min (clamp room-size-min 3 99)
          room-size-max (clamp room-size-max room-size-min 99)
          room-density (clamp room-density 0.1 1.0)
          corridor-windiness (clamp corridor-windiness 0.0 1.0))
    (when (evenp room-size-min)
      (incf room-size-min))
    (when (evenp room-size-max)
      (incf room-size-max))))

(defmethod build ((stage labyrinth))
  (add-rooms stage)
  (convolve stage (layout :square) #'filter-carvable #'carve-corridor)
  (convolve stage (layout :ortho) #'filter-connectable #'connect)
  (create-junctions stage)
  (process-cells stage (layout :ortho) #'filter-dead-end #'erode-dead-end))
