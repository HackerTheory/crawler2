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
                 :initform 0.5)
   (corridor-windiness :reader corridor-windiness
                       :initarg :corridor-windiness
                       :initform 0)
   (rooms :accessor rooms
          :initform nil)
   (regions :accessor regions
            :initform (make-hash-table))
   (current-region :accessor current-region
                   :initform 0)))

(defmethod ensure-stage-size ((stage labyrinth) dimension)
  (with-slots (room-size-max) stage
      (let ((size (max (+ (* room-size-max 2) 3) dimension)))
        (if (evenp size)
            (incf size)
            size))))

(defmethod ensure-room-size ((stage labyrinth) dimension &key (min 3) (max 99))
  (let ((size (clamp dimension min max)))
    (if (evenp size)
        (incf size)
        size)))

(defmethod ensure-dimensions ((stage labyrinth))
  (with-slots (width height room-size-min room-size-max) stage
    (setf room-size-min (ensure-room-size stage room-size-min)
          room-size-max (ensure-room-size stage room-size-max :min room-size-min)
          width (ensure-stage-size stage width)
          height (ensure-stage-size stage height))))

(defmethod build ((stage labyrinth))
  (add-rooms stage)
  ;; test convolution
  (convolve
   stage
   (layout :square-outline+origin)
   #'filter-carvable
   #'carve))
