(in-package :crawler2)

(defclass labyrinth (maze)
  ((room-size-min :reader room-size-min
                  :initarg :room-size-min
                  :initform 3)
   (room-size-max :reader room-size-max
                  :initarg :room-size-max
                  :initform 11)
   (room-density :reader room-density
                 :initarg :room-density
                 :initform 0.65)
   (door-rate :reader door-rate
              :initarg :door-rate
              :initform 0.5)
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
  (with-slots (room-size-min room-size-max room-density corridor-windiness door-rate) stage
    (setf room-size-min (clamp room-size-min 3 99)
          room-size-max (clamp room-size-max room-size-min 99)
          room-density (clamp room-density 0.1 1.0)
          door-rate (clamp door-rate 0 1)
          corridor-windiness (clamp corridor-windiness 0.0 1.0))
    (when (evenp room-size-min)
      (incf room-size-min))
    (when (evenp room-size-max)
      (incf room-size-max))))

(defmethod build ((stage labyrinth))
  (format t "Creating labyrinth.~%")
  (profile-cell-calls "  Adding Rooms" (add-rooms stage))
  (profile-cell-calls "  Carving Corridors" (create-corridors stage))
  (profile-cell-calls "  Connecting Regions" (connect-regions stage))
  (profile-cell-calls "  Eroding Dead Ends" (erode-dead-ends stage))
  (profile-cell-calls "  Adding Staircases" (create-stairs stage)))
