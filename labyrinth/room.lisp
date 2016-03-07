(in-package :crawler2)

(defclass labyrinth-room ()
  ((x1 :reader x1
       :initarg :x1)
   (x2 :reader x2
       :initarg :x2)
   (y1 :reader y1
       :initarg :y1)
   (y2 :reader y2
       :initarg :y2)
   (width :reader width
          :initarg :w)
   (height :reader height
           :initarg :h)))

(defmethod initialize-instance :after ((object labyrinth-room) &key)
  (with-slots (x1 x2 y1 y2 width height) object
    (setf x2 (+ x1 width)
          y2 (+ y1 height))))

(defmethod estimate-rooms ((stage labyrinth))
  (with-slots (width height room-size-min room-size-max room-density) stage
    (let* ((min (expt room-size-min 2))
           (max (expt room-size-max 2))
           (average (/ (abs (- max min)) 2)))
      (floor (* (/ (* width height) average)
                (clamp room-density 0.1 1))))))

(defmethod intersectsp ((stage labyrinth) (room labyrinth-room))
  (loop :for existing in (rooms stage)
        :when (and (<= (x1 room) (x2 existing))
                   (>= (x2 room) (x1 existing))
                   (<= (y1 room) (y2 existing))
                   (>= (y2 room) (y1 existing)))
          :do (return room)))

(defmethod place-room ((stage labyrinth) (room labyrinth-room))
  (with-slots (x1 x2 y1 y2) room
    (loop :with region = (make-region stage)
          :for x :from x1 :below x2
          :do (loop :for y :from y1 :below y2
                    :for tile = (tile stage x y)
                    :do (setf (walkablep tile) t
                              (region tile) region))))
  (push room (rooms stage)))

(defmethod make-room ((stage labyrinth))
  (with-slots (width height room-size-min room-size-max) stage
    (let* ((w (rng 'odd :min room-size-min :max room-size-max))
           (h (rng 'odd :min room-size-min :max room-size-max))
           (x (rng 'odd :max (- width w)))
           (y (rng 'odd :max (- height h)))
           (room (make-instance 'labyrinth-room :x1 x :y1 y :w w :h h)))
      (if (< (/ (min w h) (max w h)) (rng 'inc))
          (make-room stage)
          (unless (intersectsp stage room)
            (place-room stage room))))))

(defmethod add-rooms ((stage labyrinth))
  (loop :with max = (estimate-rooms stage)
        :with tries = 0
        :until (or (= (length (rooms stage)) max)
                   (>= tries 1000))
        :do (make-room stage)
            (incf tries)))
