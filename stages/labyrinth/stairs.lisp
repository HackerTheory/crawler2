(in-package :crawler2)

(defmethod staircase-suitable-p ((stage labyrinth) nh)
  (let ((cell (origin nh)))
    (and (featuresp cell :room)
         (not (featuresp cell :junction))
         (not (adjacent-junction-p stage cell)))))

(defmethod choose-upstairs ((stage labyrinth))
  (with-slots (x1 x2 y1 y2) (rng 'elt :list (rooms stage))
    (rng 'elt :list
         (mapcar
          #'origin
          (collect
           stage
           (layout :rect :maxs '(0))
           #'staircase-suitable-p
           :x1 x1
           :x2 (- x2 (width stage))
           :y1 y1
           :y2 (- y2 (height stage)))))))

(defmethod make-upstairs ((stage labyrinth))
  (let ((cell (choose-upstairs stage)))
    (setf (distance cell) 0)
    cell))

(defmethod choose-downstairs ((stage labyrinth) region-id)
  (rng 'elt :list (cells (get-region stage region-id))))

(defmethod make-downstairs ((stage labyrinth) source)
  (let ((queue (make-queue (* (width stage) (height stage))))
        (goal source))
    (enqueue goal queue)
    (loop :until (queue-empty-p queue)
          :do (loop :with current = (dequeue queue)
                    :with n = (cell-nh stage current (layout :orthogonal))
                    :with carved = (nfilter n #'carvedp)
                    :for cell :in carved
                    :when (= (distance cell) -1)
                      :do (enqueue cell queue)
                          (setf (distance cell) (1+ (distance current)))
                          (when (and (> (distance cell) (distance goal))
                                     (staircase-suitable-p stage n))
                            (setf goal cell))))
    (choose-downstairs stage (region-id goal))))

(defmethod create-stairs ((stage labyrinth))
  (let* ((upstairs (make-upstairs stage))
         (downstairs (make-downstairs stage upstairs)))
    (add-feature upstairs :stairs-up)
    (add-feature downstairs :stairs-down)))
