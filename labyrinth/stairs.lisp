(in-package :crawler2)

(defmethod staircase-suitable-p ((stage labyrinth) neighborhood)
  (let ((cell (origin neighborhood)))
    (and (featuresp cell :room)
         (not (featuresp cell :junction))
         (not (adjacent-junction-p stage cell)))))

(defmethod choose-upstairs ((stage labyrinth))
  (with-slots (x1 x2 y1 y2) (rng 'elt :list (rooms stage))
    (rng 'elt :list
         (mapcar
          #'origin
          (collect-cells
           stage
           (layout :square :maximum 0)
           #'staircase-suitable-p
           :x1 x1
           :x2 (- x2 (width stage))
           :y1 y1
           :y2 (- y2 (height stage)))))))

(defmethod make-upstairs ((stage labyrinth))
  (let ((cell (choose-upstairs stage)))
    (add-feature cell :stairs-up)
    (setf (distance cell) 0)
    cell))

(defmethod choose-downstairs ((stage labyrinth) region)
  (rng 'elt :list
       (mapcar
        #'origin
        (collect-cells
         stage
         (layout :square :maximum 0)
         (lambda (s n) (and (staircase-suitable-p s n)
                       (= (region-id (origin n)) region)))))))

(defmethod make-downstairs ((stage labyrinth) source)
  (let ((queue (cl-speedy-queue:make-queue (* (width stage) (height stage))))
        (goal source))
    (cl-speedy-queue:enqueue goal queue)
    (loop :until (cl-speedy-queue:queue-empty-p queue)
          :do (loop :with current = (cl-speedy-queue:dequeue queue)
                    :with neighbors = (funcall (layout :ortho) stage (cell-x current) (cell-y current))
                    :with carved = (remove nil (nmap neighbors (lambda (x) (when (carvedp x) x))))
                    :for cell :in carved
                    :when (= (distance cell) -1)
                      :do (cl-speedy-queue:enqueue cell queue)
                          (setf (distance cell) (1+ (distance current)))
                          (when (and (staircase-suitable-p stage neighbors)
                                     (> (distance cell) (distance goal)))
                            (setf goal cell))))
    (add-feature (choose-downstairs stage (region-id goal)) :stairs-down)))

(defmethod create-stairs ((stage labyrinth))
  (let ((upstairs (make-upstairs stage)))
    (make-downstairs stage upstairs)))
