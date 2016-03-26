(in-package :crawler2)

(defmethod filter-carvable ((stage labyrinth) neighborhood)
  (all-nil (nmap neighborhood #'carvedp)))

(defmethod pick-cell ((stage labyrinth) cells)
  (if (> (rng 'inc) (corridor-windiness stage))
      (rng 'elt :list cells)
      (first cells)))

(defmethod choose-uncarved ((stage labyrinth) neighborhood)
  (with-slots (width height) (stage neighborhood)
    (rng 'elt :list
         (remove-if
          (lambda (dir)
            ;; TODO: rewrite to use nref with the neighborhood, then the
            ;; clipping checks can go away.
            (with-slots (x y) (funcall dir neighborhood)
              (or (zerop x)
                  (zerop y)
                  (>= x (1- width))
                  (>= y (1- height))
                  (carvedp (funcall dir neighborhood 2)))))
          '(n s e w)))))

(defmethod carve-cell ((stage labyrinth) frontier cells)
  (with-slots (x y) frontier
    (let ((neighborhood (nh-realize (layout :ortho :maximum 2) stage x y)))
      (if-let ((dir (choose-uncarved stage neighborhood)))
          (loop :for i :from 1 :to 2
                :for cell = (funcall dir neighborhood i)
                :do (setf (carvedp cell) t
                          (region-id cell) (current-region stage))
                :finally (push cell cells))
          (deletef cells frontier)))
    cells))

(defmethod carve-corridor ((stage labyrinth) neighborhood)
  (let ((origin (origin neighborhood)))
    (setf (region-id origin) (make-region stage)
          (carvedp origin) t)
    (loop :with cells = (list origin)
          :while cells
          :for cell = (pick-cell stage cells)
          :do (setf cells (carve-cell stage cell cells)))))

(defmethod filter-dead-end ((stage labyrinth) neighborhood)
  (let ((dirs (remove-if #'identity (nmap neighborhood #'carvedp))))
    (and (carvedp (origin neighborhood))
         (>= (length dirs) 3))))

(defmethod erode-dead-end ((stage labyrinth) neighborhood)
  (with-slots (carvedp region-id) (origin neighborhood)
    (setf carvedp nil
          region-id nil)
    (when-let* ((dir (first (remove nil (nmap neighborhood (lambda (x) (when (carvedp x) x))))))
                (nh (nh-realize
                     (layout :ortho) stage (cell-x dir) (cell-y dir))))
      (when (filter-dead-end stage nh)
        nh))))
