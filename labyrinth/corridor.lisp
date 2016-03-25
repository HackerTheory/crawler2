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
              (or (<= x 1)
                  (<= y 1)
                  (>= x (1- width))
                  (>= y (1- height))
                  (carvedp (funcall dir neighborhood 2)))))
          '(n s e w)))))

(defmethod carve-cell ((stage labyrinth) frontier cells)
  (with-slots (x y) frontier
    (when-let* ((neighborhood (nh-realize (layout :ortho :maximum 2) stage x y))
                (dir (choose-uncarved stage neighborhood)))
      (dotimes (i 2)
        (with-slots (carvedp region-id) (funcall dir neighborhood (1+ i))
          (setf carvedp t
                region-id (current-region stage))))
      (push frontier cells)
      (push (funcall dir neighborhood 2) cells))
    cells))

(defmethod carve-corridor ((stage labyrinth) neighborhood)
  (with-accessors ((origin origin)) neighborhood
    (setf (region-id origin) (make-region stage)
          (carvedp origin) t)
    (loop :with cells = (list origin)
          :while cells
          :for cell = (pick-cell stage cells)
          :do (deletef cells cell)
              (setf cells (carve-cell stage cell cells)))))

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
