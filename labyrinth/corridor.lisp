(in-package :crawler2)

(defun filter-carvable (neighborhood)
  (every #'null (nmap neighborhood #'walkablep)))

(defmethod pick-cell ((stage labyrinth) cells)
  (if (> (rng 'inc) (clamp (corridor-windiness stage) 0 1))
      (rng 'elt :list cells)
      (first (last cells))))

(defun get-cell (stage cell dir &optional (scalar 1) tilep)
  (let* ((scale (list (* (first dir) (1+ scalar))
                      (* (second dir) (1+ scalar))))
         (cell (list (+ (first cell) (first scale))
                     (+ (second cell) (second scale)))))
    (if tilep (apply #'cell stage cell) cell)))

(defun neighbors (stage x y)
  (with-slots (width height) stage
    (remove-if
     (lambda (dir)
       (or (< (+ x (first dir)) 1)
           (< (+ y (second dir)) 1)
           (> (+ x (first dir)) (- width 2))
           (> (+ y (second dir)) (- height 2))
           (walkablep (get-cell stage `(,x ,y) dir 1 t))))
     '((-1 0) (1 0) (0 -1) (0 1)))))

(defun carve-tile (stage cells)
  (let* ((cell (pick-cell stage cells))
         (neighbors (neighbors stage (first cell) (second cell))))
    (deletef cells cell :test #'equal)
    (when neighbors
      (loop :with dir = (rng 'elt :list neighbors)
            :with new-cell = (list cell (get-cell stage cell dir 1))
            :for i :below 2
            :for tile = (get-cell stage cell dir i t)
            :do (setf (walkablep tile) t)
                (setf (region tile) (current-region stage))
            :finally (appendf cells new-cell))))
  cells)

(defun carve (neighborhood)
  (with-slots (x y) (origin neighborhood)
    (loop :with cells = `((,x ,y))
          :while cells
          :do (setf cells (carve-tile (stage neighborhood) cells)))))
