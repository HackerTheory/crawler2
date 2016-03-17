(in-package :crawler2)

(defun filter-carvable (neighborhood)
  (every #'null (nmap neighborhood #'cell-carved-p)))

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
       (or (zerop (+ x (first dir)))
           (zerop (+ y (second dir)))
           (>= (+ x (first dir)) (1- width))
           (>= (+ y (second dir)) (1- height))
           (cell-carved-p (get-cell stage `(,x ,y) dir 1 t))))
     '((-1 0) (1 0) (0 -1) (0 1)))))

(defun carve-cell (stage cells)
  (let* ((frontier (pick-cell stage cells))
         (neighbors (neighbors stage (first frontier) (second frontier))))
    (deletef cells frontier :test #'equal)
    (when neighbors
      (loop :with dir = (rng 'elt :list neighbors)
            :for i :below 2
            :for cell = (get-cell stage frontier dir i t)
            :do (setf (cell-carved-p cell) t
                      (cell-region cell) (current-region stage))
            :finally (appendf cells (list frontier (get-cell stage frontier dir 1))))))
  cells)

(defun carve (neighborhood)
  (with-slots (x y) (origin neighborhood)
    (loop :with cells = `((,x ,y))
          :while cells
          :do (setf cells (carve-cell (stage neighborhood) cells)))))
