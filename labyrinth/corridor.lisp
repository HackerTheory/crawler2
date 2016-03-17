(in-package :crawler2)

(defun filter-carvable (neighborhood)
  (every #'null (nmap neighborhood #'cell-carved-p)))

(defmethod pick-cell ((stage labyrinth) cells)
  (if (> (rng 'inc) (clamp (corridor-windiness stage) 0 1))
      (rng 'elt :list cells)
      (first (last cells))))

(defun get-cell (stage cell dir scalar)
  (with-slots (x y) cell
    (let ((coords (mapcar #'+ (mapcar #'* dir `(,scalar ,scalar)) `(,x ,y))))
      (apply #'cell stage coords))))

(defun neighbors (stage cell)
  (with-slots (width height) stage
    (with-slots (x y) cell
      (remove-if
       (lambda (dir)
         (or (zerop (+ x (first dir)))
             (zerop (+ y (second dir)))
             (>= (+ x (first dir)) (1- width))
             (>= (+ y (second dir)) (1- height))
             (cell-carved-p (get-cell stage cell dir 2))))
       '((-1 0) (1 0) (0 -1) (0 1))))))

(defun carve-cell (stage cells)
  (let* ((frontier (pick-cell stage cells))
         (neighbors (neighbors stage frontier)))
    (deletef cells frontier :test #'equal)
    (when neighbors
      (loop :with dir = (rng 'elt :list neighbors)
            :for i from 1 :to 2
            :for cell = (get-cell stage frontier dir i)
            :do (setf (cell-carved-p cell) t
                      (cell-region cell) (current-region stage))
            :finally (appendf cells (list frontier (get-cell stage frontier dir 2))))))
  cells)

(defun carve (neighborhood)
  (loop :with cells = `(,(origin neighborhood))
        :while cells
        :do (setf cells (carve-cell (stage neighborhood) cells))))
