(in-package :crawler2)

(defmethod filter-carvable ((stage labyrinth) neighborhood)
  (every #'null (nmap neighborhood #'carvedp)))

(defmethod pick-cell ((stage labyrinth) cells)
  (if (> (rng 'inc) (clamp (corridor-windiness stage) 0 1))
      (rng 'elt :list cells)
      (first (last cells))))

(defmethod choose-uncarved ((stage labyrinth) neighborhood)
  (with-slots (width height) (stage neighborhood)
    (rng 'elt :list
         (remove-if
          (lambda (dir)
            (with-slots (x y) (funcall dir neighborhood)
              (or (zerop x)
                  (zerop y)
                  (>= x (1- width))
                  (>= y (1- height))
                  (carvedp (funcall dir neighborhood 2)))))
          '(n s e w)))))

(defmethod carve-cell ((stage labyrinth) frontier cells)
  (with-slots (x y) frontier
    (when-let* ((neighborhood (funcall (layout :ortho :maximum 2) stage x y))
                (dir (choose-uncarved stage neighborhood)))
      (dotimes (i 2)
        (let ((cell (funcall dir neighborhood (1+ i))))
          (setf (carvedp cell) t
                (region-id cell) *current-region*)))
      (appendf cells (list frontier (funcall dir neighborhood 2))))
    cells))

(defmethod carve-corridor ((stage labyrinth) neighborhood)
  (loop :with cells = (list (origin neighborhood))
        :while cells
        :for cell = (pick-cell stage cells)
        :do (deletef cells cell)
            (setf cells (carve-cell stage cell cells))))
