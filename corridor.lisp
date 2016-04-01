(in-package :crawler2)

(defmethod filter-carvable (stage nh)
  (nmap-short nh #'carvedp))

(defmethod pick-cell (stage cells)
  (let ((windiness (corridor-windiness stage)))
    (if (or (zerop windiness)
            (and (not (= windiness 1))
                 (> (rng 'inc) windiness)))
        (rng 'elt :list cells)
        (first cells))))

(defmethod choose-uncarved (stage nh)
  (let ((results))
    (dolist (dir '(n s e w))
      (let ((c1 (funcall dir nh)))
        (unless (stage-border-p stage c1)
          (let ((c2 (funcall dir nh 2)))
            (unless (carvedp c2)
              (push (vector c1 c2) results))))))
    (rng 'elt :list results)))

(defmethod carve-direction (stage origin cells)
  (let ((nh (cell-nh stage origin (layout :orthogonal :maxs '(2)))))
    (if-let ((choice (choose-uncarved stage nh)))
      (loop :for cell :across choice
            :do (carve stage cell :feature :corridor)
            :finally (return (push cell cells)))
      (progn (push origin (dead-ends stage))
             (deletef cells origin :count 1)))))

(defmethod carve-corridor (stage nh)
  (let ((origin (origin nh)))
    (push origin (dead-ends stage))
    (carve stage origin :region-id (make-region stage) :feature :corridor)
    (loop :with cells = (list origin)
          :while cells
          :for cell = (pick-cell stage cells)
          :do (setf cells (carve-direction stage cell cells)))))

(defmethod create-corridors (stage)
  (convolve stage (layout :rect) #'filter-carvable #'carve-corridor))

(defmethod filter-dead-end (stage nh)
  (let ((dirs (remove-if #'identity (nmap nh #'carvedp))))
    (and (>= (length dirs) 3)
         (carvedp (origin nh)))))

(defmethod uncarve-dead-end (stage nh)
  (uncarve stage (origin nh))
  (with-accessors ((n n) (s s) (e e) (w w)) nh
    (multiple-value-bind (dx dy layout)
        (cond ((carvedp n)
               (values 0 1 (layout :h-sense)))
              ((carvedp s)
               (values 0 -1 (layout :h-sense)))
              ((carvedp e)
               (values 1 0 (layout :v-sense)))
              ((carvedp w)
               (values -1 0 (layout :v-sense))))
      (multiple-value-bind (x y) (stage-coords nh dx dy)
        (loop :for cell = (cell stage x y)
              :for new-nh = (cell-nh stage cell layout)
              :for carved = (nfilter new-nh #'carvedp)
              :until carved
              :do (uncarve stage cell)
                  (incf x dx)
                  (incf y dy)
                  (setf (x new-nh) x
                        (y new-nh) y)
              :finally (return cell))))))

(defmethod erode-dead-ends (stage)
  (process stage nil #'filter-dead-end #'uncarve-dead-end
           :items (dead-ends stage)
           :nh-generator (lambda (cell) (cell-nh stage cell (layout :orthogonal)))))
