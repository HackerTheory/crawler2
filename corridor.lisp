(in-package :crawler2)

(defmethod filter-carvable (stage neighborhood)
  (nmap-early-exit-reduction neighborhood #'carvedp))

(defmethod pick-cell (stage cells)
  (let ((windiness (corridor-windiness stage)))
    (if (or (zerop windiness)
            (and (not (= windiness 1))
                 (> (rng 'inc) windiness)))
        (rng 'elt :list cells)
        (first cells))))

(defmethod choose-uncarved (stage neighborhood)
  (let ((results))
    (dolist (dir '(n s e w))
      (let ((c1 (funcall dir neighborhood)))
        (unless (stage-border-p stage c1)
          (let ((c2 (funcall dir neighborhood 2)))
            (unless (carvedp c2)
              (push (vector c1 c2) results))))))
    (rng 'elt :list results)))

(defmethod carve-direction (stage origin cells)
  (let ((neighborhood (cell-nh stage origin (layout :orthogonal :maxs '(2)))))
    (if-let ((choice (choose-uncarved stage neighborhood)))
      (loop :for cell :across choice
            :do (carve stage cell :feature :corridor)
            :finally (return (push cell cells)))
      (progn (push origin (dead-ends stage))
             (deletef cells origin :count 1)))))

(defmethod carve-corridor (stage neighborhood)
  (let ((origin (origin neighborhood)))
    (push origin (dead-ends stage))
    (carve stage origin :region-id (make-region stage) :feature :corridor)
    (loop :with cells = (list origin)
          :while cells
          :for cell = (pick-cell stage cells)
          :do (setf cells (carve-direction stage cell cells)))))

(defmethod create-corridors (stage)
  (convolve stage (layout :rect) #'filter-carvable #'carve-corridor))

(defmethod filter-dead-end (stage neighborhood)
  (let ((dirs (remove-if #'identity (nmap neighborhood #'carvedp))))
    (and (>= (length dirs) 3)
         (carvedp (origin neighborhood)))))

(defmethod uncarve-dead-end (stage neighborhood)
  (uncarve stage (origin neighborhood))
  (with-accessors ((n n) (s s) (e e) (w w)) neighborhood
    (multiple-value-bind (dx dy layout)
        (cond ((carvedp n)
               (values 0 1 (layout :h-sense)))
              ((carvedp s)
               (values 0 -1 (layout :h-sense)))
              ((carvedp e)
               (values 1 0 (layout :v-sense)))
              ((carvedp w)
               (values -1 0 (layout :v-sense))))
      (multiple-value-bind (x y) (stage-coords neighborhood dx dy)
        (loop :for cell = (cell stage x y)
              :for nh = (cell-nh stage cell layout)
              :for carved = (nfilter nh #'carvedp)
              :until carved
              :do (uncarve stage cell)
                  (incf x dx)
                  (incf y dy)
                  (setf (x nh) x
                        (y nh) y)
              :finally (return cell))))))

(defmethod erode-dead-ends (stage)
  (process stage nil #'filter-dead-end #'uncarve-dead-end
           :items (dead-ends stage)
           :nh-generator (lambda (cell) (cell-nh stage cell (layout :orthogonal)))))
