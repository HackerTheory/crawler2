(in-package :crawler2)

(defmethod filter-carvable (stage neighborhood)
  (nmap-early-exit-reduction neighborhood #'carvedp))

(defmethod pick-cell (stage cells)
  (if (> (rng 'inc) (corridor-windiness stage))
      (rng 'elt :list cells)
      (first cells)))

(defmethod choose-uncarved (stage neighborhood)
  (let ((results))
    (dolist (dir '(n s e w))
      (let ((c1 (funcall dir neighborhood)))
        (when (and (> (cell-x c1) 0)
                   (> (cell-y c1) 0)
                   (< (cell-x c1) (1- (width stage)))
                   (< (cell-y c1) (1- (height stage))))
          (let ((c2 (funcall dir neighborhood 2)))
            (unless (carvedp c2)
              (push (list c1 c2) results))))))
    (rng 'elt :list results)))

(defmethod carve-direction (stage origin cells)
  (with-slots (x y) origin
    (let ((neighborhood (nh-realize (layout :ortho :maximum 2) stage x y)))
      (if-let ((choice (choose-uncarved stage neighborhood)))
        (loop :for cell :in choice
              :do (carve stage cell :feature :corridor)
              :finally (push cell cells))
        (deletef cells origin)))
    cells))

(defmethod carve-corridor (stage neighborhood)
  (let ((origin (origin neighborhood)))
    (carve stage origin :region-id (make-region stage) :feature :corridor)
    (loop :with cells = (list origin)
          :while cells
          :for cell = (pick-cell stage cells)
          :do (setf cells (carve-direction stage cell cells)))))

(defmethod create-corridors (stage)
  (convolve stage (layout :square) #'filter-carvable #'carve-corridor))

(defmethod filter-dead-end (stage neighborhood)
  (let ((dirs (remove-if #'identity (nmap neighborhood #'carvedp))))
    (and (carvedp (origin neighborhood))
         (>= (length dirs) 3))))

(defmethod uncarve-dead-end (stage neighborhood)
  (uncarve stage (origin neighborhood))
  (neighborhoodp
   (nmap-early-exit-reduction
    neighborhood
    #'carvedp
    :early-exit-continuation (lambda (x) (cell-nh stage x (layout :ortho))))))

(defmethod erode-dead-ends (stage)
  (process-cells stage (layout :ortho) #'filter-dead-end #'uncarve-dead-end))
