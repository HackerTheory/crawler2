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

(defmethod uncarve-dead-end-linear (stage neighborhood)
  ;; If we are called, it means the dead-end filter (using the :ortho nh)
  ;; passed.

  ;; 1. We uncarve the dead-end
  (uncarve stage (origin neighborhood))

  ;; 2. We figure out from what direction the dead end came from. Also create
  ;; the sensor for the direction we need.
  (with-accessors ((n n) (s s) (e e) (w w)) neighborhood
    (multiple-value-bind (dx dy sense)
        (cond
          ((carvedp n) (values 0 1 (layout :h-sense)))
          ((carvedp s) (values 0 -1 (layout :h-sense)))
          ((carvedp e) (values 1 0 (layout :v-sense)))
          ((carvedp w) (values -1 0 (layout :v-sense)))
          (t (error "Boom")))
      (multiple-value-bind (sx sy) (stage-coords neighborhood dx dy)
        ;; 3. sx and sy are the next carved cell we need to check. So we
        ;; start a loop in the actual stage coordinates going in the direction
        ;; we discovered.
        (loop
           :with esx = sx
           :with esy = sy
           :for current-cell = (cell stage esx esy)
           :for sense-nh = (nh-realize sense stage esx esy)
           :for sensed-cells = (nfilter sense-nh #'carvedp)
           :until sensed-cells
           :do
           ;; 4. uncarve the cell which passed the sense test.
           (uncarve stage current-cell)
           ;; 5. update to the next stage coord we're going to try.
           (incf esx dx)
           (incf esy dy)
           ;; 6. move the sensing nh to that position on the stage.
           (setf (x sense-nh) esx
                 (y sense-nh) esy)

           :finally
           ;; 7. If we sensed something, we have to stop and return
           ;; the cell to have it checked as being a new possible
           ;; dead-end or not on the next iteration of PROCESS.
           (return current-cell))))))

(defmethod erode-dead-ends (stage)
  (process stage nil #'filter-dead-end #'uncarve-dead-end-linear
           :items (dead-ends stage)
           :nh-generator (lambda (cell)
                           (cell-nh stage cell (layout :orthogonal)))))
