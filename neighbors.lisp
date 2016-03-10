(in-package :crawler2)

;; all neighborhoods are closed regions of space.

;; This is a STRUCTURE instead of a CLASS to make faster its allocation in
;; the CL runtime. This structure is used for convolutions across the stage
;; and we need to make it as efficient as possible.
(defstruct (neighborhood
             (:conc-name NIL)
             (:constructor make-neighborhood-structure))
  stage

  ;; The location in stage coordinates of the origin of this neighborhood.
  x
  y

  ;; Define a square centered around the origin which must contain the
  ;; whole of the neighborhod function defined by nh-set-fn.
  distance

  ;; The function which defines the neighborhood set in terms of the
  ;; neighborhood coordinate system. It accepts neighborhood coords
  ;; of nx and ny and return T or NIL if it is in the neighborhood
  ;; definition set.
  nh-set-fn

  ;; The map function which maps a function across the tiles
  ;; found in the neighborhood (which are also clipped by the stage).
  ;; Normally, there is a specific optimized function here for the
  ;; particular neighborhood that was created, but initially it'll have
  ;; a default one that will always work, just be unoptimized.
  nh-map-fn
  )

;; The nh's origin is at a tile and the axis all point the same way,
;; so it is an easy offset to convert nh coords into stage coords.
;; All neighborhood reference frames are identical. This also doesn't
;; clip the coord against valid stage boundaries, so the value you get
;; back could be off the stage.
(defun get-stage-coord (n nx ny)
  (values (+ (x n) nx)
          (+ (y n) ny)))

;; Call whatever neighborhood map function exists in the neighborhood
;; with the supplied func and collects the results into a list, an
;; returns it.
(defun nh-map (n func)
  (funcall (nh-map-fn n) n func))

;; Determine if nx/ny considered in the set defined by the nh-set-fn.
;; This does no clipping to the stage.
(defun in-set-p (n nx ny)
  (funcall (nh-set-fn n) n nx ny))

;; The default map function which is not optimized for any specific
;; neighborhood.  All it does is just scan the maximal region in which
;; the nh is contained and then if a tile is actually valid calls the
;; func and collects the results.  While this is deterministic, there
;; is no current requirement about the ordering of the map across the
;; neighborhood.
;; Return two values, the results of the mapping, and how many tiles were
;; examined to create the map.
(defun nh-default-map-fn (n func)
  (let ((results ())
        (num-examined 0))
    (with-accessors ((distance distance)) n
      (loop :for y :from distance :downto (- distance) :do
         (loop :for x :from (- distance) :to distance :do
            (let ((tile (nref n x y)))
              (incf num-examined)
              (when tile
                (push (funcall func tile) results))))))
    (values (nreverse results) num-examined)))

;; This is how we make a neighborhood.
(defun make-neighborhood (stage x y nh-def-func distance
                          &key (nh-map-fn #'nh-default-map-fn))
  (make-neighborhood-structure :stage stage
                               :x x
                               :y y
                               :distance distance
                               :nh-set-fn nh-def-func
                               :nh-map-fn nh-map-fn))

(defun nref (n nx ny)
  ;; If nx/ny is in the NH set, keep going.
  (when (in-set-p n nx ny)
    ;; Transform the defined nh position into a position on the stage.
    (multiple-value-bind (sx sy) (get-stage-coord n nx ny)
      ;; Clip the valid nh location against the stage.
      (when (valid-tile-p (stage n) sx sy)
        ;; The NH location resolved to a valid tile on the stage.
        ;; Return the tile found!
        (tile (stage n) sx sy)))))

;; Some accessor methods into a neighborhood.
(defun origin (n)
  (nref n 0 0))

(defun n (n &optional (distance 1))
  (nref n 0 distance))

(defun nw (n &optional (distance 1))
  (nref n (- distance) distance))

(defun w (n &optional (distance 1))
  (nref n (- distance) 0))

(defun sw (n &optional (distance 1))
  (nref n (- distance) (- distance)))

(defun s (n &optional (distance 1))
  (nref n 0 (- distance)))

(defun se (n &optional (distance 1))
  (nref n distance (- distance)))

(defun e (n &optional (distance 1))
  (nref n distance 0))

(defun ne (n &optional (distance 1))
  (nref n distance distance))


;; Initial library of neighborhood functions that mathematically
;; define neighborhood set membership. Return T is in the neighorhood and
;; NIL if not.

;; Define an ortho neighborhood.
(defun nh-def-ortho (n nx ny)
  (let ((distance (distance n)))
    (and (<= (abs nx) distance)
         (<= (abs ny) distance)
         (or (and (zerop nx) (zerop ny))
             (and (zerop nx) (not (zerop ny)))
             (and (not (zerop nx)) (zerop ny))))))

;; Implement a specialized nh-map function for ortho neighborhoods which will
;; reduce how many tiles are examined.
(defun nh-ortho-map-fn (n func)
  (let ((results ())
        (num-examined 0)
        (distance (distance n)))

    ;; compute vertical line results, include the origin
    (loop :for y :from (- distance) :to distance :do
       (let ((tile (nref n 0 y)))
         (incf num-examined)
         (when tile
           (push (funcall func tile) results))))

    ;; Then compute the left side of the horiz line, skipping the origin
    (loop :for x :from (- distance) :below 0 :do
       (let ((tile (nref n x 0)))
         (incf num-examined)
         (when tile
           (push (funcall func tile) results))))

    ;; Then compute the right side of the horz line, skipping the origin
    (loop :for x :from 1 :to distance :do
       (let ((tile (nref n x 0)))
         (incf num-examined)
         (when tile
           (push (funcall func tile) results))))

    ;; and finally return everything
    (values (nreverse results) num-examined)))


;; Define a diagonal neighborhood
(defun nh-def-diag (n nx ny)
  (let ((distance (distance n)))
    (and (<= (abs nx) distance)
         (<= (abs ny) distance)
         (= (abs nx) (abs ny)))))

;; Define a circular neighborhood
(defun nh-def-circle (n nx ny)
  (let ((distance (distance n)))
    (<= (+ (* nx nx) (* ny ny))
        (* distance distance))))

;; Define a square neighborhood
(defun nh-def-square (n nx ny)
  (let ((distance (distance n)))
    (and (>= nx (- distance))
         (>= ny (- distance))
         (<= nx distance)
         (<= ny distance))))



;; Testing codes.

(defun display-neighborhood (n)
  (with-accessors ((distance distance)) n
    (format t "  NH Display: distance = ~A~%" distance)
    (loop :for y :from distance :downto (- distance) :do
       (format t "    ")
       (loop :for x :from (- distance) :to distance :do
          (format t "~:[-~;X~]" (nref n x y)))

       ;; draw a strip of the actual stage data.
       ;; also show the real neighborhood in the stage.
       (format t "    ")
       (loop :for x :from (- distance) :to distance :do
          (let ((tile (nref n x y)))
            (format t "~A"
                    (cond
                      ((null tile) " ")
                      ((walkablep tile) ".")
                      ((not (walkablep tile)) "#")
                      (t "?")))))

       (format t "~%"))))

(defun neighborhood-test-nh-map (n)
  (format t "  Performing nh-map test...~%")
  (let ((walkablep 0)
        (non-walkablep 0))
    ;; we ignore results, since we side effect in the mapped function.
    ;; We record num-examined, which is how many tiles were looked at in
    ;; the neighborhood to determine membership.
    (multiple-value-bind (results num-examined)
        (nh-map n (lambda (tile)
                    (if (walkablep tile)
                        (incf walkablep)
                        (incf non-walkablep))))
      (declare (ignore results))
      (let* ((potential-examined (expt (1+ (* (distance n) 2)) 2))
             (examined-savings-per (- 1.0 (/ num-examined potential-examined))))

        (format t "    Found [walkablep = ~A, non-walkablep = ~A, total = ~A] tiles.~%"
                walkablep non-walkablep (+ walkablep non-walkablep))

        (format t "    Tiles actually examined during nh-map: ~A~%"
                num-examined)
        (format t "    Default nh-map function examination: ~A~%"
                potential-examined)
        (format t "    Savings: ~,2F%~%" (* examined-savings-per 100.0))))))

;; Around the tile given by x y in this call, make a neighborhood of
;; distance and then run all the test. check-distance is for checking tiles
;; at that check-distance in the compas directions. Also perform a map-nh
;; test of the nh.
(defun neighbor-tests (x y distance check-distance)
  (let ((tests `((,#'nh-def-ortho ,#'nh-ortho-map-fn "Ortho NH Test")
                 (,#'nh-def-diag ,#'nh-default-map-fn "Diag NH Test")
                 (,#'nh-def-circle ,#'nh-default-map-fn "Circle NH Test")
                 (,#'nh-def-square ,#'nh-default-map-fn "Square NH Test")))
        (dir-meths `((,#'N "N")
                     (,#'NW "NW")
                     (,#'W "W")
                     (,#'SW "SW")
                     (,#'S "S")
                     (,#'SE "SE")
                     (,#'E "E")
                     (,#'NE "NE")))
        (stage (make-stage 'labyrinth)))

    (loop :for (nh-func nh-map-fn desc) :in tests :do
       (format t "~A @ [x=~A, y=~A] Distance = ~A, Check Distance = ~A~%"
               desc x y distance check-distance)
       (let* (;; TODO, make nh-func a keyword arg of :nh-def and set it to
              ;; square, same with the mh-map-fn.
              (nh (make-neighborhood stage x y nh-func distance
                                     :nh-map-fn nh-map-fn)))

         (display-neighborhood nh)

         (format t "  Origin: ~S~%" (origin nh))

         (loop :for (dir-meth dir-desc) :in dir-meths :do
            (format t "    (~A ~A): ~S~%"
                    dir-desc check-distance
                    (funcall dir-meth nh check-distance)))

         (neighborhood-test-nh-map nh))

       (format t "~%"))

    NIL))
