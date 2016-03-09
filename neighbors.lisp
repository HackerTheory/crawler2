(in-package :crawler2)

;; all neighborhoods are closed regions of space.

(defclass neighborhood ()
  ((stage :reader stage
          :initarg :stage)

   ;; The tile around which this neighborhood exists.
   ;; TODO: However, I actually don't think this needs to be here.
   ;; Maybe MAKE-NEIGHBORHOOD below should get an x y insted of a TILE
   ;; and then we don't need to perform ANY tile lookups pre-emptively
   ;; until we actually need to. Then, neighborhoods are effectively FREE
   ;; unless you _actually_ look something up in them.
   (origin-tile :reader origin-tile
                :initarg :origin-tile)

   ;; Define a square centered around the origin which must contain the
   ;; whole of the neighborhod function defined by nh-set-fn.
   (distance :reader distance
             :initarg :distance)

   ;; The function which defines the neighborhood set in terms of the
   ;; neighborhood coordinate system. It accepts neighborhood coords
   ;; of nx and ny and return T or NIL if it is in the neighborhood
   ;; definition set.
   (nh-set-fn :reader nh-set-fn
              :initarg :nh-set-fn)

   ;; The map function which maps a function across the tiles
   ;; found in the neighborhood (which are also clipped by the stage).
   ;; Normally, there is a specific optimized function here for the
   ;; particular neighborhood that was created, but initially it'll have
   ;; a default one that will always work, just be unoptimized.
   (nh-map-fn :reader nh-map-fn
              :initarg :nh-map-fn))

  )

;; The nh's origin is at the tile and the axis all point the same way,
;; so it is an easy offset to convert nh coords into stage coords.
;; All neighborhood reference frames are identical.
(defmethod change-coord ((n neighborhood) nx ny)
  (let ((ot (origin-tile n)))
    (values (+ (x ot) nx)
            (+ (y ot) ny))))

;; Call whatever neighborhood map function exists in the neighborhood
;; with the supplied func and collects the results into a list, an
;; returns it.
(defmethod map-nh ((n neighborhood) func)
  (funcall (nh-map-fn n) n func))

;; The default map function which is not optimized for any specific
;; neighborhood.  All it does is just scan the maximal region in which
;; the nh is contained and then if a tile is actually valid calls the
;; func and collects the results.  While this is deterministic, there
;; is no current requirement about the ordering of the map across the
;; neighborhood.
(defun nh-default-map-fn (n func)
  (let ((results ()))
    (with-slots (distance) n
      (loop :for y :from distance :downto (- distance) :do
         (loop :for x :from (- distance) :to distance :do
            (let ((tile (nref n x y)))
              (when tile
                (push (funcall func tile) results))))))
    (nreverse results)))

;; This is how we make a neighborhood.
(defun make-neighborhood (stage tile make-nh-def-func distance
                          &key (map-fn #'nh-default-map-fn))
  (make-instance 'neighborhood
                 :stage stage
                 :origin-tile tile
                 :distance distance
                 :nh-set-fn (funcall make-nh-def-func distance)
                 :nh-map-fn map-fn))

(defmethod nref ((n neighborhood) nx ny)
  ;; First we see if nx/ny is in the defined nh set for this neighborhood
  (let ((in-nh-set-p (funcall (nh-set-fn n) nx ny)))
    ;; If so, we keep going and do more work to find it.
    (when in-nh-set-p
      ;; Transform the defined nh position into a position on the stage.
      (multiple-value-bind (sx sy) (change-coord n nx ny)
        ;; Clip the valid nh location against the stage.
        (when (valid-tile-p (stage n) sx sy)
          ;; The NH location resolved to a valid tile on the stage.
          ;; Return the tile found!
          (tile (stage n) sx sy))))))

;; Some accessor methods into a neighborhood.
(defmethod origin (n)
  (nref n 0 0))

(defmethod n (n &optional (distance 1))
  (nref n 0 distance))

(defmethod nw (n &optional (distance 1))
  (nref n (- distance) distance))

(defmethod w (n &optional (distance 1))
  (nref n (- distance) 0))

(defmethod sw (n &optional (distance 1))
  (nref n (- distance) (- distance)))

(defmethod s (n &optional (distance 1))
  (nref n 0 (- distance)))

(defmethod se (n &optional (distance 1))
  (nref n distance (- distance)))

(defmethod e (n &optional (distance 1))
  (nref n distance 0))

(defmethod ne (n &optional (distance 1))
  (nref n distance distance))


;; Initial library of functions that define neighborhood set
;; definition functions. :) Distance defines a closed square that
;; contains the nh.
(defun make-nh-def-ortho (distance)
  (lambda (nx ny)
    (and (<= (abs nx) distance)
         (<= (abs ny) distance)
         (or (and (zerop nx) (zerop ny))
             (and (zerop nx) (not (zerop ny)))
             (and (not (zerop nx)) (zerop ny))))))

(defun make-nh-def-diag (distance)
  (lambda (nx ny)
    (and (<= (abs nx) distance)
         (<= (abs ny) distance)
         (= (abs nx) (abs ny)))))

(defun make-nh-def-circle (distance)
  (lambda (nx ny)
    (<= (+ (* nx nx) (* ny ny))
        (* distance distance))))

(defun make-nh-def-square (distance)
  (lambda (nx ny)
    (and (>= nx (- distance))
         (>= ny (- distance))
         (<= nx distance)
         (<= ny distance))))





;; Testing codes.

(defun display-neighborhood (n)
  (with-slots (distance) n
    (format t "  NH Display: distance = ~A~%" distance)
    (loop :for y :from distance :downto (- distance) :do
       (format t "    ")
       (loop :for x :from (- distance) :to distance :do
          (format t "~:[.~;X~]" (nref n x y)))
       (format t "~%"))))

(defun neighborhood-test-map-nh (n)
  (format t "  Performing map-nh test...")
  (let ((sum 0))
    (map-nh n (lambda (tile)
                (declare (ignore tile))
                (incf sum)))
    (format t "Found ~A tiles!~%" sum)))

;; Around the tile given by x y in this call, make a neighborhood of
;; distance and then run all the test. check-distance is for checking tiles
;; at that check-distance in the compas directions. Also perform a map-nh
;; test of the nh.
(defun neighbor-tests (x y distance check-distance)
  (let ((tests `((,#'make-nh-def-ortho "Ortho NH Test")
                 (,#'make-nh-def-diag "Diag NH Test")
                 (,#'make-nh-def-circle "Circle NH Test")
                 (,#'make-nh-def-square "Square NH Test")))
        (dir-meths `((,#'N "N")
                     (,#'NW "NW")
                     (,#'W "W")
                     (,#'SW "SW")
                     (,#'S "S")
                     (,#'SE "SE")
                     (,#'E "E")
                     (,#'NE "NE"))))

    (loop :for (nh-func desc) in tests :do
       (format t "~A @ [x=~A, y=~A] Distance = ~A, Check Distance = ~A~%"
               desc x y distance check-distance)
       (let* ((stage (make-stage 'labyrinth))
              (nh (make-neighborhood
                   ;; SEE TODO in defclass for neighborhood.
                   stage (tile stage x y) nh-func distance)))
         (display-neighborhood nh)
         (format t "  Origin: ~S~%" (origin nh))
         (loop :for (dir-meth dir-desc) :in dir-meths :do
            (format t "    (~A ~A): ~S~%"
                    dir-desc check-distance
                    (funcall dir-meth nh check-distance)))
         (neighborhood-test-map-fn nh))
       (format t "~%"))

    NIL))
