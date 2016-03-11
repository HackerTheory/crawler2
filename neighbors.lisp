(in-package :crawler2)

;;;; Neighborhoods are mathematical objects which represent a finite
;;;; and closed area of cells around a particular point on the
;;;; stage. The neighborhood exists in its own coordinate space and
;;;; the origin of the neighborhood coordinate system is at the (x0,
;;;; x1, ..., xN) location of the cell on the stage. The axes of the
;;;; neighborhood align in direction with the stage coordinate system.
;;;;
;;;; There is a function, called a NH-SET-FN function, which defines
;;;; an equation which indicates set membership in the
;;;; neighborhood. Another function, called a NH-MAP-FN function, maps
;;;; an arbitrary function across the cells which are a member of the
;;;; neighborhood as defined by the NH-SET-FN function.  The NH-SET-FN
;;;; function can define things like squares, circles, polygons,
;;;; curves, outlines of squares, or any other set of cells that are
;;;; considered the neighborhood. The order of mapping that an NH-MAP-FN
;;;; function performs is not defined.
;;;;
;;;; Since neighborhoods may be defined to be larger than the
;;;; boundaries of the stage (example: a 7x7 square neighborhood at
;;;; stage location 2,2), the neighborhoods are clipped in terms of
;;;; NH-REF. If you are manually converting neighborhood coordinates to
;;;; stage coordinates, don't forget to pass those stage coordinates
;;;; through VALID-CELL-P to ensure that you will be accessing a valid
;;;; cell on the stage.
;;;;
;;;; Extents are structures which help define the neighborhood
;;;; regions.  you must ALWAYS initialize MAX-DISTANCE when creating
;;;; an EXTENT because that represents the full size of the square
;;;; centered at the origin of the neighborhood which must contain the
;;;; entire neighborhood set. Extents are sort of a hodgepodge of
;;;; information used by different NH-SET-FN and NH-MAP-FN functions
;;;; to define the valid region that NH-SET-FN considers to be in the
;;;; neighborhood.
;;;;
;;;; A set of helper functions allow curried building of
;;;; neighborhoods. These are the NH-GEN functions. These return a
;;;; function which when invoked with the rest of the data needed
;;;; complete the construction of a neighborhood. They are useful in
;;;; situations when you want to describe the properties of a
;;;; neighborhood but not, at that point in the code, want to declare
;;;; exactly where it is on which stage.
;;;;
;;;; There are specific accessors for well known places on the
;;;; neighborhood such as NH-ORIGIN, NH-N, NH-NW, NH-W, NH-SW, NH-S,
;;;; NH-SE, NH-E, NH-NE.  They may be passed distances to offset the
;;;; cell looked up in that direction. NH-REF is a general accessor
;;;; which will look up a cell in the neighborhood's coordinate system
;;;; and return NIL if there is no cell there (due to clipping or that
;;;; location not being in the NH-SET-FN membership) or the cell
;;;; refernce otherwise.  NOTE: Neighborhoods only perform their work
;;;; when NH-REF is called on it.  Hence, no cell lookups are performed
;;;; through the neighborhood API on the stage unless NH-REF is called.
;;;;
;;;; Neighborhood are created with MAKE-NH as per the structure slot
;;;; initializers. :stage, :x, and :y are the minimum number of init
;;;; args one needs and the SET-FN will result in a square neighorhood 1
;;;; unit around the origin and including the origin.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The EXTENT structure.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A union of various ways to specify ranges, how big something is,
;; bounds on things, thicknesses of lines, context of such, etc, etc,
;; etc. Used by the neighborhood code for various nh-sets. Currently
;; only max-distance is in here, but as soon as we want a sqaure outline
;; of a neighborhood where you can control the thickness of the line and
;; such, we'll need more slots in this structure.
(defstruct ext
  ;; MAX-DISTANCE should ALWAYS be defined since it represents the farthest
  ;; distance from the origin which still contains the neighborhood. This is
  ;; used to define a symmetric square around the neighborhood that MUST
  ;; enclose it completely.
  max-distance
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The NH structure.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a STRUCTURE instead of a CLASS to make faster its allocation in
;; the CL runtime. This structure is used for convolutions across the stage
;; and we need to make it as efficient as possible.
(defstruct nh
  ;; For what stage was this neighborhood created?
  stage

  ;; The location in stage coordinates of the origin of this neighborhood.
  x
  y

  ;; Describe various properties about where the set-fn is valid.
  ;; We initially define a 1 unit square which includes the origin.
  (ext (make-ext :max-distance 1))

  ;; Function that mathematically describes the set. Returns T if coord is
  ;; a member of the set, NIL otherwise.
  (set-fn #'nh-set-fn-default)

  ;; Function that will map another function across the cells which are in
  ;; the neighborhood and actually on the stage.
  (map-fn #'nh-map-fn-default)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neighborhood API methods
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert an NH coordinate to the stage coordinate system, but do no
;; clipping to the stage boundaries.
(defun nh-to-stage-coord (n nx ny)
  (values (+ (nh-x n) nx)
          (+ (nh-y n) ny)))

;; A nicer interface to mapping a function across the valid cells in a
;; neighborhood.
(defun nh-map (n func)
  (funcall (nh-map-fn n) n func))

;; Determine if nx/ny considered in the set defined by the nh-set-fn.
;; This does no clipping to the stage.
(defun nh-in-set-p (n nx ny)
  (funcall (nh-set-fn n) n nx ny))

;; The neighborhood reference function, similar to aref. Returns NIL if
;; the neghborhood coordinate results in a position which is not defined to
;; be in the set or is off the stage. Return a cell reference if NX and NY
;; are both in the defined set of the neighborhood and on the stage.
(defun nh-ref (n nx ny)
  ;; If nx/ny is in the NH set, keep going.
  (when (nh-in-set-p n nx ny)
    ;; Transform the defined nh position into a position on the stage.
    (multiple-value-bind (sx sy) (nh-to-stage-coord n nx ny)
      ;; Clip the valid nh location against the stage.
      (when (valid-cell-p (nh-stage n) sx sy)
        ;; The NH location resolved to a valid cell on the stage.
        ;; Return the cell found!
        (cell (nh-stage n) sx sy)))))

;; A pile of well named places to inspect in the neighborhood.

(defun nh-origin (n)
  (nh-ref n 0 0))

(defun nh-n (n &optional (distance 1))
  (nh-ref n 0 distance))

(defun nh-nw (n &optional (distance 1))
  (nh-ref n (- distance) distance))

(defun nh-w (n &optional (distance 1))
  (nh-ref n (- distance) 0))

(defun nh-sw (n &optional (distance 1))
  (nh-ref n (- distance) (- distance)))

(defun nh-s (n &optional (distance 1))
  (nh-ref n 0 (- distance)))

(defun nh-se (n &optional (distance 1))
  (nh-ref n distance (- distance)))

(defun nh-e (n &optional (distance 1))
  (nh-ref n distance 0))

(defun nh-ne (n &optional (distance 1))
  (nh-ref n distance distance))

;; Next are NH-GEN curried neighborhood generation functions which
;; allow fixation of the extent and kinf of neighborhood and return a
;; fuunction that can be called later with the stage desired and
;; location of the neighborhood to complete the generation of the
;; neighborhood with all of those parameters at a later date.
;;
;; These are usually used in a context like this where you want to
;; talk about the shape and size of a neighborhood without making the
;; whole object at the call site.
;;
;; (convolve stage (nh-gen-ortho 1) filter-fn effect-fn)

;; A generic neighborhood generator.
(defun nh-gen (set-fn map-fn ext)
  (lambda (stage x y)
    (make-nh :stage stage
             :x x
             :y y
             :ext ext
             :set-fn set-fn
             :map-fn map-fn)))

;; A specifc generator for ortho neighborhoods.
(defun nh-gen-ortho (max-distance)
  (lambda (stage x y)
    (make-nh :stage stage
             :x x
             :y y
             :ext (make-ext :max-distance max-distance)
             :set-fn #'nh-set-fn-ortho
             :map-fn #'nh-map-fn-ortho)))

;; A specific generator for diag neighborhoods.
(defun nh-gen-diag (max-distance)
  (lambda (stage x y)
    (make-nh :stage stage
             :x x
             :y y
             :ext (make-ext :max-distance max-distance)
             :set-fn #'nh-set-fn-diag
             :map-fn #'nh-map-fn-diag)))

;; A specific generator for circle neighborhoods.
(defun nh-gen-circle (max-distance)
  (lambda (stage x y)
    (make-nh :stage stage
             :x x
             :y y
             :ext (make-ext :max-distance max-distance)
             :set-fn #'nh-set-fn-circle
             :map-fn #'nh-map-fn-default)))

;; A specific generator for quare neighborhoods.
(defun nh-gen-square (max-distance)
  (lambda (stage x y)
    (make-nh :stage stage
             :x x
             :y y
             :ext (make-ext :max-distance max-distance)
             :set-fn #'nh-set-fn-square
             :map-fn #'nh-map-fn-square)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A library of NH-SET-FN and NH-MAP-FN functions.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is an initial library of functions used to define different kinds
;; of neighborhood sets and their higher order mapping functions. The
;; NH-MAP-HN functions return two values, the list of results, and a number
;; which indicates how many cells were actually examined. MAP-FN functions
;; are carefully designed to only examine exactly the number of cells
;; necessary to perform their work.

;; ;;;;;;;;;;;;
;; ORTHO neighborhoods
;; ;;;;;;;;;;;;

;; Define an ortho neighborhood.
(defun nh-set-fn-ortho (n nx ny)
  (let ((max-distance (ext-max-distance (nh-ext n))))
    (and (<= (abs nx) max-distance)
         (<= (abs ny) max-distance)
         (or (and (zerop nx) (zerop ny))
             (and (zerop nx) (not (zerop ny)))
             (and (not (zerop nx)) (zerop ny))))))

;; Implement a specialized nh-map function for ortho neighborhoods which will
;; reduce how many cells are examined.
(defun nh-map-fn-ortho (n func)
  (let ((results ())
        (num-examined 0)
        (max-distance (ext-max-distance (nh-ext n))))

    ;; compute vertical line results, include the origin
    (loop :for y :from (- max-distance) :to max-distance :do
       (let ((cell (nh-ref n 0 y)))
         (incf num-examined)
         (when cell
           (push (funcall func cell) results))))

    ;; Then compute the left side of the horiz line, skipping the origin
    (loop :for x :from (- max-distance) :below 0 :do
       (let ((cell (nh-ref n x 0)))
         (incf num-examined)
         (when cell
           (push (funcall func cell) results))))

    ;; Then compute the right side of the horz line, skipping the origin
    (loop :for x :from 1 :to max-distance :do
       (let ((cell (nh-ref n x 0)))
         (incf num-examined)
         (when cell
           (push (funcall func cell) results))))

    ;; NOTE: We don't nreverse this like idiomatic usage, because there is
    ;; no defined order of mapping the function across the neighborhood.
    (values results num-examined)))

;; ;;;;;;;;;;;;
;; DIAG neighborhoods
;; ;;;;;;;;;;;;

;; Define a diagonal neighborhood
(defun nh-set-fn-diag (n nx ny)
  (let ((max-distance (ext-max-distance (nh-ext n))))
    (and (<= (abs nx) max-distance)
         (<= (abs ny) max-distance)
         (= (abs nx) (abs ny)))))

;; Implement a specialized map-fn for the diag neighborhood.
(defun nh-map-fn-diag (n func)
  (let ((results ())
        (num-examined 0)
        (max-distance (ext-max-distance (nh-ext n))))
    ;; do left top to right bottom diagonal, including origin
    (loop :for x :from (- max-distance) :to max-distance :do
       (let ((cell (nh-ref n x (- x))))
         (incf num-examined)
         (when cell
           (push (funcall func cell) results))))

    ;; do left bottom to origin, skipping origin
    (loop :for x :from (- max-distance) :below 0 :do
       (let ((cell (nh-ref n x x)))
         (incf num-examined)
         (when cell
           (push (funcall func cell) results))))

    ;; do origin to upper right, skipping origin
    (loop :for x :from 1 :to max-distance :do
       (let ((cell (nh-ref n x x)))
         (incf num-examined)
         (when cell
           (push (funcall func cell) results))))

    ;; NOTE: We don't nreverse this like idiomatic usage, because there is
    ;; no defined order of mapping the function across the neighborhood.
    (values results num-examined)))

;; ;;;;;;;;;;;;
;; CIRCLE neighborhoods
;; ;;;;;;;;;;;;

;; Define a circular neighborhood
(defun nh-set-fn-circle (n nx ny)
  (let ((max-distance (ext-max-distance (nh-ext n))))
    (<= (+ (* nx nx) (* ny ny))
        (* max-distance max-distance))))

;; ;;;;;;;;;;;;
;; SQUARE neighborhoods
;; ;;;;;;;;;;;;

;; Define a square neighborhood
(defun nh-set-fn-square (n nx ny)
  (let ((max-distance (ext-max-distance (nh-ext n))))
    (and (>= nx (- max-distance))
         (>= ny (- max-distance))
         (<= nx max-distance)
         (<= ny max-distance))))

(defun nh-map-fn-square (n func)
  (let ((results ())
        (num-examined 0)
        (max-distance (ext-max-distance (nh-ext n))))
    (loop :for y :from max-distance :downto (- max-distance) :do
       (loop :for x :from (- max-distance) :to max-distance :do
          (let ((cell (nh-ref n x y)))
            (incf num-examined)
            (when cell
              (push (funcall func cell) results)))))

    ;; NOTE: We don't nreverse this like idiomatic usage, because there is
    ;; no defined order of mapping the function across the neighborhood.
    (values results num-examined)))

;; ;;;;;;;;;;;;
;; DEFAULT (square) neighborhoods
;; ;;;;;;;;;;;;

;; Define the default nh set function, which is the square set.
(defun nh-set-fn-default (n nx ny)
  (nh-set-fn-square n nx ny))

;; The default map function will always work no matter the set-fn, but
;; it'll examine all possible cells to do its work so it might not be as
;; efficient as a specific one designed to a neighborhood.
(defun nh-map-fn-default (n func)
  (nh-map-fn-square n func))














;;;; Testing codes.

(defun display-neighborhood (n)
  (let ((max-distance (ext-max-distance (nh-ext n))))
    (format t "  NH Display: max-distance = ~A~%" max-distance)
    (loop :for y :from max-distance :downto (- max-distance) :do
       (format t "    ")
       ;; First draw a strip of the neighborhood
       (loop :for x :from (- max-distance) :to max-distance :do
          (format t "~:[-~;X~]" (nh-ref n x y)))

       ;; then on the same line draw a strip of the actual stage data.
       (format t "    ")
       (loop :for x :from (- max-distance) :to max-distance :do
          (let ((cell (nh-ref n x y)))
            (format t "~A"
                    (cond
                      ((null cell) " ")
                      ((walkablep cell) ".")
                      ((not (walkablep cell)) "#")
                      (t "?")))))

       (format t "~%"))))

(defun neighborhood-test-nh-map (n)
  (format t "  Performing nh-map test...~%")
  (let ((walkablep 0)
        (non-walkablep 0)
        (max-distance (ext-max-distance (nh-ext n))))
    ;; we ignore results, since we side effect in the mapped function.
    ;; We record num-examined, which is how many cells were looked at in
    ;; the neighborhood to determine membership.
    (multiple-value-bind (results num-examined)
        (nh-map n (lambda (cell)
                    (if (walkablep cell)
                        (incf walkablep)
                        (incf non-walkablep))))
      (declare (ignore results))
      (let* ((potential-examined (expt (1+ (* max-distance 2)) 2))
             (examined-savings-per (- 1.0 (/ num-examined potential-examined))))

        (format t "    Found [walkablep = ~A, non-walkablep = ~A, total = ~A] cells.~%"
                walkablep non-walkablep (+ walkablep non-walkablep))

        (format t "    Cells actually examined during nh-map: ~A~%"
                num-examined)
        (format t "    Default nh-map function examination: ~A~%"
                potential-examined)
        (format t "    Savings: ~,2F%~%" (* examined-savings-per 100.0))))))

;; Around the cell given by x y in this call, make a neighborhood of
;; distance and then run all the test. check-distance is for checking cells
;; at that check-distance in the compas directions. Also perform a map-nh
;; test of the nh.
(defun neighbor-tests (x y max-distance check-distance)
  (let ((tests `((,#'nh-set-fn-ortho ,#'nh-map-fn-ortho "Ortho NH Test")
                 (,#'nh-set-fn-diag ,#'nh-map-fn-diag "Diag NH Test")
                 (,#'nh-set-fn-circle ,#'nh-map-fn-default "Circle NH Test")
                 (,#'nh-set-fn-square ,#'nh-map-fn-default "Square NH Test")))
        (dir-meths `((,#'nh-n "N")
                     (,#'nh-nw "NW")
                     (,#'nh-w "W")
                     (,#'nh-sw "SW")
                     (,#'nh-s "S")
                     (,#'nh-se "SE")
                     (,#'nh-e "E")
                     (,#'nh-ne "NE")))
        (stage (make-stage 'labyrinth)))

    (loop :for (nh-func nh-map-fn desc) :in tests :do
       (format t "~A @ [x=~A, y=~A] Max Distance = ~A, Check Distance = ~A~%"
               desc x y max-distance check-distance)
       (let* ((nh (make-nh :stage stage
                           :x x
                           :y y
                           :ext (make-ext :max-distance max-distance)
                           :set-fn nh-func
                           :map-fn nh-map-fn)))

         (display-neighborhood nh)

         (format t "  Origin: ~S~%" (nh-origin nh))

         (loop :for (dir-meth dir-desc) :in dir-meths :do
            (format t "    (~A ~A): ~S~%"
                    dir-desc check-distance
                    (funcall dir-meth nh check-distance)))

         (neighborhood-test-nh-map nh))

       (format t "~%"))

    NIL))
