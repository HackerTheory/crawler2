(in-package :crawler2-examples)

(defvar *stage* nil)
(defvar *cell-size* 9)

(defmethod draw ()
  (with-slots (width height) *stage*
    (background (gray 0.3))
    (dotimes (x width)
      (dotimes (y height)
        (let ((cell (cell *stage* x y)))
          (draw-cell cell)
          (draw-connector cell))))))

(defmethod draw-cell (cell)
  (with-slots (x y) cell
    (with-pen (make-pen :fill (select-color cell))
      (rect (* x *cell-size*)
            (* y *cell-size*)
            (1- *cell-size*)
            (1- *cell-size*)))))

(defmethod draw-connector (cell)
  (with-slots (x y) cell
    (with-pen (make-pen :fill (rgb 1 0 1))
      (when (featuresp (cell *stage* x y) :connector)
        (circle (+ (* x *cell-size*) (floor (/ *cell-size* 2)))
                (+ (* y *cell-size*) (floor (/ *cell-size* 2)))
                (/ *cell-size* 6))))))

(defmethod select-color (cell)
  (cond ((featuresp cell :stairs-up)
         (rgb 1 0.5 0.1))
        ((featuresp cell :stairs-down)
         (rgb 0.1 1 0.5))
        ((featuresp cell :door)
         (rgb 0.1 0.5 1))
        ((featuresp cell :junction)
         (gray 1))
        ((featuresp cell :wall :connector)
         (gray 0.2))
        ((featuresp cell :corridor :room)
         (gray 1))))

(defmethod mousebutton-event :after (window state ts button x y)
  (when (and (eq state :MOUSEBUTTONUP)
             (= button 1))
    (regenerate window)))

(defmethod close-window :after (window)
  (setf *stage* nil))

(defmethod generate (stage-type attrs)
  (setf *stage* (apply #'make-stage stage-type attrs)))

(defmethod run :around (stage-type &rest attrs)
  (generate stage-type attrs)
  (call-next-method))
