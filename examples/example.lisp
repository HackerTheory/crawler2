(in-package :crawler2-examples)

(defvar *stage* nil)
(defvar *cell-size* 9)

(defmethod draw ()
  (with-slots (width height) *stage*
    (background (gray 0.3))
    (dotimes (x width)
      (dotimes (y height)
        (let ((cell (cell *stage* x y)))
          (draw-rect cell)
          (draw-circle cell))))))

(defmethod draw-rect (cell)
  (with-slots (x y) cell
    (with-pen (make-pen :fill (select-color :rect cell))
      (rect (* x *cell-size*)
            (* y *cell-size*)
            (1- *cell-size*)
            (1- *cell-size*)))))

(defmethod draw-circle (cell)
  (with-slots (x y) cell
    (with-pen (make-pen :fill (select-color :circle cell))
      (circle (+ (* x *cell-size*) (floor (/ *cell-size* 2)))
              (+ (* y *cell-size*) (floor (/ *cell-size* 2)))
              (/ *cell-size* (select-radius cell))))))

(defmethod select-color ((object (eql :rect)) cell)
  (cond ((featuresp cell :door)
         (rgb 0.1 0.5 1))
        ((featuresp cell :corridor :room :junction :stairs-up :stairs-down)
         (gray 1))
        (t
         (gray 0.2))))

(defmethod select-color ((object (eql :circle)) cell)
  (cond ((featuresp cell :connector)
         (rgb 1 0.1 0.5))
        ((featuresp cell :stairs-up)
         (rgb 1 0 0))
        ((featuresp cell :stairs-down)
         (rgb 0 0 1))))

(defmethod select-radius (cell)
  (cond ((featuresp cell :connector) 6)
        ((featuresp cell :stairs-up :stairs-down) 3)
        (t 1)))

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
