(in-package :crawler2-examples)

(defvar *stage* nil)
(defvar *attrs* nil)
(defvar *cell-size* 8)

(defmethod draw ()
  (with-slots (width height) *stage*
    (background (gray 0.2))
    (dotimes (x width)
      (dotimes (y height)
        (draw-cell x y)))))

(defmethod draw-cell (x y)
  (with-pen (make-pen :fill (select-color x y))
    (rect (* x *cell-size*)
          (* y *cell-size*)
          (1- *cell-size*)
          (1- *cell-size*))))

(defmethod select-color (x y)
  (let ((cell (cell *stage* x y)))
    (cond ((featuresp cell :stairs-up)
           (rgb 1 0.5 0.1))
          ((featuresp cell :stairs-down)
           (rgb 0.1 1 0.5))
          ((featuresp cell :junction)
           (rgb 0.1 0.5 1))
          ((featuresp cell :room :corridor)
           (gray 1)))))

(defmethod mousebutton-event :after (window state ts button x y)
  (when (and (eq state :MOUSEBUTTONUP)
             (= button 1))
    (regenerate window)))

(defmethod close-window :after (window)
  (setf *stage* nil
        *attrs* nil))

(defmethod generate (stage-type attrs)
  (setf *stage* (apply #'make-stage stage-type attrs)))

(defmethod run :around (stage-type &rest attrs)
  (setf *attrs* attrs)
  (generate stage-type *attrs*)
  (call-next-method))
